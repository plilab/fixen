{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | C++17 backend. Parsing, symbol solving and forest construction are shared
-- with Haskell; storage and host-language lowering are backend decisions.
module Fixen.CodeGen.Cpp (codeGen) where

import Control.Monad (forM_, when)
import Data.IntMap.Strict qualified as IM
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Error.Diagnose
import Fixen.CodeGen.Common (CodeGenOptions (..), CodeGenState, hasMultiConclusionSeeds)
import Fixen.CodeGen.Cpp.Common
import Fixen.CodeGen.Cpp.Database
import Fixen.CodeGen.Cpp.Debug qualified as Debug
import Fixen.CodeGen.Cpp.Match
import Fixen.CodeGen.Cpp.Syntax
import Fixen.IR.AST hiding (Expr)
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad
import Prettyprinter

codeGen :: CodeGenOptions -> NE.NonEmpty RuleForest -> RelationRepresentation -> Program -> FixenPass CodeGenState Text
codeGen options forests layouts program = do
  namespace <- hostNameFromModule
  let reserved = ["Fact", "Database", "Interpretation", "Phase", "solve", "reSolve", "factLeq", "entails", "insertToDb", "mergeContour", "maximalContour"] ++ ["Phase" <> T.show p | p <- [0 .. NE.length forests - 1]]
      checkExport declaration n = do
        when (n `elem` reserved || n == last (T.splitOn "::" namespace)) $ do
          pos <- getPosition declaration
          failErr (Just "C++ backend") ("Name conflicts with generated C++ API: " ++ T.unpack n) [(pos, This "choose another name")] []
  forM_ (programRelationDeclarations program) $ \r -> checkExport r (simpleIdentifier (relationLikeName r))
  forM_ (programQueries program) $ \q -> checkExport q (simpleIdentifier (queryName q))
  db <- database layouts
  instances <- ruleInstances options layouts
  matching <- stepWithOptions options forests layouts
  initial <- seeds forests layouts
  queries <- mapM (query (NE.length forests) layouts) (programQueries program)
  let blocks footer = [pretty (cppBlockContents b) | b <- programCppBlocks program, cppBlockFooter b == footer]
      generated = db ++ instances ++ Debug.definitions options (NE.length forests) layouts ++ [matching] ++ solver options (NE.length forests) initial ++ queries
      debugHeaders = ["#include <sstream>" | codeGenDebug options]
  let scoped = if T.null namespace then generated else [block ("namespace" <+> pretty namespace) generated]
  pure (render ([pretty headers] ++ debugHeaders ++ blocks False ++ scoped ++ blocks True) <> "\n")
  where
    hostNameFromModule = case programCppNamespace program of
      Nothing -> pure ""
      Just "" -> pure ""
      Just name -> T.intercalate "::" <$> mapM identifier (T.splitOn "::" name)

ruleInstances :: CodeGenOptions -> RelationRepresentation -> Gen [Doc ()]
ruleInstances options layouts = do
  allRules <- fixenGetRuleInfo
  let rules = filter (not . null . ruleAssumptions . _ruleDeclaration . snd) (IM.toList allRules)
      batchSeeds = hasMultiConclusionSeeds allRules
      initial side = "std::holds_alternative<fx_Init>(" <> side <> ")" <> if batchSeeds then " || std::holds_alternative<fx_Seed>(" <> side <> ")" else mempty
  declarations <- mapM record rules
  evaluations <- mapM evaluate rules
  priorities <- fixenGetPriorities >>= mapM (priorityCase allRules) . IM.elems
  pure $
    ["struct fx_Init { Fact fact; };"]
      ++ ["struct fx_Seed { std::vector<Fact> facts; };" | batchSeeds]
      ++ declarations
      ++ [ "using fx_Instance =" <+> pretty (template "std::variant" (["fx_Init"] ++ ["fx_Seed" | batchSeeds] ++ (ruleType . fst <$> rules))) <> semi
         , block "struct fx_Candidate" (["fx_Instance instance;", "std::size_t phase;"] ++ ["std::optional<std::vector<Fact>> preview = std::nullopt;" | codeGenDebug options]) <> semi
         , block
             "template<class Emit> inline void fx_evaluate(const fx_Instance& instance, Emit&& emit)"
             ( ["if (const auto* initial = std::get_if<fx_Init>(&instance)) { emit(initial->fact); return; }"]
                 ++ ["if (const auto* seed = std::get_if<fx_Seed>(&instance)) { for (const auto& fact : seed->facts) emit(fact); return; }" | batchSeeds]
                 ++ evaluations
                 ++ ["throw std::logic_error(\"Invalid Fixen rule instance\");"]
             )
         , block
             "inline bool fx_lower(const fx_Instance& left, const fx_Instance& right)"
             ( [ "if (" <> initial "left" <> ") return false;"
               , "if (" <> initial "right" <> ") return true;"
               ]
                 ++ priorities
                 ++ ["return false;"]
             )
         , "struct fx_Lower { bool operator()(const fx_Candidate& a, const fx_Candidate& b) const { return fx_lower(a.instance, b.instance); } };"
         , "using fx_Queue = std::priority_queue<fx_Candidate, std::vector<fx_Candidate>, fx_Lower>;"
         ]
  where
    record (i, info) = do
      types <- mapM underlying (Map.elems (_ruleBoundVars info))
      pure (block ("struct" <+> pretty (ruleType i)) [pretty t <+> pretty ("arg" <> T.show j) <> semi | (j, t) <- zip [0 :: Int ..] types] <> semi)
    underlying argument = case _ruleParamType argument of
      ActualType t _ -> getUnderlyingType t >>= lowerType
      _ -> unsupported "Unresolved rule argument type in C++ generation."
    evaluate (i, info) = do
      let names = Map.fromList [(n, field (Name "(*value)") j) | (j, n) <- zip [0 ..] (Map.keys (_ruleBoundVars info))]
      results <- mapM (conclusion layouts names) (ruleConclusion (_ruleDeclaration info))
      pure
        ( block
            ("if ([[maybe_unused]] const auto* value = std::get_if<" <> pretty (ruleType i) <> ">(&instance))")
            ([stmt (Statement (call "emit" [result])) | result <- NE.toList results] ++ ["return;"])
        )

priorityCase :: IM.IntMap RuleInfo -> PriorityInfo -> Gen (Doc ())
priorityCase rules info = do
  let (leftId, rightId) = _priorityRules info
      c = priorityConclusion (_priorityDeclaration info)
      bindings i side inst =
        let positions = Map.fromList (zip (Map.keys (_ruleBoundVars (rules IM.! i))) [0 ..])
         in [(simpleIdentifier value, field (Name ("(*" <> side <> ")")) (positions Map.! simpleIdentifier parameter)) | (parameter, value) <- Map.toList (ruleInstanceMap inst)]
      names = Map.fromList (bindings leftId "a" (priorityConclusionLHS c) ++ bindings rightId "b" (priorityConclusionRHS c))
  priorityCondition <- lowerExpr names Nothing (priorityPremise (_priorityDeclaration info))
  pure $
    block
      mempty
      [ "const auto* a = std::get_if<" <> pretty (ruleType leftId) <> ">(&left);"
      , "const auto* b = std::get_if<" <> pretty (ruleType rightId) <> ">(&right);"
      , stmt (If (Binary "&&" (Name "a") (Name "b")) [Return priorityCondition])
      ]

query :: Int -> RelationRepresentation -> Query -> Gen (Doc ())
query count layouts q = do
  n <- identifier (simpleIdentifier (queryName q))
  let rel = simpleIdentifier (relationLikeName (queryRel q))
      fields = _factTypes (_factRepresentation (layouts Map.! rel))
      inputs = [(i, kind, ty) | (i, ((kind, ty), Input _)) <- zip [0 ..] (zip fields (relationLikeArgs (queryRel q)))]
  params <- sequence [do t <- lowerType ty; pure ("const " <> t <> "& fx_input" <> T.show i) | (i, _, ty) <- inputs]
  let layout = layouts Map.! rel
      bound = Map.fromList [(i, Name ("fx_input" <> T.show i)) | (i, Match, _) <- inputs]
  rows <- scanRows "fx_query" layout (Member (Name "db") (factsField rel)) bound $ \value -> do
    conditions <- sequence [comparison kind (Name ("fx_input" <> T.show i)) (value Map.! i) | (i, kind, _) <- inputs]
    pure [If (andExpr conditions) [Statement (call "result.push_back" [Construct rel (Map.elems value)])]]
  let extras = if count == 1 then ["const Database& db"] else ["const Interpretation& interpretation", "Phase phase"]
      select = [Declare "const auto&" "db" (call "interpretation.at" [call "static_cast<std::size_t>" [Name "phase"]]) | count /= 1]
  pure $
    function ("inline " <> template "std::vector" [rel]) n (params ++ extras) $
      select
        ++ [ Declare (template "std::vector" [rel]) "result" (Construct (template "std::vector" [rel]) [])
           ]
        ++ rows
        ++ [Return (Name "result")]

solver :: CodeGenOptions -> Int -> [Stmt] -> [Doc ()]
solver options count initial =
  phaseDeclarations
    ++ [ block
           ("inline" <+> pretty stateType <+> "reSolve(" <> pretty stateType <+> "state, const std::vector<Fact>& facts)")
           ( [ "fx_Queue queue;"
             , "for (const auto& fact : facts) queue.push({fx_Init{fact}," <+> pretty (count - 1) <> "});"
             ]
               ++ (stmt <$> initial)
               ++ [ block
                      "while (!queue.empty())"
                      [ "const auto candidate = queue.top();"
                      , "queue.pop();"
                      , "const auto phase = (candidate.phase + 1) %" <+> pretty count <> semi
                      , if count == 1 then "auto& db = state;" else "auto& db = state.at(phase);"
                      , block
                          "const auto process = [&](const Fact& fact)"
                          [ block "if (entails(db, fact))" (["fx_debugRejected(fact, candidate.phase, phase);" | codeGenDebug options] ++ ["return;"])
                          , "auto contour = maximalContour(mergeContour(fact, db));"
                          , "contour.erase(std::remove_if(contour.begin(), contour.end(), [&](const auto& f) { return entails(db, f); }), contour.end());"
                          , "for (const auto& f : contour) insertToDb(db, f);"
                          , if codeGenDebug options then "fx_debugAccepted(fact, contour, candidate.phase, phase);" else mempty
                          , "for (const auto& f : contour) fx_step(db, f, phase, queue);"
                          ]
                          <> semi
                      , if codeGenDebug options
                          then "if (candidate.preview) { for (const auto& fact : *candidate.preview) process(fact); } else { fx_evaluate(candidate.instance, process); }"
                          else "fx_evaluate(candidate.instance, process);"
                      ]
                  , "return state;"
                  ]
           )
       , "inline" <+> pretty stateType <+> "solve(const std::vector<Fact>& facts) { return reSolve(" <> pretty stateType <> "{}, facts); }"
       ]
  where
    -- Single-phase entry points and queries use Database directly, so the
    -- public phase types and constants are only needed for multiple phases.
    phaseDeclarations
      | count == 1 = []
      | otherwise =
          [ "using Interpretation = std::array<Database," <+> pretty count <> ">;"
          , "enum class Phase : std::size_t" <+> braces (commaSep [pretty ("Phase" <> T.show i) | i <- [0 .. count - 1]]) <> semi
          ]
            ++ ["inline constexpr Phase" <+> pretty ("Phase" <> T.show i) <+> "= Phase::" <> pretty ("Phase" <> T.show i) <> semi | i <- [0 .. count - 1]]
    stateType :: Text
    stateType = if count == 1 then "Database" else "Interpretation"

headers :: Text
headers =
  """
  // Generated by Fixen. Requires C++17 and the standard library.
  #include <algorithm>
  #include <array>
  #include <cmath>
  #include <cstddef>
  #include <cstdint>
  #include <cstdlib>
  #include <functional>
  #include <iostream>
  #include <optional>
  #include <queue>
  #include <stdexcept>
  #include <string>
  #include <tuple>
  #include <type_traits>
  #include <utility>
  #include <unordered_map>
  #include <unordered_set>
  #include <variant>
  #include <vector>
  """
