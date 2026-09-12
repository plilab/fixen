{-# LANGUAGE OverloadedStrings #-}

-- | Lower the shared rule forest to C++ loops. Bindings own their values;
-- database indexes are never mutated while a matching traversal is active.
module Fixen.CodeGen.Cpp.Match (step, stepWithOptions, seeds) where

import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Fixen.CodeGen.Common (CodeGenOptions (..))
import Fixen.CodeGen.Cpp.Common
import Fixen.CodeGen.Cpp.Database (conclusion, discreteFields, scanRows)
import Fixen.CodeGen.Cpp.Syntax
import Fixen.CodeGen.RuleInstance (codeGenRuleInstanceName)
import Fixen.IR.AST hiding (Expr)
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad (RuleInfo (..), fixenGetRuleInfo)
import Prettyprinter

step :: NE.NonEmpty RuleForest -> RelationRepresentation -> Gen (Doc ())
step = stepWithOptions (CodeGenOptions False False)

stepWithOptions :: CodeGenOptions -> NE.NonEmpty RuleForest -> RelationRepresentation -> Gen (Doc ())
stepWithOptions options forests layouts = do
  phases <- sequence [phaseBody p forest | (p, forest) <- zip [0 :: Int ..] (NE.toList forests)]
  pure (function "inline void" "fx_step" ["const Database& db", "const Fact& fact", "std::size_t phase", "fx_Queue& queue"] phases)
  where
    phaseBody p forest = do
      branches <-
        sequence
          [ do
              body <- matchTree True p IM.empty 0 rel tree
              pure (Scope [Declare "const auto*" "fx_head" (call (template "std::get_if" [rel]) [Unary "&" (Name "fact")]), If (Name "fx_head") body])
          | (rel, trees) <- Map.toList (_ruleForestTrees forest)
          , tree <- NE.toList trees
          ]
      pure (If (Binary "==" (Name "phase") (Name (T.show p))) branches)
    matchTree incoming p bindings depth rel tree = do
      let args = _ruleTreeChoppedHeadArgs tree
          layout = layouts Map.! rel
          fields = _factTypes (_factRepresentation layout)
          order = if incoming then [0 .. length args - 1] else IM.elems (_extractionMap (_databaseRepresentation layout))
          scanPrefix = "fx_scan" <> T.show depth
          -- Repeated variables can become lookup keys during this same
          -- traversal: an unbound outer key is known at the next index level.
          bindKey (known, keys) (level, (i, _)) = case IM.lookup (args !! i) known of
            Just value -> (known, Map.insert i value keys)
            Nothing ->
              let key = Member (Name (scanPrefix <> "_index" <> T.show level)) "first"
               in (IM.insert (args !! i) key known, keys)
          (_, bound) = foldl bindKey (bindings, Map.empty) (zip [0 :: Int ..] (discreteFields layout))
          match source = matchFields incoming bindings depth [(args !! i, fst (fields !! i), source Map.! i) | i <- order] $ \next count -> forestBody p next count (_ruleTreeChoppedHeadBranches tree)
      if incoming
        then match (Map.fromList [(i, field (Name "(*fx_head)") i) | i <- order])
        else scanRows scanPrefix layout (Member (Name "db") (factsField rel)) bound match
    forestBody p bindings depth forest = do
      branches <- concat <$> sequence [matchTree False p bindings depth rel tree | (rel, trees) <- Map.toList (_ruleForestTrees forest), tree <- NE.toList trees]
      leaves <- mapM (leaf p bindings) (_ruleForestLeaves forest)
      pure (branches ++ leaves)
    leaf p bindings l = do
      let vars = sortOn fst [(n, bindings IM.! i) | (n, i) <- zip (_ruleLeafVariableMap l) [0 ..], n /= "_"]
          names = Map.fromList vars
      guards <- mapM (lowerExpr names Nothing . conditionExpr) (_ruleLeafCondition l)
      let instanceValue = Construct (ruleType (_ruleLeafRuleId l)) (snd <$> vars)
      body <-
        if codeGenDebug options
          then do
            rules <- fixenGetRuleInfo
            let label = codeGenRuleInstanceName (_ruleDeclaration (rules IM.! _ruleLeafRuleId l))
            pure
              [ Declare "fx_Candidate" "fx_candidate" (Construct "fx_Candidate" [instanceValue, Name (T.show p)])
              , Statement (call "fx_debugActivation" [Name "fact", Name "phase", Name (stringLiteral label), Name "fx_candidate"])
              , Statement (call "queue.push" [call "std::move" [Name "fx_candidate"]])
              ]
          else pure [enqueue p instanceValue]
      pure (If (andExpr guards) body)

matchFields :: Bool -> IM.IntMap Expr -> Int -> [(Int, QueryType, Expr)] -> (IM.IntMap Expr -> Int -> Gen [Stmt]) -> Gen [Stmt]
matchFields _ bindings depth [] continuation = continuation bindings depth
matchFields incoming bindings depth ((i, q, value) : rest) continuation = do
  let local = "fx_v" <> T.show depth
      next = IM.insert i (Name local) bindings
      recurse = matchFields incoming next (depth + 1) rest continuation
  case IM.lookup i bindings of
    Nothing -> (Declare "const auto" local value :) <$> recurse
    Just previous -> case q of
      Match -> do
        body <- matchFields incoming bindings depth rest continuation
        pure [If (Binary "==" previous value) body]
      Meet _ mlbs -> do
        f <- operation mlbs
        body <- recurse
        pure [For ("const auto& " <> local) (call f (if incoming then [previous, value] else [value, previous])) body]
      LatticeMeet _ _ meet -> do
        f <- operation meet
        (Declare "const auto" local (call f (if incoming then [previous, value] else [value, previous])) :) <$> recurse

enqueue :: Int -> Expr -> Stmt
enqueue p value = Statement (call "queue.push" [Construct "fx_Candidate" [value, Name (T.show p)]])

seeds :: NE.NonEmpty RuleForest -> RelationRepresentation -> Gen [Stmt]
seeds forests layouts =
  sequence
    [ do
        guards <- mapM (lowerExpr Map.empty Nothing . conditionExpr) (_ruleLeafCondition leaf)
        facts <- mapM (conclusion layouts Map.empty) (_ruleLeafConclusion leaf)
        let value = case NE.toList facts of
              [fact] -> Construct "fx_Init" [fact]
              fs -> Construct "fx_Seed" [Construct "std::vector<Fact>" fs]
        pure (If (andExpr guards) [enqueue p value])
    | (p, forest) <- zip [0 ..] (NE.toList forests)
    , leaf <- _ruleForestLeaves forest
    ]
