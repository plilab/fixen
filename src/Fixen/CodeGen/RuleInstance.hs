{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rule-instance constructors, priority comparisons and evaluation.
module Fixen.CodeGen.RuleInstance where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Pattern (storedRulePatterns)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.Fields (args, assumptions, conclusion, declaration, lhs, map, name, nodeId, premise, rhs, rules, ty, (^.))
import Fixen.IR.AST
import Fixen.Monad
import Prelude hiding (map)

codeGenRuleInstance :: FixenPass CodeGenState Text
codeGenRuleInstance = do
  constructors <- codeGenRuleInstanceDef
  evaluation <- codeGenEvaluate
  priorities <- codeGenPriorities
  queue <- codeGenQueueDef
  pure (Text.intercalate "\n\n" [constructors, evaluation, codeGenEqInstance, priorities, queue])

codeGenRuleInstanceDef :: FixenPass CodeGenState Text
codeGenRuleInstanceDef = do
  ruleInfos <- fixenGetRuleInfo
  constructors <- catMaybes <$> mapM ruleConstructor (IntMap.elems ruleInfos)
  let seeds = [Hs.Constructor (Hs.name "InitMany") [Hs.TyList (Hs.typ "Fact")] | hasMultiConclusionSeeds ruleInfos]
  pure $
    Hs.renderDecls
      [Hs.Data (Hs.name "RuleInstance") (Hs.Constructor (Hs.name "Init") [Hs.typ "Fact"] :| (seeds ++ constructors)) [Hs.name "Show"]]

ruleConstructor :: RuleInfo -> FixenPass CodeGenState (Maybe Hs.Constructor)
ruleConstructor info
  | null (info ^. declaration . assumptions) = pure Nothing
  | otherwise = do
      types <- mapM underlying (Map.elems (Map.restrictKeys (info ^. args) (Map.keysSet (ruleStorageVariables (info ^. declaration)))))
      pure (Just (Hs.Constructor (Hs.name (codeGenRuleInstanceName (info ^. declaration))) types))
  where
    underlying argument = case argument ^. ty of
      ActualType t _ -> lowerType <$> getUnderlyingType t
      _ -> failErr (Just "panic") "unresolved rule argument type in Haskell code generation" [] []

codeGenRuleInstanceName :: Rule -> Text
codeGenRuleInstanceName rule = case ruleName rule of
  Nothing -> "UnnamedRule" <> Text.show (rule ^. nodeId)
  Just n -> "Rule" <> capitalize (simpleIdentifier n)

codeGenEqInstance :: Text
codeGenEqInstance =
  """
  instance Eq RuleInstance where
    f == f' = not (f < f' || f' < f)
  """

-- | Preserve the existing priority contract, including Init taking precedence.
-- This is a Haskell backend decision, not a proposed cross-backend ordering.
codeGenPriorities :: FixenPass CodeGenState Text
codeGenPriorities = do
  ruleInfos <- fixenGetRuleInfo
  priorities <- IntMap.elems <$> fixenGetPriorities
  cases <- mapM priorityCase priorities
  let initials = [Hs.PCon (Hs.name n) [Hs.PWildcard] | n <- "Init" : ["InitMany" | hasMultiConclusionSeeds ruleInfos]]
      comparison op a b result = Hs.Function (Hs.name op) [a, b] result
      methods =
        if null priorities
          then [comparison "<=" Hs.PWildcard initial (Hs.var "True") | initial <- initials] ++ [comparison "<=" Hs.PWildcard Hs.PWildcard (Hs.var "False")]
          else
            [ comparison "<=" (Hs.pat "i") (Hs.pat "i'") (Hs.call "not" [Hs.call "<" [Hs.var "i'", Hs.var "i"]])
            ]
              ++ [comparison "<" a b (Hs.var "False") | a <- initials, b <- initials]
              ++ [comparison "<" Hs.PWildcard initial (Hs.var "True") | initial <- initials]
              ++ cases
              ++ [comparison "<" Hs.PWildcard Hs.PWildcard (Hs.var "False")]
  pure (Hs.renderDecls [Hs.Instance (Hs.TyApp (Hs.typ "Ord") (Hs.typ "RuleInstance")) methods])

priorityCase :: PriorityInfo -> FixenPass CodeGenState Hs.Decl
priorityCase info = do
  ruleInfos <- fixenGetRuleInfo
  let (leftId, rightId) = info ^. rules
      conclusion' = info ^. declaration . conclusion
      patternFor rule instance' =
        let bindings = Map.fromList [(simpleIdentifier k, Hs.name (simpleIdentifier v)) | (k, v) <- Map.toList (instance' ^. map)]
         in Hs.PCon
              (Hs.name (codeGenRuleInstanceName (rule ^. declaration)))
              (storedRulePatterns (rule ^. declaration) bindings)
  pure
    ( Hs.Function
        (Hs.name "<")
        [patternFor (ruleInfos IntMap.! leftId) (conclusion' ^. lhs), patternFor (ruleInfos IntMap.! rightId) (conclusion' ^. rhs)]
        (lowerExpr (info ^. declaration . premise))
    )

codeGenQueueDef :: FixenPass CodeGenState Text
codeGenQueueDef = do
  phases <- fixenGetPhases
  let element = if length phases == 1 then Hs.typ "RuleInstance" else Hs.TyTuple [Hs.typ "RuleInstance", Hs.typ "Phase"]
  pure (Hs.renderDecls [Hs.TypeAlias (Hs.name "Queue") (Hs.TyApp (Hs.typ "Q.MaxQueue") element)])

codeGenEvaluate :: FixenPass CodeGenState Text
codeGenEvaluate = do
  ruleInfos <- fixenGetRuleInfo
  let active = filter (not . null . (^. declaration . assumptions)) (IntMap.elems ruleInfos)
  pure $
    Hs.renderDecls $
      [ Hs.Signature (Hs.name "evaluate") (Hs.functionType [Hs.typ "RuleInstance"] (Hs.TyList (Hs.typ "Fact")))
      , Hs.Function (Hs.name "evaluate") [Hs.PCon (Hs.name "Init") [Hs.pat "f"]] (Hs.List [Hs.var "f"])
      ]
        ++ [Hs.Function (Hs.name "evaluate") [Hs.PCon (Hs.name "InitMany") [Hs.pat "fs"]] (Hs.var "fs") | hasMultiConclusionSeeds ruleInfos]
        ++ (evaluateCase <$> active)

evaluateCase :: RuleInfo -> Hs.Decl
evaluateCase info =
  let rule = info ^. declaration
      results = NonEmpty.toList (rule ^. conclusion)
   in Hs.Function
        (Hs.name "evaluate")
        [Hs.PCon (Hs.name (codeGenRuleInstanceName rule)) (storedRulePatterns rule (Map.fromList [(n, Hs.name n) | n <- Map.keys (info ^. args)]))]
        (Hs.List [Hs.call (simpleIdentifier (result ^. name)) (lowerExpr <$> result ^. args) | result <- results])
