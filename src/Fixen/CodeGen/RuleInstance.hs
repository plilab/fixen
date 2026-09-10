{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rule-instance constructors, priority comparisons and evaluation.
module Fixen.CodeGen.RuleInstance where

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
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
  pure $
    Hs.renderDecls
      [Hs.Data (Hs.name "RuleInstance") (Hs.Constructor (Hs.name "Init") [Hs.typ "Fact"] :| constructors) [Hs.name "Show"]]

ruleConstructor :: RuleInfo -> FixenPass CodeGenState (Maybe Hs.Constructor)
ruleConstructor info
  | null (info ^. declaration . assumptions) = pure Nothing
  | otherwise = do
      types <- mapM underlying (Map.elems (info ^. args))
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
  priorities <- IntMap.elems <$> fixenGetPriorities
  cases <- mapM priorityCase priorities
  let initial = Hs.PCon (Hs.name "Init") [Hs.PWildcard]
      comparison op a b result = Hs.Function (Hs.name op) [a, b] result
      methods =
        if null priorities
          then [comparison "<=" Hs.PWildcard initial (Hs.var "True"), comparison "<=" Hs.PWildcard Hs.PWildcard (Hs.var "False")]
          else
            [ comparison "<=" (Hs.pat "i") (Hs.pat "i'") (Hs.call "not" [Hs.call "<" [Hs.var "i'", Hs.var "i"]])
            , comparison "<" initial initial (Hs.var "False")
            , comparison "<" Hs.PWildcard initial (Hs.var "True")
            ]
              ++ cases
              ++ [comparison "<" Hs.PWildcard Hs.PWildcard (Hs.var "False")]
  pure (Hs.renderDecls [Hs.Instance (Hs.TyApp (Hs.typ "Ord") (Hs.typ "RuleInstance")) methods])

priorityCase :: PriorityInfo -> FixenPass CodeGenState Hs.Decl
priorityCase info = do
  ruleInfos <- fixenGetRuleInfo
  let (leftId, rightId) = info ^. rules
      conclusion' = info ^. declaration . conclusion
      patternFor rule instance' =
        let bindings = Map.fromList [(simpleIdentifier k, simpleIdentifier v) | (k, v) <- Map.toList (instance' ^. map)]
         in Hs.PCon
              (Hs.name (codeGenRuleInstanceName (rule ^. declaration)))
              [maybe Hs.PWildcard Hs.pat (Map.lookup parameter bindings) | parameter <- Map.keys (rule ^. args)]
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
      [ Hs.Signature (Hs.name "evaluate") (Hs.functionType [Hs.typ "RuleInstance"] (Hs.typ "Fact"))
      , Hs.Function (Hs.name "evaluate") [Hs.PCon (Hs.name "Init") [Hs.pat "f"]] (Hs.var "f")
      ]
        ++ (evaluateCase <$> active)

evaluateCase :: RuleInfo -> Hs.Decl
evaluateCase info =
  let rule = info ^. declaration
      result = rule ^. conclusion
   in Hs.Function
        (Hs.name "evaluate")
        [Hs.PCon (Hs.name (codeGenRuleInstanceName rule)) (Hs.pat <$> Map.keys (info ^. args))]
        (Hs.call (simpleIdentifier (result ^. name)) (lowerExpr <$> result ^. args))
