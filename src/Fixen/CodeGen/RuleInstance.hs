{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.SymbolSolver.RuleInstance
-- Description : Code generation for rule instances
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities for stuff involving rule
-- instances.
--
-- The 'codeGenRuleInstance' entry point performs code generation for:
--
-- 1. The @RuleInstance@ type ('codeGenRuleInstanceDef')
-- 2. The @evaluate@ function ('codeGenEvaluate')
-- 3. The @Eq RuleInstance@ instance ('codeGenEqInstance')
-- 4. The @Ord RuleInstance@ instance and priorities ('codeGenPriorities')
-- 5. The @Queue@ type ('codeGenQueueDef')
--
-- @since 26.7
module Fixen.CodeGen.RuleInstance where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Utils
import Prelude hiding (map)

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Generates the Haskell source code for dealing with rule instances.
--
-- @since 26.7
codeGenRuleInstance :: FixenPass CodeGenState Text
codeGenRuleInstance = do
  let eq_instance = codeGenEqInstance
  rule_instance_def <- codeGenRuleInstanceDef
  eval_def <- codeGenEvaluate
  priority_def <- codeGenPriorities
  q_def <- codeGenQueueDef
  return $
    Text.intercalate
      "\n\n"
      [ rule_instance_def
      , eval_def
      , eq_instance
      , priority_def
      , q_def
      ]

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- ** Rule Instance Definition

-- | Generates the @RuleInstance@ datatype.
--
-- @since 26.7
codeGenRuleInstanceDef :: FixenPass CodeGenState Text
codeGenRuleInstanceDef = do
  rule_map <- fixenGetRuleInfo
  let the_rules = values rule_map
  let header =
        """
        ----- RULE INSTANCES -----
        data RuleInstance
               = 
        """
  rule_constructors <- catMaybes <$> mapM codeGenRuleInstanceConstructor the_rules
  return $
    Text.concat
      [ header
      , Text.intercalate "\n       | " ("Init Fact" : rule_constructors)
      , "\n  deriving Show"
      ]

-- | Generates a constructor for the @RuleInstance@ type.
--
-- @since 26.7
codeGenRuleInstanceConstructor
  :: RuleInfo
  -> FixenPass CodeGenState (Maybe Text)
codeGenRuleInstanceConstructor r = do
  let rule_declaration = r ^. declaration
  if null (rule_declaration ^. assumptions)
    then -- rules with no assumptions will never be rule instances in the queue
      return Nothing
    else do
      let rule_instance_name = codeGenRuleInstanceName rule_declaration
          rule_params = r ^. args & values <&> (^. ty)
      param_types <- mapM fromTypeLattice rule_params
      let arg_ty_code = codeGenTypeAsAtomic <$> param_types
      return $ Just $ Text.intercalate " " (rule_instance_name : arg_ty_code)
  where
    fromTypeLattice (ActualType t _) = getUnderlyingType t
    fromTypeLattice Dynamic =
      failErr
        (Just "panic")
        "something went wrong; dynamic type found in codegen!"
        []
        []
    fromTypeLattice Bottom =
      failErr
        (Just "panic")
        "something went wrong; bottom type found in codegen!"
        []
        []

-- | Generates the name (constructor) of a rule instance.
--
-- @since 26.7
codeGenRuleInstanceName :: Rule -> Text
codeGenRuleInstanceName the_rule
  | Nothing <- the_rule ^. name =
      Text.append "UnnamedRule" (Text.show (the_rule ^. nodeId))
  | Just v <- the_rule ^. name =
      Text.append "Rule" $ capitalize $ simpleIdentifier v

-- ** @RuleInstance@ Class Instances

-- *** @Eq@

-- | Generates the @Eq RuleInstance@ instance.
--
-- @since 26.7
codeGenEqInstance :: Text
codeGenEqInstance =
  """
  instance Eq RuleInstance where
    f == f' = not (f < f' || f' < f)
  """

-- *** @Ord@

-- | Generates the @Ord RuleInstance@ instance, which uses the Fixen program's
-- @priority@ declarations.
--
-- @since 26.7
codeGenPriorities :: FixenPass CodeGenState Text
codeGenPriorities = do
  priority_info <- values <$> fixenGetPriorities
  if null priority_info
    then
      return
        """
        instance Ord RuleInstance where
          _ <= Init _ = True
          _ <= _ = False
        """
    else do
      let header =
            """
            instance Ord RuleInstance where
              i <= i' = not (i' < i)
              ----- PRIORITIES -----
              Init _ < Init _ = False
              _ < Init _ = True
            """
      cases <- mapM codeGenPriority priority_info
      return $
        Text.concat
          [ header
          , Text.concat $ Text.append "\n" <$> cases
          , "\n  _ < _ = False"
          ]

-- | Generates a @<@ case using a @priority@ declaration.
--
-- @since 26.7
codeGenPriority :: PriorityInfo -> FixenPass CodeGenState Text
codeGenPriority p_info = do
  rule_info_map <- fixenGetRuleInfo
  let prem = p_info ^. declaration . premise
      conc = p_info ^. declaration . conclusion
      (lhs_rule_id, rhs_rule_id) = p_info ^. rules
      (lhs_rule_info, rhs_rule_info) =
        ( rule_info_map IntMap.! lhs_rule_id
        , rule_info_map IntMap.! rhs_rule_id
        )
      (lhs_rule_instance, rhs_rule_instance) = (conc ^. lhs, conc ^. rhs)
      (lhs_rule_instance_name, lhs_code_vars) = mk lhs_rule_info lhs_rule_instance
      (rhs_rule_instance_name, rhs_code_vars) = mk rhs_rule_info rhs_rule_instance
  return $
    Text.concat
      [ "  ("
      , lhs_rule_instance_name
      , " "
      , Text.intercalate " " lhs_code_vars
      , ") < ("
      , rhs_rule_instance_name
      , " "
      , Text.intercalate " " rhs_code_vars
      , ") = "
      , codeGenExpr prem
      ]
  where
    mk rule_info rule_instance =
      let priority_vars =
            rule_instance
              ^. map
              & Map.mapKeys simpleIdentifier
              & Map.map simpleIdentifier
          rule_instance_name = codeGenRuleInstanceName (rule_info ^. declaration)
          rule_params = rule_info ^. args & Map.keys
          params_code =
            ( \v -> case priority_vars Map.!? v of
                Nothing -> "_"
                Just v' -> v'
            )
              <$> rule_params
       in (rule_instance_name, params_code)

-- ** Work Queues

-- | Generates the @Queue@ type.
--
-- @since 26.7
codeGenQueueDef :: FixenPass CodeGenState Text
codeGenQueueDef = do
  let header = "type Queue = Q.MaxQueue "
  phase_info <- fixenGetPhases
  return $
    Text.append header $
      if length phase_info == 1
        then "RuleInstance"
        else "(RuleInstance, Phase)"

-- ** Evaluate

-- | Generates the @evaluate@ function.
--
-- @since 26.7
codeGenEvaluate :: FixenPass CodeGenState Text
codeGenEvaluate = do
  rule_info_map <- fixenGetRuleInfo
  let header = "evaluate :: RuleInstance -> Fact"
      actual_rule_instances = filter (\r -> (¬) (null (r ^. declaration . assumptions))) $ values rule_info_map
      evaluate_cases = codeGenEvaluateCase <$> actual_rule_instances
  return $ Text.intercalate "\n" $ header : "evaluate (Init f) = f" : evaluate_cases

-- | Generates a case for the @evaluate@ function.
--
-- @since 26.7
codeGenEvaluateCase :: RuleInfo -> Text
codeGenEvaluateCase rule_info =
  let rule_declaration = rule_info ^. declaration
      rule_instance_name = codeGenRuleInstanceName rule_declaration
      rule_params = rule_info ^. args & Map.keys
      rule_conclusion = rule_declaration ^. conclusion . name
      rule_conclusion_args = rule_declaration ^. conclusion . args <&> asAtomic
   in Text.intercalate " " $
        [ "evaluate"
        , parenthesize $ Text.intercalate " " $ rule_instance_name : rule_params
        , "="
        , simpleIdentifier rule_conclusion
        ]
          ++ rule_conclusion_args
