{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.SymbolSolver.Priorities
-- Description : Symbol solving for priorities
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides facilities for solving priority declarations.
--
-- @since 0.0.1
module Fixen.SymbolSolver.Priorities where

import Control.Lens
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern
import Fixen.SymbolSolver.Validation
import Fixen.Utils
import Prelude hiding (map)

--------------------------------------------------------------------------------

-- * Main Entry Point

-- $mainEntryPoint
--
-- You should only need 'initEnvWithPriorities'.

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with a 'Priority' declaration. Insertion fails
-- whenever the rule instances are invalid or if there are duplicate local
-- variables.
--
-- /Precondition/: Rules must have been initialized in the 'SymbolEnv'.
--
-- @since 0.0.1
initEnvWithPriorities
  :: SymbolEnv
  -- ^ The 'SymbolEnv'
  -> Priority
  -- ^ The 'Priority' declaration
  -> FixenPass SymbolState SymbolEnv
initEnvWithPriorities env p = do
  let prem = p ^. premise
      concl = p ^. conclusion
      l = concl ^. lhs
      r = concl ^. rhs

  -- Validate the names and the rule parameters of these instances
  lhs_invalid <- validateRuleInstance l env
  rhs_invalid <- validateRuleInstance r env
  if lhs_invalid ∨ rhs_invalid
    then return env
    else do
      -- now ensure that there are no duplicate local variables across the entire
      -- priority declaration
      let lhs_map = l ^. map
          rhs_map = r ^. map
      lhs_rule <- (^. nodeId) <$> fromJust <$> getRuleFromRuleInstance l env
      rhs_rule <- (^. nodeId) <$> fromJust <$> getRuleFromRuleInstance r env
      let lhs_local_vars = values lhs_map
          rhs_local_vars = values rhs_map
          lhs_local_var_map = Map.fromList $ (\i -> (simpleIdentifier i, (lhs_rule, i))) <$> lhs_local_vars
          rhs_local_var_map = Map.fromList $ (\i -> (simpleIdentifier i, (rhs_rule, i))) <$> rhs_local_vars
          all_local_vars = lhs_local_vars ++ rhs_local_vars
          duplicate_local_vars_map =
            all_local_vars
              <&> (\i -> Map.singleton (simpleIdentifier i) [i])
              & Map.unionsWith (++)
              & Map.filter (\ls -> length ls > 1)
      if (¬) (null duplicate_local_vars_map)
        then do
          let all_vars = foldl' (++) [] duplicate_local_vars_map
          pos <- mapM getPosition all_vars
          accumErr
            Nothing
            "duplicate local variables"
            ((,This "variable") <$> pos)
            [Note "priority local variables must be unique"]
          return env
        else do
          -- take out unused variables
          let used_ids = Set.toList $ getAllExprNames prem
              used_names = simpleIdentifier <$> used_ids
              unused = filter (\i -> simpleIdentifier i ∉ used_names) all_local_vars
          -- time to warn unused
          when (not (null unused)) $ do
            pos <- mapM getPosition unused
            accumWarn
              Nothing
              "unused local variable(s)"
              ((,This "variable") <$> pos)
              [Hint "unused priority local variables do not need to be instantiated and can be omitted"]
          let used_local_vars = filter (\i -> simpleIdentifier i ∈ used_names) all_local_vars
              used_local_vars_names = Set.fromList $ simpleIdentifier <$> used_local_vars
              externs = filter (\i -> simpleIdentifier i ∉ used_local_vars_names) used_ids
              used_local_vars_lhs = Map.filterKeys (∈ used_names) lhs_local_var_map
              used_local_vars_rhs = Map.filterKeys (∈ used_names) rhs_local_var_map
          forM_ (used_local_vars) $ \i ->
            validate
              [warnNameShadowingAgainstExtern "local variable" (simpleIdentifier i)]
              i
              env
          let priority_info =
                PriorityInfo
                  { _priorityDeclaration = p
                  , _priorityLocalVars = used_local_vars_lhs ∪ used_local_vars_rhs
                  , _priorityRules = (lhs_rule, rhs_rule)
                  }

          let priority_node_id = p ^. nodeId
          -- time to insert the priority info
          env
            & priorityInfos . at priority_node_id ?~ priority_info
            & foldMWith initEnvWithExternSymbol externs

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Validates a rule instance. The rule is that the rule being instantiated
-- must exist, and that the rule parameters being instantiated must also exist.
-- The rule being instantiated must also have assumptions, otherwise, they are
-- never added to the priority queue.
--
-- @since 0.0.1
validateRuleInstance :: SymbolValidator RuleInstance
validateRuleInstance = validate [ruleExists]
  where
    ruleExists :: SymbolRule RuleInstance
    ruleExists rule_inst env = do
      let rule_name = rule_inst ^. rule
          rule_inst_params = Map.keys $ ruleInstanceMap rule_inst
      x <- getRuleFromRuleInstance rule_inst env
      case x of
        Nothing -> do
          pos <- getPosition rule_name
          return [Err Nothing "unknown rule" [(pos, This "rule name")] []]
        Just r -> do
          let rule_params = env ^. ruleInfos . ix (r ^. nodeId) . args & Map.keys
              rule_inst_params_that_do_not_match =
                filter
                  (\i -> all (≠ (simpleIdentifier i)) rule_params)
                  rule_inst_params
          -- NOTE: earlier, we threw an error when the rule parameters were
          -- empty; actually, parameter-less rules should be able to be
          -- instantiated. What we probably want is to have assumption-less
          -- rules be uninstantiable because they will not ever be added to the
          -- work queue (they are added as initial, starting facts.)
          if null (r ^. assumptions)
            then do
              pos <- getPosition r
              pos' <- getPosition rule_inst
              return
                [ Err
                    Nothing
                    "invalid rule instance"
                    [(pos', This "rule instance"), (pos, Where "rule")]
                    [Note "cannot create instances of rules with no assumptions"]
                ]
            else
              if null rule_inst_params_that_do_not_match
                then return []
                else do
                  pos <- mapM getPosition rule_inst_params_that_do_not_match
                  rule_pos <- getPosition r
                  return
                    [ Err
                        Nothing
                        "unknown rule parameters"
                        ((rule_pos, Where "rule declaration") : ((,This "rule parameter") <$> pos))
                        []
                    ]

-- | Obtains a 'Rule' declaration from a rule instance.
--
-- @since 0.0.1
getRuleFromRuleInstance
  :: RuleInstance
  -- ^ The 'RuleInstance'
  -> SymbolEnv
  -- ^ The 'SymbolEnv'
  -> FixenPass SymbolState (Maybe Rule)
getRuleFromRuleInstance rule_inst env = do
  let rule_name = rule_inst ^. rule
      all_relevant_names =
        values (env ^. ruleInfos)
          <&> (^. declaration)
          <&> ( \d -> case ruleName d of
                  Nothing -> Nothing
                  Just n -> Just (d, n)
              )
          & catMaybes
          & filter ((≅ rule_name) ∘ snd)
  case all_relevant_names of
    [] -> return Nothing
    ((r, _) : _) -> return $ Just r
