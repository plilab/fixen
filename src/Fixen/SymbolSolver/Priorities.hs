{-# LANGUAGE OverloadedStrings #-}

module Fixen.SymbolSolver.Priorities where

import Control.Lens
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern
import Fixen.SymbolSolver.Validation

getRuleFromRuleInstance :: RuleInstance -> SymbolEnv -> FixenPass SymbolState (Maybe Rule)
getRuleFromRuleInstance rule_inst env = do
  let rule_name = ruleInstanceRule rule_inst
      all_relevant_names =
        env ^. ruleMap
          & foldr (:) []
          <&> (^. ruleDeclaration)
          <&> ( \d -> case ruleName d of
                  Nothing -> Nothing
                  Just n -> Just (d, n)
              )
          & catMaybes
          & filter ((=== rule_name) . snd)
  case all_relevant_names of
    [] -> return Nothing
    ((rule, _) : _) -> return $ Just rule

validateRuleInstance :: SymbolValidator RuleInstance
validateRuleInstance = validate [ruleExists]
  where
    ruleExists :: SymbolRule RuleInstance
    ruleExists rule_inst env = do
      let rule_name = ruleInstanceRule rule_inst
          rule_inst_params = Map.keys $ ruleInstanceMap rule_inst
      x <- getRuleFromRuleInstance rule_inst env
      case x of
        Nothing -> do
          pos <- fixenGetPosition rule_name
          return [Err Nothing "unknown rule" [(pos, This "rule name")] []]
        Just rule -> do
          let rule_params = env ^. ruleMap . ix (getNodeId rule) . Fixen.Monad.ruleBoundVars & Map.keys
              rule_inst_params_that_do_not_match = filter (\i -> all (/= (simpleIdentifier i)) rule_params) rule_inst_params
          if null rule_inst_params_that_do_not_match
            then return []
            else do
              pos <- mapM fixenGetPosition rule_inst_params_that_do_not_match
              rule_pos <- fixenGetPosition rule
              return
                [ Err
                    Nothing
                    "unknown rule parameters"
                    ((rule_pos, Where "rule declaration") : ((,This "rule parameter") <$> pos))
                    []
                ]

initEnvWithPriorities :: SymbolEnv -> Priority -> FixenPass SymbolState SymbolEnv
initEnvWithPriorities env p = do
  let premise = priorityPremise p
      concl = priorityConclusion p
      lhs = priorityConclusionLHS concl
      rhs = priorityConclusionRHS concl

  -- Validate the names and the rule parameters of these instances
  lhs_invalid <- validateRuleInstance lhs env
  rhs_invalid <- validateRuleInstance rhs env
  if lhs_invalid || rhs_invalid
    then return env
    else do
      -- now ensure that there are no duplicate local variables across the entire
      -- priority declaration
      let lhs_map = ruleInstanceMap lhs
          rhs_map = ruleInstanceMap rhs
      lhs_rule <- getNodeId <$> fromJust <$> getRuleFromRuleInstance lhs env
      rhs_rule <- getNodeId <$> fromJust <$> getRuleFromRuleInstance rhs env
      let lhs_local_vars = foldr (:) [] lhs_map
          rhs_local_vars = foldr (:) [] rhs_map
          lhs_local_var_map = Map.fromList $ (\i -> (simpleIdentifier i, (lhs_rule, i))) <$> lhs_local_vars
          rhs_local_var_map = Map.fromList $ (\i -> (simpleIdentifier i, (rhs_rule, i))) <$> rhs_local_vars
          all_local_vars = lhs_local_vars ++ rhs_local_vars
          duplicate_local_vars_map =
            all_local_vars
              <&> (\i -> Map.singleton (simpleIdentifier i) [i])
              & Map.unionsWith (++)
              & Map.filter (\ls -> length ls > 1)
      if not (null duplicate_local_vars_map)
        then do
          let all_vars = foldl' (++) [] duplicate_local_vars_map
          pos <- mapM fixenGetPosition all_vars
          accumErr
            Nothing
            "duplicate local variables"
            ((,This "variable") <$> pos)
            [Note "priority local variables must be unique"]
          return env
        else do
          -- take out unused variables
          let used_ids = Set.toList $ getAllExprNames premise
              used_names = simpleIdentifier <$> used_ids
              unused = filter (\i -> simpleIdentifier i `notElem` used_names) all_local_vars
          -- time to warn unused
          when (not (null unused)) $ do
            pos <- mapM fixenGetPosition unused
            accumWarn Nothing "unused local variable(s)" ((,This "variable") <$> pos) []
          let used_local_vars = filter (\i -> simpleIdentifier i `elem` used_names) all_local_vars
              used_local_vars_names = Set.fromList $ simpleIdentifier <$> used_local_vars
              externs = filter (\i -> simpleIdentifier i `Set.notMember` used_local_vars_names) used_ids
              used_local_vars_lhs = Map.filterKeys (`elem` used_names) lhs_local_var_map
              used_local_vars_rhs = Map.filterKeys (`elem` used_names) rhs_local_var_map
          forM_ (used_local_vars) $ \i ->
            validate [warnNameShadowingAgainstExtern "local variable" (simpleIdentifier i)] i env
          let priority_info =
                PriorityInfo
                  { _priorityDeclaration = p
                  , _priorityLocalVars = Map.union used_local_vars_lhs used_local_vars_rhs
                  }

          let priority_node_id = getNodeId p
          -- time to insert the priority info
          env
            & priorityMap . at priority_node_id ?~ priority_info
            & foldMWith initEnvWithExternSymbol externs
