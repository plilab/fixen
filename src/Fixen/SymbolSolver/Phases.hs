{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Fixen.SymbolSolver.Phases
-- Description : Symbol solving for phases
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides facilities for solving phases declarations.
--
-- @since 26.7
module Fixen.SymbolSolver.Phases where

import Control.Lens
import Control.Monad
import Data.Either
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with an optional 'PhasesDeclaration', as produced
-- by the parser. If no 'PhasesDeclaration' is provided, the symbol solver
-- defaults the phases to @[*]@, i.e., a single phase containing every declared
-- rule.
--
-- /Precondition/: Rules must have been initialized in the 'SymbolEnv'.
--
-- @since 26.7
initEnvWithPhases
  :: SymbolState σ
  => Maybe PhasesDeclaration
  -- ^ The 'PhasesDeclaration'
  -> SymbolEnv
  -- ^ The 'SymbolEnv'
  -> FixenPass σ SymbolEnv
initEnvWithPhases Nothing env =
  -- trivial. Just get all the rules and use that as the phases
  return $ env & phaseInfos .~ (all_rules :| [])
  where
    all_rules = env ^. ruleInfos & IntMap.keys & IntSet.fromList
initEnvWithPhases (Just p) env = do
  -- match the rules in explicit rulesets with rules in the program; if
  -- there are no matches for a rule, retain the 'SimpleIdentifier' so we
  -- can emit errors
  let matched_rules_attempt = p ^. phases <&> rule_ids
      fails = matched_rules_attempt <&> onlyFailing & NonEmpty.toList & catMaybes & concat
  if (¬) (null fails)
    then do
      pos <- mapM getPosition fails
      accumErr
        Nothing
        "unknown rules"
        ((,This "rule") <$> pos)
        []
      return env
    else do
      -- remove all the "failing" explicit rules
      let matched_rules = matched_rules_attempt <&> succeeding
          -- get what rules * is referring to
          all_explicit_rules = matched_rules & NonEmpty.toList & partitionEithers & fst & (⋃)
          all_remaining_rules = all_rules ∖ all_explicit_rules
          (_, everything_elses) = partitionEithers (NonEmpty.toList matched_rules)
      if (¬) (null everything_elses) ∧ IntSet.null all_remaining_rules
        then do
          pos <- mapM getPosition everything_elses
          accumErr
            Nothing
            "empty phase"
            ((,This "this phase is empty") <$> pos)
            [ Note "Every phase must have at least one rule"
            , Hint "Consider explicitly listing the rules in each phase"
            ]
          return env
        else do
          -- Now convert all the * into an explicit set of rules too
          let explicit_phases = (\case Left x' -> x'; Right _ -> all_remaining_rules) <$> matched_rules
              all_used = (⋃) explicit_phases
              all_unused = all_rules ∖ all_used
          when ((¬) (IntSet.null all_unused)) $ do
            pos <- mapM getPositionFromNodeId (IntSet.toList all_unused)
            accumWarn
              Nothing
              "unused rule"
              ((,This "rule") <$> pos)
              [Hint "did you forget these in your phases declaration?"]
          return $ env & phaseInfos .~ explicit_phases
  where
    all_rules = env ^. ruleInfos & IntMap.keys & IntSet.fromList
    -- Converts an explicit ruleset into an explicit list of rule node IDs (when
    -- there is a matching rule) or 'SimpleIdentifier's (when no rule names
    -- match the rule being declared as being in the phase).
    rule_ids
      :: Either Ruleset EverythingElseRuleset
      -> Either (NonEmpty (Either SimpleIdentifier NodeId)) EverythingElseRuleset
    rule_ids (Left rs) =
      let rule_names :: NonEmpty SimpleIdentifier = rs ^. rules
       in Left $ match_against_all_rules <$> rule_names
    rule_ids (Right i) = Right i

    match_against_all_rules i =
      let k = filter (\(_, i') -> i' ≅ i) all_rule_names
       in case k of
            [] -> Left i
            (x, _) : _ -> Right x

    all_rule_names =
      env ^. ruleInfos
        & IntMap.toList
        <&> ( \(x, y) -> case ruleName (y ^. declaration) of
                Just n -> Just (x, n)
                Nothing -> Nothing
            )
        & catMaybes

    onlyFailing
      :: Either
          (NonEmpty (Either SimpleIdentifier NodeId))
          EverythingElseRuleset
      -> Maybe [SimpleIdentifier]
    onlyFailing (Left ls) =
      let (left, _) = partitionEithers (NonEmpty.toList ls)
       in Just left
    onlyFailing _ = Nothing

    succeeding
      :: Either
          (NonEmpty (Either SimpleIdentifier NodeId))
          EverythingElseRuleset
      -> Either NodeSet EverythingElseRuleset
    succeeding (Left ls) =
      let (_, rights') = partitionEithers (NonEmpty.toList ls)
       in Left (IntSet.fromList rights')
    succeeding (Right x) = Right x
