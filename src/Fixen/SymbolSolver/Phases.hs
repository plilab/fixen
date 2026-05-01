module Fixen.SymbolSolver.Phases where

import Control.Lens
import Control.Monad
import Data.Either
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common

initEnvWithPhases :: Maybe Phases -> SymbolEnv -> FixenPass SymbolState SymbolEnv
initEnvWithPhases Nothing env =
  -- trivial. Just get all the rules and use that as the phases
  return $ env & infoMap . phaseInfo .~ [all_rules]
  where
    all_rules = env ^. infoMap . ruleInfoMap & IntMap.keys & Set.fromList
initEnvWithPhases (Just p) env = do
  let x = rule_ids <$> phasesPhases p
      fails = concat $ catMaybes $ NonEmpty.toList $ onlyFailing <$> x
  if not (null fails)
    then do
      pos <- mapM fixenGetPosition fails
      accumErr
        Nothing
        "unknown rules"
        ((,This "rule") <$> pos)
        []
      return env
    else do
      let s = NonEmpty.toList $ succeeding <$> x
          all_explicit_rules = Set.unions $ fst $ partitionEithers s
          all_remaining_rules = all_rules Set.\\ all_explicit_rules
          (_, everything_elses) = partitionEithers s
      if not (null everything_elses) && null all_remaining_rules
        then do
          pos <- mapM fixenGetPosition everything_elses
          accumErr
            Nothing
            "empty phase"
            ((,This "this phase is empty") <$> pos)
            [ Note "Every phase must have at least one rule"
            , Hint "Consider explicitly listing the rules in each phase"
            ]
          return env
        else do
          let new_phases =
                map
                  ( \x'' -> case x'' of
                      Left x' -> x'
                      Right _ -> all_remaining_rules
                  )
                  s
              all_used = Set.unions new_phases
              all_unused = all_rules Set.\\ all_used
          when (not (null all_unused)) $ do
            pos <- mapM fixenGetPosition (Set.toList all_unused)
            accumWarn
              Nothing
              "unused rule"
              ((,This "rule") <$> pos)
              [Hint "did you forget these in your phases declaration?"]
          return $ env & infoMap . phaseInfo .~ new_phases
  where
    all_rules = env ^. infoMap . ruleInfoMap & IntMap.keys & Set.fromList
    rule_ids (Left rs) =
      let rule_names :: NonEmpty SimpleIdentifier = ruleSetRules rs
       in Left $ f <$> rule_names
    rule_ids (Right i) = Right i
    f i =
      let k = filter (\(_, i') -> i' === i) all_rule_names
       in case k of
            [] -> Left i
            (x, _) : _ -> Right x
    all_rule_names =
      env ^. infoMap . ruleInfoMap
        & IntMap.toList
        <&> ( \(x, y) -> case ruleName (y ^. ruleDeclaration) of
                Just n -> Just (x, n)
                Nothing -> Nothing
            )
        & catMaybes
    onlyFailing :: Either (NonEmpty (Either SimpleIdentifier NodeId)) EverythingElseRuleset -> Maybe [SimpleIdentifier]
    onlyFailing (Left ls) =
      let (left, _) = partitionEithers (NonEmpty.toList ls)
       in Just left
    onlyFailing _ = Nothing

    succeeding :: Either (NonEmpty (Either SimpleIdentifier NodeId)) EverythingElseRuleset -> Either (Set NodeId) EverythingElseRuleset
    succeeding (Left ls) =
      let (_, rights') = partitionEithers (NonEmpty.toList ls)
       in Left (Set.fromList rights')
    succeeding (Right x) = Right x
