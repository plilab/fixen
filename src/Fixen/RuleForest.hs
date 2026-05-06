{-# LANGUAGE OverloadedStrings #-}

module Fixen.RuleForest where

import Data.Bifunctor
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.IR.RuleForest
import Fixen.Monad

type RuleForestState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

getRuleForest :: Program -> FixenPass RuleForestState (NonEmpty RuleForest)
getRuleForest prog = do
  prog_phases <- fixenGetPhases
  let r = IntMap.fromList $ (\r' -> (getNodeId r', r')) <$> rules prog
      phased_rules = (\ns -> (r IntMap.!) <$> IntSet.toList ns) <$> prog_phases
      -- there must be at least one rule, so we can make it a NonEmpty (NonEmpty Rule)
      phased_rules' = (\ls -> ls !! 0 :| drop 1 ls) <$> phased_rules
      rts = (\r' -> getBranch <$> r') <$> phased_rules'
      phased_forests = concatNonEmpty <$> rts
      merged_forests = mergePhaseForests <$> phased_forests
  return merged_forests

mergePhaseForests :: NonEmpty RuleForest -> RuleForest
mergePhaseForests (x :| xs) = foldl' mergeForests x xs

mergeForests :: RuleForest -> RuleForest -> RuleForest
mergeForests RuleForest {_ruleForestTrees = t1, _ruleForestLeaves = l1} RuleForest {_ruleForestTrees = t2, _ruleForestLeaves = l2} =
  let new_leaves = l1 ++ l2
      new_trees = Map.unionWith mergeMaps t1 t2
   in RuleForest {_ruleForestTrees = new_trees, _ruleForestLeaves = new_leaves}

concatNonEmpty :: NonEmpty (NonEmpty a) -> NonEmpty a
concatNonEmpty (x :| xs) = foldl' NonEmpty.append x xs

mergeMaps :: NonEmpty RuleTreeChoppedHead -> NonEmpty RuleTreeChoppedHead -> NonEmpty RuleTreeChoppedHead
mergeMaps lhs (y :| ys) =
  case attemptMerge lhs y of
    Just lhs' ->
      case ys of
        [] -> lhs'
        (y' : ys') -> mergeMaps lhs' (y' :| ys')
    Nothing ->
      case ys of
        [] -> NonEmpty.cons y lhs
        (y' : ys') -> NonEmpty.cons y (mergeMaps lhs (y' :| ys'))

attemptMerge :: NonEmpty RuleTreeChoppedHead -> RuleTreeChoppedHead -> Maybe (NonEmpty RuleTreeChoppedHead)
attemptMerge (x :| xs) t =
  let merged = tryMerge x t
   in case merged of
        Nothing -> case xs of
          [] -> Nothing
          (y : ys) ->
            let z = attemptMerge (y :| ys) t
             in NonEmpty.cons x <$> z
        Just res -> Just (res :| xs)

tryMerge :: RuleTreeChoppedHead -> RuleTreeChoppedHead -> Maybe RuleTreeChoppedHead
tryMerge RuleTreeChoppedHead {_ruleTreeChoppedHeadArgs = a1, _ruleTreeChoppedHeadBranches = b1} RuleTreeChoppedHead {_ruleTreeChoppedHeadArgs = a2, _ruleTreeChoppedHeadBranches = b2} =
  if a1 == a2
    then
      Just
        RuleTreeChoppedHead
          { _ruleTreeChoppedHeadArgs = a1
          , _ruleTreeChoppedHeadBranches = mergeForests b1 b2
          }
    else Nothing

getBranch :: Rule -> NonEmpty RuleForest
getBranch rule =
  let asms = ruleAssumptions rule
   in case asms of
        [] ->
          RuleForest
            { _ruleForestTrees = Map.empty
            , _ruleForestLeaves =
                [ RuleLeaf
                    { _ruleLeafRuleId = getNodeId rule
                    , _ruleLeafVariableMap = []
                    , _ruleLeafCondition = ruleConditions rule
                    , _ruleLeafConclusion = ruleConclusion rule
                    }
                ]
            }
            :| []
        _ ->
          -- factor out every possible assumption. Do not merge duplicates at this point!!!
          let factored :: NonEmpty (Assumption, Rule) = factorRelation <$> (0 :| [1 .. length asms - 1])
              x :: NonEmpty ((Text, [Int], [Text]), Rule) = (first (getVariableNumbering [])) <$> factored
           in ( \((rel_name, rel_args, mapping), rul) ->
                  RuleForest
                    { _ruleForestLeaves = []
                    , _ruleForestTrees =
                        Map.singleton
                          rel_name
                          ( RuleTreeChoppedHead
                              { _ruleTreeChoppedHeadArgs = rel_args
                              , _ruleTreeChoppedHeadBranches = nonCombinatoriallyGetBranch mapping rul
                              }
                              :| []
                          )
                    }
              )
                <$> x
  where
    factorRelation i = do
      let asms = ruleAssumptions rule
          factored = asms !! i
          remaining = take i asms ++ drop (i + 1) asms
       in (factored, rule {ruleAssumptions = remaining})
    getVariableNumbering :: [Text] -> Assumption -> (Text, [Int], [Text])
    getVariableNumbering bound_var_mapping asm =
      let name = simpleIdentifier $ relationName asm
          args = simpleIdentifier <$> relationParams asm
          (idx, new_mapping) = foldl' f ([], bound_var_mapping) args
       in (name, reverse idx, new_mapping)
    f :: ([Int], [Text]) -> Text -> ([Int], [Text])
    f (ls, mapping) t =
      -- I think, continue to bind the holes so that we can more easily merge stuff.
      -- However, unconditionally bind holes as new variables so that the variables
      -- are not matched.
      if t == "_"
        then ((length mapping) : ls, t : mapping)
        else case elemIndex t mapping of
          Just i -> ((length mapping - i - 1) : ls, mapping)
          Nothing ->
            ((length mapping) : ls, t : mapping)
    nonCombinatoriallyGetBranch :: [Text] -> Rule -> RuleForest
    nonCombinatoriallyGetBranch var_mapping rul =
      case ruleAssumptions rul of
        [] ->
          RuleForest
            { _ruleForestLeaves =
                [ RuleLeaf
                    { _ruleLeafRuleId = getNodeId rul
                    , _ruleLeafVariableMap = reverse var_mapping
                    , _ruleLeafCondition = ruleConditions rule
                    , _ruleLeafConclusion = ruleConclusion rule
                    }
                ]
            , _ruleForestTrees = Map.empty
            }
        (asm : xs) ->
          let new_rul = rul {ruleAssumptions = xs}
              (rel_name, idx, new_mapping) = getVariableNumbering var_mapping asm
           in RuleForest
                { _ruleForestLeaves = []
                , _ruleForestTrees =
                    Map.singleton
                      rel_name
                      ( RuleTreeChoppedHead
                          { _ruleTreeChoppedHeadArgs = idx
                          , _ruleTreeChoppedHeadBranches = nonCombinatoriallyGetBranch new_mapping new_rul
                          }
                          :| []
                      )
                }
