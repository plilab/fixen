{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.RuleForest
-- Description : Generating Rule Forests
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module defines how 'RuleForest's are generated from 'Program's.
--
-- @since 0.0.1
module Fixen.RuleForest where

import Data.Bifunctor
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Fixen.Fields
import Fixen.IR.AST
import Fixen.IR.RuleForest
import Fixen.Monad
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | The state used for obtaining rule forests.
--
-- It must consist of a 'SymbolEnv' for obtaining information about the program.
--
-- @since 0.0.1
type RuleForestState σ = (WithSymbolEnv σ)

-- | Obtains the rule forests of a program.
--
-- @since 0.0.1
getRuleForest :: RuleForestState σ => Program -> FixenPass σ (NonEmpty RuleForest)
getRuleForest prog = do
  -- get all phases in the program. we are getting them from the SymbolEnv
  -- since the phases there are completely explicit
  prog_phases <- fixenGetPhases
  -- get all rules in the program; it should be a map of NodeId -> Rule
  let rule_map =
        prog
          ^. rules
          <&> (\r -> (r ^. nodeId, r))
          & IntMap.fromList
      -- use prog_phases to convert each phase of NodeIds to sets of rules
      phased_rules = (\ns -> (rule_map IntMap.!) <$> IntSet.toList ns) <$> prog_phases
      -- there must be at least one rule, so we can make it a
      -- NonEmpty (NonEmpty Rule)
      phased_rules' = (\ls -> ls !! 0 :| drop 1 ls) <$> phased_rules
      -- now convert each rule to a NonEmpty RuleForest. This gives us a 3D
      -- list of rule forests (which is itself is technically a list of trees,
      -- big brain stuff 🧠)
      rule_trees = (\r' -> getBranch <$> r') <$> phased_rules'
      -- Flatten by one level.
      --
      -- IMPORTANT: notice we are not doing:
      -- concatNonEmpty rule_trees
      --
      -- The reason is because each element of rule_trees represents a phase.
      -- We ABSOLUTELY do not want to merge phases together!!!
      phased_forests = concatNonEmpty <$> rule_trees
      -- Now we merge forests together as much as we can. Same as above, we do
      -- NOT merge ACROSS phases; that's a big no no!
      merged_forests = mergePhaseForests <$> phased_forests
  return merged_forests

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Converts a single rule into a list of 'RuleForest's. Essentially, each
-- 'RuleForest' in this list only has one straight-line branch. Each branch
-- has distinct starting points for the rule. For instance, let's say we have
-- a rule
--
-- @
-- rule: A x y, B y z, C a b, if undefined |- D a a x y z b
-- @
--
-- There are three possible starting points for this rule:
--
-- 1. @A x y@
-- 2. @B y z@
-- 3. @C a b@
--
-- We thus build three rule forests. Each rule forest has a single tree.
-- Each tree is the branch we get, starting from one of these three assumptions
-- and continuing down the assumptions of the rule (in the order as written)
-- until we arrive at the same conclusion.
--
-- @since 0.0.1
getBranch :: Rule -> NonEmpty RuleForest
getBranch r =
  -- Note that this function does not perform any recursion; recursing down
  -- the tree is handled by recursivelyBuildBranch. The reason is it is only
  -- at the root where we factor out different possible assumptions; there is
  -- absolutely no reason to do the same recursively at every step of the rule.
  let asms = r ^. assumptions
   in case asms of
        [] ->
          -- This rule has no assumptions, so is just a leaf. Easy peasy.
          RuleForest
            { _ruleForestTrees = Map.empty
            , _ruleForestLeaves =
                [ RuleLeaf
                    { _ruleLeafRuleId = r ^. nodeId
                    , _ruleLeafVariableMap = []
                    , _ruleLeafCondition = r ^. conditions
                    , _ruleLeafConclusion = r ^. conclusion
                    }
                ]
            }
            :| []
        _ ->
          -- factor out every possible assumption. Do not merge duplicates at this point!!!
          let factored = factorAssumption <$> (0 :| [1 .. length asms - 1])
              -- now destructure the assumption into three things:
              -- 1. the relation name,
              -- 2. the list of arguments ([Int])
              -- 3. the variable mapping ([Text])
              -- The variable mapping must allow us to recover the original
              -- arguments from the [Int] list of arguments
              destructured_assumptions = (first (destructureAssumption [])) <$> factored
           in destructured_assumptions <&> \((rel_name, rel_args, var_mp), rul) ->
                let new_branches = recursivelyBuildBranch var_mp rul
                    new_trees =
                      Map.singleton
                        rel_name
                        ( RuleTreeChoppedHead
                            { _ruleTreeChoppedHeadArgs = rel_args
                            , _ruleTreeChoppedHeadBranches = new_branches
                            }
                            :| []
                        )
                 in RuleForest {_ruleForestLeaves = [], _ruleForestTrees = new_trees}
  where
    factorAssumption :: Int -> (Assumption, Rule)
    factorAssumption i = do
      let asms = r ^. assumptions
          factored = asms !! i
          remaining = take i asms ++ drop (i + 1) asms
       in -- remove the factored assumption from the assumptions of the rule
          (factored, r & assumptions .~ remaining)

    -- Destructures the assumption. The first argument is the current variable
    -- mapping. The result has three components:
    -- 1. The name of the relation
    -- 2. The argument list
    -- 3. The new variable mapping
    destructureAssumption :: [Text] -> Assumption -> (Text, [Int], [Text])
    destructureAssumption var_mapping asm =
      let n = simpleIdentifier $ asm ^. name
          a = simpleIdentifier <$> asm ^. args
          (idx, new_mapping) = foldl' bindNewVar ([], var_mapping) a
       in (n, reverse idx, new_mapping)

    -- Binds a new variable. In other words, given the current argument
    -- list and an existing mapping, and a variable, update the argument
    -- list and the variable mapping by inserting the variable, updating
    -- the variable map if necessary.
    --
    -- all the lists used here are in reverse order. they will be un-reversed
    -- when necessary.
    bindNewVar :: ([Int], [Text]) -> Text -> ([Int], [Text])
    bindNewVar (ls, mp) t =
      -- I think, continue to bind the holes so that we can more easily merge stuff.
      -- However, unconditionally bind holes as new variables so that the variables
      -- are not matched.
      if t == "_"
        then ((length mp) : ls, t : mp)
        else case elemIndex t mp of
          -- the variable is already in the variable map. use the same number.
          Just i -> ((length mp - i - 1) : ls, mp)
          Nothing ->
            -- create a new variable number
            ((length mp) : ls, t : mp)

    -- Recursively descend the rule to build the branch.
    recursivelyBuildBranch :: [Text] -> Rule -> RuleForest
    recursivelyBuildBranch var_mapping rul =
      case rul ^. assumptions of
        [] ->
          -- Nothing left, this is a leaf
          RuleForest
            { _ruleForestLeaves =
                [ RuleLeaf
                    { _ruleLeafRuleId = rul ^. nodeId
                    , -- remember to reverse!
                      _ruleLeafVariableMap = reverse var_mapping
                    , _ruleLeafCondition = rul ^. conditions
                    , _ruleLeafConclusion = rul ^. conclusion
                    }
                ]
            , _ruleForestTrees = Map.empty
            }
        (asm : xs) ->
          -- remove asm from the assumptions of the current rule
          let new_rul = rul & assumptions .~ xs
              -- again, de-structure the arguments
              (rel_name, arguments, new_mapping) = destructureAssumption var_mapping asm
              new_branches = recursivelyBuildBranch new_mapping new_rul
              new_trees =
                Map.singleton
                  rel_name
                  ( RuleTreeChoppedHead
                      { _ruleTreeChoppedHeadArgs = arguments
                      , _ruleTreeChoppedHeadBranches = new_branches
                      }
                      :| []
                  )
           in RuleForest {_ruleForestLeaves = [], _ruleForestTrees = new_trees}

-- | Merges all the rule forests in a phase. The result is a single
-- 'RuleForest'.
--
-- @since 0.0.1
mergePhaseForests
  :: NonEmpty RuleForest
  -- ^ The 'RuleForest's of this phase
  -> RuleForest
mergePhaseForests (x :| xs) = foldl' mergeForests x xs

-- | Merges two forests together. Forest merging is guaranteed to succeed
-- (merging TREES may not succeed, but remember that a 'RuleForest' is sort of
-- a LIST of trees!)
--
-- @since 0.0.1
mergeForests :: RuleForest -> RuleForest -> RuleForest
mergeForests
  RuleForest {_ruleForestTrees = t1, _ruleForestLeaves = l1}
  RuleForest {_ruleForestTrees = t2, _ruleForestLeaves = l2} =
    let new_leaves = l1 ++ l2
        new_trees = Map.unionWith mergeRuleTreeChoppedHeads t1 t2
     in RuleForest {_ruleForestTrees = new_trees, _ruleForestLeaves = new_leaves}

-- | Merges two lists of 'RuleTreeChoppedHead's.
--
-- /Invariant/: In each list, the 'RuleTreeChoppedHead's are mutually
-- un-mergeable.
--
-- @since 0.0.1
mergeRuleTreeChoppedHeads
  :: NonEmpty RuleTreeChoppedHead
  -> NonEmpty RuleTreeChoppedHead
  -> NonEmpty RuleTreeChoppedHead
mergeRuleTreeChoppedHeads l (y :| ys) =
  -- try merging y into anyone in l
  case tryMergeRuleTreeChoppedHeadIntoList l y of
    -- y was successfully merged somewhere in l
    Just lhs' ->
      case ys of
        -- complete
        [] -> lhs'
        -- recursively merge the other stuff in ys
        (y' : ys') -> mergeRuleTreeChoppedHeads lhs' (y' :| ys')
    -- merge failure
    Nothing ->
      case ys of
        -- nothing to do, just put y in l
        [] -> NonEmpty.cons y l
        -- recursively try merging the other stuff, then put y back in.
        (y' : ys') -> NonEmpty.cons y (mergeRuleTreeChoppedHeads l (y' :| ys'))

-- | Attempts to merge a 'RuleTreeChoppedHead' into any one of the
-- 'RuleTreeChoppedHead's in the list.
--
-- /Invariant/: The list of 'RuleTreeChoppedHead's are mutually un-mergeable.
--
-- @since 0.0.1
tryMergeRuleTreeChoppedHeadIntoList
  :: NonEmpty RuleTreeChoppedHead
  -- ^ The list of 'RuleTreeChoppedHead's that are mutually un-mergeable
  --
  -- @since 0.0.1
  -> RuleTreeChoppedHead
  -- ^ The 'RuleTreeChoppedHead' to try merging into any one of the
  -- 'RuleTreeChoppedHead's in the list
  --
  -- @since 0.0.1
  -> Maybe (NonEmpty RuleTreeChoppedHead)
tryMergeRuleTreeChoppedHeadIntoList (x :| xs) t =
  -- first try merging x with t
  case tryMergeRuleTreeChoppedHeads x t of
    -- merge failure
    Nothing -> case xs of
      -- nothing left, just fail, lil bro
      [] -> Nothing
      (y : ys) ->
        -- try merging t into xs
        let z = tryMergeRuleTreeChoppedHeadIntoList (y :| ys) t
         in -- if one of the merges succeed, great. Just put x back in.
            NonEmpty.cons x <$> z
    -- merge success, can return directly
    Just res -> Just (res :| xs)

-- | Attempts to merge two 'RuleTreeChoppedHead's. It is not guaranteed to
-- succeed. It fails primarily whenever the two argument lists are different.
--
-- @since 0.0.1
tryMergeRuleTreeChoppedHeads :: RuleTreeChoppedHead -> RuleTreeChoppedHead -> Maybe RuleTreeChoppedHead
tryMergeRuleTreeChoppedHeads
  RuleTreeChoppedHead {_ruleTreeChoppedHeadArgs = a1, _ruleTreeChoppedHeadBranches = b1}
  RuleTreeChoppedHead {_ruleTreeChoppedHeadArgs = a2, _ruleTreeChoppedHeadBranches = b2} =
    if a1 == a2
      then
        let -- recursively merge the children
            new_forests = mergeForests b1 b2
         in Just
              RuleTreeChoppedHead
                { _ruleTreeChoppedHeadArgs = a1
                , _ruleTreeChoppedHeadBranches = new_forests
                }
      else Nothing
