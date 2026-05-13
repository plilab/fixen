-- |
-- Module      : Fixen.IR.RuleForest
-- Description : Definitions for Rule Forests
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module contains definitions for working with rule forests.
--
-- A rule forest is a data structure used to represent rules in a Fixen program.
-- It instructs the compiler how to generate the code for the sequence of
-- actions to be taken whenever a new fact is concluded by a rule.
--
-- The main data structure for rule forests is 'NonEmpty' 'RuleForest'. Each
-- element corresponds to the rule forest for a phase in the program (of which
-- there is at least one).
--
-- As seen in the definition of 'RuleForest', it contains a series of trees,
-- each of which are either
--
-- * 'Map' 'Text' ('NonEmpty' 'RuleTreeChoppedHead')
-- * 'RuleLeaf'
--
-- Leaves correspond to when a rule has been completely instantiated by
-- premises and a rule instance can be generated (from which, a conclusion
-- can also be generated).
--
-- The map has relation symbols as keys, and the values are lists of
-- 'RuleTreeChoppedHead'. It is named as such because the head of the tree
-- is a relation minus its relation symbol (hence, the \"chopped head\").
-- For instance, suppose we have a series of rules:
--
-- @
-- rule: R x y |- R x (y + 1)
-- rule: R z z |- R z 1
-- @
--
-- These two rules form a single entry in the map, where the key is the symbol
-- @R@, and its corresponding value is a list of two 'RuleTreeChoppedHead's.
-- The first 'RuleTreeChoppedHead' starts with the arguments @x y@ and proceeds
-- with the rest of the first rule, and the second 'RuleTreeChoppedHead' starts
-- with the arguments @z z@, then proceeds with the rest of the second rule.
-- By chopping the heads of the tree, we can group multiple rules that start
-- with the same relation symbol together, while being sensitive to how
-- variables are being matched in the rule.
--
-- An additional note: the arguments to rules are represented as 'Int's. This
-- is so we are not sensitive to the actual names of variables themselves
-- (aside from how they match with each other). Variables are numbered in
-- ascending order of occurrence, so in the example rules above, the first
-- 'RuleTreeChoppedHead' has arguments @[0, 1]@, while the second will have
-- arguments @[0, 0]@ above. Since these two argument lists are different, they
-- cannot be merged into a single branch in the forest.
--
-- @since 0.0.1
module Fixen.IR.RuleForest (
  -- * Rule Forests
  RuleForest (..),
  RuleTreeChoppedHead (..),
  RuleLeaf (..),

  -- * Showing Rule Forests
  showPhasedForests,
) where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Data.Tree
import Fixen.Fields
import Fixen.IR.AST

--------------------------------------------------------------------------------

-- * Rule Forests

--------------------------------------------------------------------------------

-- | A 'RuleForest', consisting of (typically) at least one tree. Non-leaf trees
-- are stored in '_ruleForestTrees'; leaves are stored in '_ruleForestLeaves'.
--
-- @since 0.0.1
data RuleForest = RuleForest
  { _ruleForestTrees :: Map Text (NonEmpty RuleTreeChoppedHead)
  -- ^ The non-leaf trees
  --
  -- @since 0.0.1
  , _ruleForestLeaves :: [RuleLeaf]
  -- ^ The leaves
  --
  -- @since 0.0.1
  }
  deriving (Eq)

instance HasTrees RuleForest (Map Text (NonEmpty RuleTreeChoppedHead)) where
  trees = lens _ruleForestTrees (\s i -> s {_ruleForestTrees = i})

instance HasLeaves RuleForest [RuleLeaf] where
  leaves = lens _ruleForestLeaves (\s i -> s {_ruleForestLeaves = i})

-- | A tree in the rule forest with its head (the relation symbol of the very
-- first assumption) chopped off.
--
-- @since 0.0.1
data RuleTreeChoppedHead = RuleTreeChoppedHead
  { _ruleTreeChoppedHeadArgs :: [Int]
  -- ^ The arguments to the relation symbol (the relation symbol is not present
  -- in this data structure; it should be stored in the enclosing 'RuleForest').
  --
  -- @since 0.0.1
  , _ruleTreeChoppedHeadBranches :: RuleForest
  -- ^ The children of this node, itself a 'RuleForest'.
  --
  -- @since 0.0.1
  }
  deriving (Eq)

-- | A leaf in the rule forest. It represents a rule with all assumptions
-- removed.
--
-- @since 0.0.1
data RuleLeaf = RuleLeaf
  { _ruleLeafRuleId :: NodeId
  -- ^ The 'NodeId' of the rule this leaf represents
  --
  -- @since 0.0.1
  , _ruleLeafVariableMap :: [Text]
  -- ^ A variable map. The \(i^\text{th}\) element of the map being \(x\) means
  -- that in the tree branch leading up to this leaf, occurrences of \(i\)
  -- represent the variable \(x\). Renaming variables in the conditions and
  -- conclusions of this leaf is simply a matter of renaming via the inverse of
  -- this map.
  --
  -- @since 0.0.1
  , _ruleLeafCondition :: [Condition]
  -- ^ The 'Condition's of this leaf.
  --
  -- @since 0.0.1
  , _ruleLeafConclusion :: Conclusion
  -- ^ The 'Conclusion' of this leaf.
  --
  -- @since 0.0.1
  }
  deriving (Eq)

instance Show RuleLeaf where
  show = drawTree . leafToRoseTree

instance Show RuleForest where
  show = drawForest . forestToRoseForest

-- | Conversion of 'RuleForest's to 'String's.
--
-- @since 0.0.1
showPhasedForests
  :: Bool
  -- ^ Whether to draw the forest with unicode ('True' means unicode symbols
  -- are used).
  -> NonEmpty RuleForest
  -- ^ The forests to draw.
  -> String
showPhasedForests unicode ls =
  let ls' = NonEmpty.toList ls
      idxed = zip [0 .. length ls' - 1] ls'
   in "**Rule Forest**\n" ++ intercalate "\n" (showPhaseOfForests unicode <$> idxed)

showPhaseOfForests :: Bool -> (Int, RuleForest) -> String
showPhaseOfForests unicode (i, ls) =
  "*Phase "
    ++ show (i + 1)
    ++ "*\n"
    ++ (myDrawForest unicode $ forestToRoseForest ls)

forestToRoseForest :: RuleForest -> [Tree String]
forestToRoseForest RuleForest {_ruleForestTrees = t, _ruleForestLeaves = l} =
  mapToRoseForest t ++ (leafToRoseTree <$> l)

mapToRoseForest :: Map Text (NonEmpty RuleTreeChoppedHead) -> [Tree String]
mapToRoseForest mp =
  -- collapse the map into k,v pairs
  let kv :: [(Text, RuleTreeChoppedHead)] =
        Map.toList mp
          >>= (\(rel_name, chopped_heads) -> (rel_name,) <$> NonEmpty.toList chopped_heads)
      -- make into [(rel_name, rel_args, forest)]
      x :: [(Text, [Int], RuleForest)] =
        (\(k, v) -> (k, _ruleTreeChoppedHeadArgs v, _ruleTreeChoppedHeadBranches v)) <$> kv
      -- convert the first two stuff in the tuple into a node
      y :: [(String, RuleForest)] = (\(a, b, c) -> (relToRoseNode a b, c)) <$> x
   in -- map to a rose forest
      (\(s, f) -> Node s (forestToRoseForest f)) <$> y

relToRoseNode :: Text -> [Int] -> String
relToRoseNode rel_name rel_args =
  let show_args =
        if null rel_args
          then ""
          else " " ++ intercalate " " (showIntArg <$> rel_args)
   in unpack rel_name ++ show_args

showIntArg :: Int -> String
showIntArg (-1) = "_"
showIntArg n = "$" ++ show n

leafToRoseTree :: RuleLeaf -> Tree String
leafToRoseTree
  RuleLeaf
    { _ruleLeafRuleId = rule_id
    , _ruleLeafVariableMap = mp
    , _ruleLeafCondition = conds
    , _ruleLeafConclusion = conc
    } =
    let show_conds =
          if null conds
            then ""
            else intercalate ", " (showCond mp <$> conds) ++ " "
        showed_conc = showConc mp conc
        showed_rule_id = concat ["<rule ", show rule_id, "> "]
     in Node (concat [show_conds, showed_rule_id, show mp, "\n", showed_conc]) []

showCond :: [Text] -> Condition -> String
showCond m c =
  let e = conditionExpr c
   in "if " ++ showExpr m e

showConc :: [Text] -> Conclusion -> String
showConc m conc =
  let rel_name = conc ^. name
      rel_args = conc ^. args
      showed_args = if null rel_args then "" else " " ++ intercalate " " (showExpr m <$> rel_args)
   in concat ["⊢ ", unpack $ simpleIdentifier rel_name, showed_args]

showExpr :: [Text] -> Expr -> String
showExpr m (ExprVar _ (IdentifierSimpleIdentifier s))
  | Just i <- elemIndex (simpleIdentifier s) m =
      '$' : show i
  | otherwise = unpack (simpleIdentifier s)
showExpr _ (ExprVar _ i) = unpack (fullIdentifier i)
showExpr m (ExprApp _ l r) =
  concat
    [ "("
    , showExpr m l
    , " "
    , showExpr m r
    , ")"
    ]
showExpr _ (ExprUnit _) = "()"
showExpr _ (ExprIntLit _ i) = show i
showExpr _ (ExprStrLit _ i) = show i
showExpr m (ExprList _ ls) =
  let com_sep = intercalate ", " (showExpr m <$> ls)
   in concat ["[", com_sep, "]"]
showExpr m (ExprTuple _ hd tl) =
  let all_stuff = hd : (NonEmpty.toList tl)
      com_sep = intercalate ", " (showExpr m <$> all_stuff)
   in concat ["(", com_sep, ")"]

myDrawTree :: Bool -> Tree String -> String
myDrawTree unicode = unlines . myDraw unicode

myDrawForest :: Bool -> [Tree String] -> String
myDrawForest unicode = unlines . fmap (myDrawTree unicode)

myDraw :: Bool -> Tree String -> [String]
myDraw unicode (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
      pipe : shift hook "   " (myDraw unicode t)
    drawSubTrees (t : ts) =
      pipe : shift cros (pipe ++ "  ") (myDraw unicode t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
    pipe = if unicode then "│" else "|"
    hook = if unicode then "╰─ " else "`-"
    cros = if unicode then "├─ " else "+-"
