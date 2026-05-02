module Fixen.IR.RuleForest (RuleLeaf (..), RuleTreeChoppedHead (..), RuleForest (..), showPhasedForests) where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Data.Tree
import Fixen.Data.NodeId
import Fixen.IR.AST

data RuleLeaf = RuleLeaf
  { _ruleLeafRuleId :: NodeId
  , _ruleLeafVariableMap :: [Text]
  , _ruleLeafCondition :: [Condition]
  , _ruleLeafConclusion :: Conclusion
  }
  deriving (Eq)

instance Show RuleLeaf where
  show = drawTree . leafToRoseTree

data RuleTreeChoppedHead = RuleTreeChoppedHead
  { _ruleTreeChoppedHeadArgs :: [Int]
  , _ruleTreeChoppedHeadBranches :: RuleForest
  }
  deriving (Eq)

data RuleForest = RuleForest
  { _ruleForestTrees :: Map Text (NonEmpty RuleTreeChoppedHead)
  , _ruleForestLeaves :: [RuleLeaf]
  }
  deriving (Eq)

instance Show RuleForest where
  show = drawForest . forestToRoseForest

showPhasedForests :: Bool -> NonEmpty RuleForest -> String
showPhasedForests unicode ls =
  let ls' = NonEmpty.toList ls
      idxed = zip [0 .. length ls' - 1] ls'
   in "**Rule Forest**\n" ++ intercalate "\n" (showPhaseOfForests unicode <$> idxed)

showPhaseOfForests :: Bool -> (Int, RuleForest) -> String
showPhaseOfForests unicode (i, ls) = "*Phase " ++ show i ++ "*\n" ++ (myDrawForest unicode $ forestToRoseForest ls)

forestToRoseForest :: RuleForest -> [Tree String]
forestToRoseForest RuleForest {_ruleForestTrees = t, _ruleForestLeaves = l} =
  mapToRoseForest t ++ (leafToRoseTree <$> l)

mapToRoseForest :: Map Text (NonEmpty RuleTreeChoppedHead) -> [Tree String]
mapToRoseForest mp =
  -- collapse the map into k,v pairs
  let kv :: [(Text, RuleTreeChoppedHead)] = Map.toList mp >>= (\(rel_name, chopped_heads) -> (rel_name,) <$> NonEmpty.toList chopped_heads)
      -- make into [(rel_name, rel_args, forest)]
      x :: [(Text, [Int], RuleForest)] = (\(k, v) -> (k, _ruleTreeChoppedHeadArgs v, _ruleTreeChoppedHeadBranches v)) <$> kv
      -- convert the first two stuff in the tuple into a node
      y :: [(String, RuleForest)] = (\(a, b, c) -> (relToRoseNode a b, c)) <$> x
   in -- map to a rose forest
      (\(s, f) -> Node s (forestToRoseForest f)) <$> y

relToRoseNode :: Text -> [Int] -> String
relToRoseNode rel_name rel_args =
  let show_args = if null rel_args then "" else " " ++ intercalate " " (showIntArg <$> rel_args)
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
    let show_conds = if null conds then "" else intercalate ", " (showCond mp <$> conds) ++ " "
        showed_conc = showConc mp conc
        showed_rule_id = concat ["<rule ", show rule_id, "> "]
     in Node (concat [show_conds, showed_rule_id, showed_conc]) []

showCond :: [Text] -> Condition -> String
showCond mapping c =
  let expr = conditionExpr c
   in "if " ++ showExpr mapping expr

showConc :: [Text] -> Conclusion -> String
showConc mapping conc =
  let rel_name = relationName conc
      rel_args = relationParams conc
      showed_args = if null rel_args then "" else " " ++ intercalate " " (showExpr mapping <$> rel_args)
   in concat ["⊢ ", unpack $ simpleIdentifier rel_name, showed_args]

showExpr :: [Text] -> Expr -> String
showExpr mapping (ExprVar _ (IdentifierSimpleIdentifier s))
  | Just i <- elemIndex (simpleIdentifier s) mapping =
      '$' : show i
  | otherwise = unpack (simpleIdentifier s)
showExpr _ (ExprVar _ i) = unpack (fullIdentifier i)
showExpr mapping (ExprApp _ lhs rhs) =
  concat
    [ "("
    , showExpr mapping lhs
    , " "
    , showExpr mapping rhs
    , ")"
    ]
showExpr _ (ExprUnit _) = "()"
showExpr _ (ExprIntLit _ i) = show i
showExpr _ (ExprStrLit _ i) = show i
showExpr mapping (ExprList _ ls) =
  let com_sep = intercalate ", " (showExpr mapping <$> ls)
   in concat ["[", com_sep, "]"]
showExpr mapping (ExprTuple _ hd tl) =
  let all_stuff = hd : (NonEmpty.toList tl)
      com_sep = intercalate ", " (showExpr mapping <$> all_stuff)
   in concat ["(", com_sep, ")"]

myDrawTree :: Bool -> Tree String -> String
myDrawTree unicode = unlines . myDraw unicode

myDrawForest :: Bool -> [Tree String] -> String
myDrawForest unicode = unlines . map (myDrawTree unicode)

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
