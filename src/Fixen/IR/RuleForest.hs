module Fixen.IR.RuleForest (RuleLeaf (..), RuleTreeChoppedHead (..), RuleForest (..), showPhasedForests) where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Text (Text, unpack)
import Data.Tree
import Fixen.Fields
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
showPhaseOfForests unicode (i, ls) = "*Phase " ++ show (i + 1) ++ "*\n" ++ (myDrawForest unicode $ forestToRoseForest ls)

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
