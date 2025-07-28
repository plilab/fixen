{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax.Compact where

import Common.Util
import qualified Data.HashMap.Strict as M
import Data.Foldable (Foldable(foldl'))
import Syntax.Common
import Prettyprinter

type CExpr = Expr CVar
type CAtomExpr = AtomExpr CVar

data CompactProgram prem a = Compact
  { moduleDecl    :: Module
  , imports       :: [Import]
  , dataDefs      :: [DataDef]
  , signatures    :: [Signature]
  , ruleForest    :: RuleForest prem a
  , continuations :: M.HashMap Identifier (Continuation Var)
  , priorities    :: [PriorityClause]
  , querries      :: [ModalDef]
  } deriving (Show)

data RuleTree prem a 
  = Result ContinuationFact 
  | Branch (prem a) [RuleTree prem a]
  deriving (Show)

newtype RuleForest prem a = RF { getTrees :: [RuleTree prem a] }
  deriving (Show)

type ImplicitRuleTree   = RuleTree Premise Var
type ImplicitRuleForest = RuleForest Premise Var
type ImplicitCompactProgram = CompactProgram Premise Var

type ExplicitRuleTree   = RuleTree CPremise CVar
type ExplicitRuleForest = RuleForest CPremise CVar
type ExplicitCompactProgram = CompactProgram CPremise CVar

data Continuation v = Cont
  { context    :: [(Var, TypeExpr)]
  , conclusion :: Conclusion v
  } deriving (Show)

type ContinuationFact = Proposition Var

instance Functor Continuation where
  fmap f cont = cont { conclusion = f <$> conclusion cont }

instance (Functor f) => Functor (RuleTree f) where
  fmap f rt = case rt of
    (Result cs)   -> Result cs -- $ fmap f cs
    (Branch p ts) -> Branch (fmap f p) $ map (fmap f) ts

instance (Traversable f) => Foldable (RuleTree f) where
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance (Traversable f) => Traversable (RuleTree f) where
  traverse f rt = case rt of
    (Result cs) -> pure $ Result cs -- <$> traverse f cs
    (Branch p ts) ->
      Branch
        <$> traverse f p
        <*> traverse (traverse f) ts

instance (Pretty (f a), Pretty a) => Pretty (RuleTree f a) where
  pretty t = "|-" <+> case t of
    (Result cs) -> align . pretty $ cs
    (Branch p ts) -> pretty p <+> (align . vsep . map pretty $ ts)

instance (Functor f) => Functor (RuleForest f) where
  fmap f (RF trees) = RF $ fmap f <$> trees

instance (Traversable f) => Foldable (RuleForest f) where
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance (Traversable f) => Traversable (RuleForest f) where
  traverse f (RF trees) = RF <$> traverse (traverse f) trees

instance (Pretty (f a), Pretty a) => Pretty (RuleForest f a) where
  pretty = vsep . map pretty . getTrees

instance (Pretty v) => Pretty (Continuation v) where
  pretty (Cont ctx concl) = pretty ctx <+> "->" <+> pretty concl

instance (Pretty a, Pretty (prem a)) => Pretty (CompactProgram prem a) where
  pretty prog = vsep [
    vsep (pretty <$> imports prog),
    vsep (pretty <$> dataDefs prog),
    "rels:" <+> (align . vsep) (pretty <$> signatures prog),
    "tree:" <+> (align . pretty . ruleForest) prog,
    "conts:" <+> (align . vsep . fmap pretty . M.toList . continuations) prog,
    "priorities:" <+> (align . vsep) (pretty <$> priorities prog),
    "qrys:" <+> (align . vsep) (pretty <$> querries prog)
    ]
