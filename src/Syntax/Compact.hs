{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax.Compact where

import Common.Util
import Data.Foldable (Foldable(foldl'))
import Syntax.Common
import Prettyprinter

type CExpr = Expr CVar
type CAtomExpr = AtomExpr CVar

data CompactProgram assump a = Compact 
  { moduleDecl :: Module
  , imports    :: [Import]
  , dataDefs   :: [DataDef]
  , signatures :: [Signature]
  , ruleForest :: RuleForest assump a
  , ordClauses :: [OrdClause Var]
  , querries   :: [ModalDef]
  } deriving (Show)

data RuleTree assump a = Result (Conclusion a) | Branch (assump a) [RuleTree assump a] --RT { getCase :: Case (Conclusion a) (assump a) (RuleTree assump a) }
  deriving (Show)

newtype RuleForest assump a = RF { getTrees :: [RuleTree assump a] }
  deriving (Show)

type ImplicitRuleTree   = RuleTree Assumption Var
type ImplicitRuleForest = RuleForest Assumption Var
type ImplicitCompactProgram = CompactProgram Assumption Var

type ExplicitRuleTree   = RuleTree CAssumption CVar
type ExplicitRuleForest = RuleForest CAssumption CVar
type ExplicitCompactProgram = CompactProgram CAssumption CVar

{- data RuleTree a
  = Conclusions [Proposition (Expr a)]
  | Branch (Proposition (AtomExpr a)) [RuleTree a] -}

instance (Functor f) => Functor (RuleTree f) where
  fmap f rt = case rt of
    (Result cs)   -> Result $ fmap f cs
    (Branch p ts) -> Branch (fmap f p) $ map (fmap f) ts

instance (Traversable f) => Foldable (RuleTree f) where
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance (Traversable f) => Traversable (RuleTree f) where
  traverse f rt = case rt of
    (Result cs) -> Result <$> traverse f cs
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

instance (Pretty a, Pretty (assump a)) => Pretty (CompactProgram assump a) where
  pretty prog = vsep [
    vsep (pretty <$> imports prog),
    vsep (pretty <$> dataDefs prog),
    "rels:" <+> (align . vsep) (pretty <$> signatures prog),
    "tree:" <+> (align . pretty . ruleForest) prog,
    "ords:" <+> (align . vsep) (pretty <$> ordClauses prog),
    "qrys:" <+> (align . vsep) (pretty <$> querries prog)
    ]
