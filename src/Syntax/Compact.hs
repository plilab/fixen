{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax.Compact where

import Common.Util
import Data.Foldable (Foldable(foldl'))
import Syntax.Common
import Prettyprinter
import Data.Functor
import Data.Bifunctor (Bifunctor(bimap))
import Data.Traversable (for)

type CExpr = Expr CVar
type CAtomExpr = AtomExpr CVar

data CompactProgram a = Compact 
  { moduleDecl :: Module
  , imports    :: [Import]
  , dataDefs   :: [DataDef]
  , signatures :: [Signature]
  , ruleForest :: RuleForest a
  , ordClauses :: [OrdClause Identifier]
  , querries   :: [ModalDef]
  }

data Case a b c = Result a | Branch b [c]

newtype RuleTree a = RT { getCase :: Case (Conclusion a) (Assumption a) (RuleTree a) }
newtype RuleForest a = RF { getTrees :: [(Assumption a, [RuleTree a])] }

{- data RuleTree a
  = Conclusions [Proposition (Expr a)]
  | Branch (Proposition (AtomExpr a)) [RuleTree a] -}

instance Functor RuleTree where
  fmap f rt = RT $ case getCase rt of
    (Result cs)   -> Result $ fmap f cs
    (Branch p ts) -> Branch (fmap f p) $ map (fmap f) ts

instance Foldable RuleTree where
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance Traversable RuleTree where
  traverse f rt = RT <$> case getCase rt of
    (Result cs) -> Result <$> traverse f cs
    (Branch p ts) -> 
      Branch 
        <$> traverse f p
        <*> traverse (traverse f) ts

instance (Pretty a) => Pretty (RuleTree a) where
  pretty t = "|-" <+> case getCase t of
    (Result cs) -> align . pretty $ cs
    (Branch p ts) -> pretty p <+> (align . vsep . map pretty $ ts)

instance Functor RuleForest where
  fmap f (RF trees) = RF $ trees <&> bimap (fmap f) (map $ fmap f)

instance Foldable RuleForest where
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance Traversable RuleForest where
  traverse f (RF trees) = 
    fmap RF $ for trees $ \(prem, tree) -> do
      prem' <- traverse f prem
      tree' <- traverse (traverse f) tree
      pure (prem', tree')

instance (Pretty a) => Pretty (RuleForest a) where
  pretty = vsep . map prettyPair . getTrees
    where
      prettyPair (prop, trees) = pretty prop <+> (align . vsep) (pretty <$> trees)

instance (Pretty a) => Pretty (CompactProgram a) where
  pretty prog = vsep [
    vsep (pretty <$> imports prog),
    vsep (pretty <$> dataDefs prog),
    "rels:" <+> (align . vsep) (pretty <$> signatures prog),
    "tree:" <+> (align . pretty . ruleForest) prog,
    "ords:" <+> (align . vsep) (pretty <$> ordClauses prog),
    "qrys:" <+> (align . vsep) (pretty <$> querries prog)
    ]
