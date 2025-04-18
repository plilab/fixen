{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax.Common where

import Algebra.PartialOrd
import Common.Util (alphaEq, foldrFromTraversable, foldlFromTraversable')
import Data.Bifunctor (Bifunctor, bimap)
import Data.Foldable (Foldable(foldl'))
import Data.Hashable (Hashable(hash))
import GHC.Generics (Generic, Generic1)
import Prettyprinter
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Classes (Show1 (liftShowsPrec, liftShowList), Eq1 (liftEq))
import Data.List (intersperse)
import Data.Hashable.Lifted (Hashable1)
import Data.Char (toUpper)

type Identifier = String

type ModuleName = String

type Mode = Bool

data TypeExpr = TNat | TString | TBool | TVar Identifier 
  deriving (Eq)

newtype Module = Module { unModule :: ModuleName }
  deriving (Eq, Show)

newtype Import = Import { unImport :: ModuleName }
  deriving (Eq, Show)

{- data Operator = Op | Ordered | AddBot | AddTop -}

newtype DataDef = Foreign Identifier
  deriving (Eq, Show)
{- | Composed Identifier [Operator] Identifier
   | Defined Identifier [Rule LitExpr LitAtomExpr]

data LatticeDef = 
  { order :: [Rule LitExpr LitAtomExpr]
  , join :: [Rule LitExpr LitAtomExpr]
  , bottom :: LitExpr
  }
-}

data Signature = Signature { relName :: Identifier, paramTypes :: [TypeExpr] }
  deriving (Eq, Show)

data Rule a b = Rule [a] b
  deriving (Show, Functor)

type RuleClause = Rule (Assumption Identifier) (Conclusion Identifier)

data Instantiation = Instantiation Identifier [(Identifier, Expr Identifier)]
  deriving (Show, Eq)

data ModalDef = ModalDef Identifier Identifier [Mode]
  deriving (Eq, Show)

{- TODO: add constructor case:
  Ground = Literal | Con Identifier [Ground] -}
data AtomExpr v = Id v | Ground Literal
  deriving (Show, Eq, Functor, Generic, Generic1)

getId :: AtomExpr a -> Maybe a
getId (Id v) = Just v
getId _      = Nothing

data Expr v = Atom (AtomExpr v) | App Identifier [Expr v] -- App (Expr v) (Expr v)
  deriving (Show, Eq, Functor, Generic)

data Proposition a = Proposition { headSymbol :: Identifier, arguments :: [a] }
  deriving (Show, Eq, Functor, Generic, Generic1)
-- data Premise = Invocation | Let | Exsts Identifer
-- Invocation = Ident [Ident | Lit]

headSymbolOf :: PropositionOf f a -> Identifier
headSymbolOf = headSymbol . getCompose

argumentsOf :: PropositionOf f a -> [f a]
argumentsOf = arguments . getCompose

{- data Premise a
  = Assumption (Proposition (AtomExpr a))
  | Bounded (Proposition (AtomExpr a)) [(a, a)] -}

type PropositionOf = Compose Proposition

type Assumption = PropositionOf AtomExpr
pattern Assumption :: Identifier -> [AtomExpr a] -> Assumption a
pattern Assumption name args = (Compose (Proposition name args))

type Conclusion = PropositionOf Expr
pattern Conclusion :: Identifier -> [Expr a] -> Conclusion a
pattern Conclusion a b = Compose (Proposition a b)

data Literal = LInt Int | LString String | LBool Bool
  deriving (Show, Eq, Generic)

data CVar = First Identifier | Constrained Identifier
  deriving (Show, Eq)

getFirst :: CVar -> Maybe Identifier
getFirst (First x) = Just x
getFirst _         = Nothing

getConstrained :: CVar -> Maybe Identifier
getConstrained (Constrained x) = Just x
getConstrained _               = Nothing

getCVar :: CVar -> Identifier
getCVar (First x) = x
getCVar (Constrained x) = x

liftCVar :: (Identifier -> Identifier) -> CVar -> CVar
liftCVar f (First x) = First $ f x
liftCVar f (Constrained x) = Constrained $ f x 

getAtomName :: AtomExpr CVar -> Maybe Identifier
getAtomName = fmap getCVar . getId

data CAssumption a = CAssumption { 
  assumption :: Assumption a,
  constraints :: [(Identifier, Identifier)]
  } deriving (Show, Functor)

data OrdHead = OrdHead Instantiation Instantiation
  deriving (Show, Eq)

type OrdClause v = Rule (Assumption v) OrdHead

instance Bifunctor Rule where
  bimap f g (Rule lhs rhs) = Rule (map f lhs) $ g rhs

{- can make them proper traversables later -}
instance Foldable Proposition where
  foldr f e (Proposition _ args) = foldr f e args

instance Traversable Proposition where
  --traverse :: (Monad m) => (a -> m b) -> Proposition a -> m (Proposition b)
  traverse f (Proposition name args) = Proposition name <$> traverse f args

instance Hashable a => Hashable (Proposition a)

instance Foldable CAssumption where
  foldr f z (CAssumption assump _) = foldr f z assump

instance Traversable CAssumption where
  traverse f (CAssumption assump eqs) =
    CAssumption <$> traverse f assump <*> pure eqs

instance Foldable AtomExpr where
  foldr f e aex = case aex of
    (Id a) -> f a e
    _      -> e

instance Traversable AtomExpr where
  --traverseAtomExpr :: (Monad m) => (a -> m b) -> AtomExpr a -> m (AtomExpr b)
  traverse f aex = case aex of
    (Id a)     -> Id <$> f a
    (Ground l) -> pure $ Ground l

instance Hashable a => Hashable (AtomExpr a)

instance Foldable Expr where
  -- collects variables left to right into a list and folds over it
  foldr = foldrFromTraversable
  foldl' = foldlFromTraversable'

instance Traversable Expr where
  traverse f ex = case ex of
    (App fun args) -> App fun <$> (traverse . traverse) f args
    (Atom atm)    -> Atom <$> traverse f atm

instance Hashable a => Hashable (Expr a)

instance Hashable Literal where
  hash (LInt lint) = hash lint
  hash (LBool lbool) = hash lbool
  hash (LString lstr) = hash lstr 

{- PartialOrd instances for expressions and propositions -}

{- TBD
instance (PartialOrd a) => PartialOrd (AtomExpr a) where
  leq (Id x) (Id y) = leq x y
  leq (Ground g) (Ground h) = g == h
  leq (Ground _) (Id _) = True -- what about when Id is constrained?
  leq _ _ = False

instance (PartialOrd a) => PartialOrd (Expr a) where
  leq (Atom a) (Atom b) = leq a b
  leq a@(App _ _) b@(App _ _) = a == b
  leq _ _ = False

  comparable a b = case (a,b) of
    (Atom _, Atom _) -> True
    (App _ _, App _ _) -> True
    _ -> False
-}

instance (PartialOrd a) => PartialOrd (Proposition a) where 
  leq (Proposition x es) (Proposition y hs) | x == y = and $ zipWith leq es hs
  leq _ _ = False

instance Show1 Proposition where
  liftShowsPrec _ sl d (Proposition sym args) = 
    showParen (d > 10) $
      showString sym .
      showString " " .
      sl args

instance Eq1 Proposition where
  liftEq eq (Proposition name1 args1) (Proposition name2 args2) = name1 == name2 && and (zipWith eq args1 args2)

instance Hashable1 Proposition

instance Show1 AtomExpr where
  liftShowsPrec sp _ d (Id name) = sp d name
  liftShowsPrec _ _ _ (Ground lit) = showString $ show lit

instance Eq1 AtomExpr where
  liftEq eq (Id x) (Id y) = eq x y
  liftEq _ (Ground l1) (Ground l2) = l1 == l2
  liftEq _ _ _ = False

instance Hashable1 AtomExpr

instance Show1 Expr where
  liftShowsPrec sp ss d (Atom a) = liftShowsPrec sp ss d a
  liftShowsPrec sp ss _ (App fun args) = 
    showString "(" . 
    showString fun . 
    showString " " . 
    liftShowList sp ss args . 
    showString ")"

instance Show TypeExpr where
  show TString  = "String"
  show TBool    = "Bool"
  show TNat     = "Natural"
  show (TVar t) = t  

{- pretty instances for common syntax -}
instance (Pretty (f (g a))) => Pretty (Compose f g a) where
  pretty = pretty . getCompose

instance Pretty Literal where
  pretty (LInt n) = pretty n
  pretty (LString s) = pretty s
  pretty (LBool b) = pretty b 

instance (Pretty a) => Pretty (AtomExpr a) where
  pretty (Id name)  = pretty name
  pretty (Ground g) = pretty g

instance (Pretty a) => Pretty (Expr a) where
  pretty (Atom a)  = pretty a
  pretty (App f args) = "(" <> pretty f <+> hsep (pretty <$> args) <> ")"

instance Pretty CVar where
  pretty (First x) = pretty x
  pretty (Constrained x) = pretty x <> "!"

instance (Pretty a) => Pretty (Proposition a) where
--prettyProposition :: (Pretty a) => Proposition a -> Doc ann
  pretty (Proposition name args) =
    pretty name <+> hsep (map pretty args)

instance (Pretty a) => Pretty (CAssumption a) where
  pretty (CAssumption assump eqs) = 
    if null eqs then 
      pretty assump 
    else
      "(" <> pretty assump <> ")" 
      <> "[" <> prettyCommaSep (prettyEq <$> eqs) <> "]"
    where
      prettyEq (id1, id2) = pretty id1 <+> "=" <+> pretty id2

prettyCommaSep :: [Doc ann] -> Doc ann
prettyCommaSep = hcat . punctuate ", "

instance Pretty TypeExpr where
  pretty TNat  = "Nat"
  pretty TBool = "Bool"
  pretty TString = "String"
  pretty (TVar s) = pretty s

instance Pretty Instantiation where
  pretty (Instantiation name insts) = pretty name <+> "{" <+> (prettyCommaSep . map prettyAssgn) insts <+> "}"
    where prettyAssgn (id_i, expr_i) = pretty id_i <+> "=" <+> pretty expr_i

instance Pretty Module where
  pretty (Module modName) = 
    "module" <+> pretty modName <+> "where"

instance Pretty Import where
  pretty (Import modName) =
    "import" <+> pretty modName

instance Pretty DataDef where
  pretty (Foreign name) =
    "data" <+> pretty name

instance Pretty Signature where 
  pretty (Signature name types) =
    pretty name <> 
      if null types 
      then mempty 
      else ":" <+> prettyCommaSep (map pretty types)

instance (Pretty a, Pretty b) => Pretty (Rule a b) where
  pretty (Rule lhs rhs) =
    prettyCommaSep (map pretty lhs)
      <> (if null lhs then mempty else space) 
      <> "|-" <+> pretty rhs

instance Pretty ModalDef where
  pretty (ModalDef name ruleName modes) =
    pretty name 
    <+> "as" <+> pretty ruleName 
    <+> hsep (map (\b -> if b then "+" else "-") modes)

instance Pretty OrdHead where
  pretty (OrdHead instl instr) = pretty instl <+> "<=" <+> pretty instr

{- newtype AlphaClass a = Alpha a

instance (Eq a, Ord a) => Eq (AlphaClass a) where
  Alpha a1 == Alpha a2 = alphaEq a1 a2 -}

newtype AlphaExpr a = AlphaExpr { unAlphaExpr :: Expr a } deriving (Functor, Generic)
newtype AlphaAtomExpr a = AlphaAtomExpr { unAlphaAtomExpr :: AtomExpr a } deriving (Functor, Generic)
newtype AlphaProp a = AlphaProp { unAlphaProp :: Proposition a } deriving (Functor, Generic)

instance Hashable a => Eq (AlphaExpr a) where
  (AlphaExpr x) == (AlphaExpr y) = alphaEq x y

instance (Hashable a) => Hashable (AlphaExpr a)

instance Hashable a => Eq (AlphaAtomExpr a) where
  (AlphaAtomExpr x) == (AlphaAtomExpr y) = alphaEq x y

instance Hashable a => Hashable (AlphaAtomExpr a)

instance Hashable a => Eq (AlphaProp a) where
  (AlphaProp x) == (AlphaProp y) = alphaEq x y

instance Hashable a => Hashable (AlphaProp a)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

{- class Alpha a where
  type AlphaRep a
  alpha :: a -> AlphaRep a
  unAlpha :: AlphaRep a -> a

instance Alpha (Expr a) where
  type AlphaRep (Expr a) = AlphaExpr a
  alpha = AlphaExpr
  unAlpha = unAlphaExpr

instance Alpha (AtomExpr a) where
  type AlphaRep (AtomExpr a) = AlphaAtomExpr a
  alpha = AlphaAtomExpr
  unAlpha = unAlphaAtomExpr

instance Alpha (Proposition a) where
  type AlphaRep (Proposition a) = AlphaProp a
  alpha = AlphaProp
  unAlpha = unAlphaProp -}
