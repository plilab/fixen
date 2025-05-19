{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Syntax.Common where

import Algebra.PartialOrd
import Common.Util (foldrFromTraversable, foldlFromTraversable', toDBLvl)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Foldable (Foldable(foldl'))
import Data.Hashable (Hashable(hash, hashWithSalt))
import GHC.Generics (Generic, Generic1)
import Prettyprinter
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Classes (Show1 (liftShowsPrec, liftShowList), Eq1 (liftEq))
import Data.Hashable.Lifted (Hashable1)
import Data.Char (toUpper)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import GHC.Natural (Natural)

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

type RuleClause = Rule (Assumption Var) (Conclusion Var)

data Instantiation = Instantiation Identifier [(Identifier, Expr Var)]
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

data Literal = LInt Int | LString String | LBool Bool | LCons Identifier
  deriving (Show, Eq, Generic)

newtype Variable a = Variable {unVariable :: a}
  deriving (Show, Eq, Generic, Functor)

type Var = Variable Identifier

data FreezableVar a = Frozen Identifier | Unfrozen a
  deriving (Show, Eq)

data ConstrVar a = First a | Constrained a
  deriving (Show, Eq)

type CVar = ConstrVar Identifier

getFirst :: ConstrVar a -> Maybe a
getFirst (First x) = Just x
getFirst _         = Nothing

getConstrained :: ConstrVar a -> Maybe a
getConstrained (Constrained x) = Just x
getConstrained _               = Nothing

getCVar :: ConstrVar a -> a
getCVar (First x) = x
getCVar (Constrained x) = x

liftCVar :: (a -> a) -> ConstrVar a -> ConstrVar a
liftCVar f (First x) = First $ f x
liftCVar f (Constrained x) = Constrained $ f x 

getAtomName :: AtomExpr (ConstrVar a) -> Maybe a
getAtomName = fmap getCVar . getId

type Constraints = M.HashMap Identifier (S.HashSet Identifier)

data CAssumption a = CAssumption { 
  assumption :: Assumption a,
  constraints :: Constraints
  } deriving (Show, Functor)

data RuleOrdHead = RuleOrdHead Instantiation Instantiation
  deriving (Show, Eq)

type PriorityClause v = Rule (Assumption v) RuleOrdHead

data FactOrdHead = FactOrdHead (Proposition Identifier) (Proposition Identifier)

type FactOrdClause v = Rule (Assumption v) FactOrdHead

instance Bifunctor Rule where
  bimap f g (Rule lhs rhs) = Rule (map f lhs) $ g rhs

{- can make them proper traversables later -}
instance Foldable Proposition where
  foldr f e (Proposition _ args) = foldr f e args

instance Traversable Proposition where
  --traverse :: (Monad m) => (a -> m b) -> Proposition a -> m (Proposition b)
  traverse f (Proposition name args) = Proposition name <$> traverse f args

instance Hashable a => Hashable (Proposition a)

instance Hashable a => Hashable (Variable a) where
  hash = hash . unVariable

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
  hash (LCons lcons) = hashWithSalt (hash lcons) ("LCons" :: String)

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
  pretty (LCons c) = pretty c

instance (Pretty a) => Pretty (AtomExpr a) where
  pretty (Id name)  = pretty name
  pretty (Ground g) = pretty g

instance (Pretty a) => Pretty (Expr a) where
  pretty (Atom a)  = pretty a
  pretty (App f args) = "(" <> pretty f <+> hsep (pretty <$> args) <> ")"

instance (Pretty a) => Pretty (Variable a) where
  pretty (Variable x) = pretty x

instance (Pretty a) => Pretty (ConstrVar a) where
  pretty (First x) = pretty x
  pretty (Constrained x) = pretty x <> "!"

instance (Pretty a) => Pretty (Proposition a) where
--prettyProposition :: (Pretty a) => Proposition a -> Doc ann
  pretty (Proposition name args) =
    pretty name <+> hsep (map pretty args)

instance (Pretty a) => Pretty (CAssumption a) where
  pretty (CAssumption assump eqs) = 
    if M.null eqs then 
      pretty assump 
    else
      "(" <> pretty assump <> ")" 
      <> "[" <> prettyEqs eqs <> "]"
    where
      prettyEqs = 
        prettyCommaSep . map (hcat . punctuate " = " . map pretty . S.toList) . M.elems

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

instance Pretty RuleOrdHead where
  pretty (RuleOrdHead instl instr) = pretty instl <+> "<=" <+> pretty instr

{- wrapper mapping constructs to their alpha equivalence class -}
newtype Alpha f a = Alpha { unAlpha :: f a } deriving (Generic, Functor)

instance (Traversable f, Eq (f Natural), Hashable a) => Eq (Alpha f a) where
  (Alpha x) == (Alpha y) = toDBLvl x == toDBLvl y

instance (Traversable f, Hashable (f a), Hashable (f Natural), Hashable a) => Hashable (Alpha f a) where
  hash = hash . toDBLvl . unAlpha

instance Show (f a) => Show (Alpha f a) where
  show = show . unAlpha

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
