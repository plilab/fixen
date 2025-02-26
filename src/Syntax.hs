{-# LANGUAGE DeriveFunctor #-}
module Syntax where

import Prettyprinter
import Algebra.PartialOrd
import Data.Bifunctor (Bifunctor, bimap)

newtype Program a = Program [Declaration a] deriving (Show)

data Declaration a
  = LatDecl Identifier
  | RelDecl Identifier [TypeExpr]
  | RuleDecl (Maybe Identifier) (Rule (Expr a) (Expr a)) -- should it be from AtomExprs to Expr ??
  | OrdDecl [Fact (Expr a)] (Instantiation a) (Instantiation a)
  | QueryDecl Identifier Identifier [Mode]
  deriving (Show)

data Rule a b = Rule [Fact a] (Fact b) deriving (Show, Functor)

-- actually, I want TNat, TString, TBool for raw literals, and TVar String for any imported/defined stuff e.g. Dist
data TypeExpr = TNat | TString | TBool | TVar Identifier deriving (Show, Eq)

data Instantiation a = Instantiation Identifier [(Identifier, Expr a)] deriving (Show, Eq, Functor)

newtype GroundExpr a = GroundExpr a deriving (Show, Eq, Functor)

data AtomExpr a = Id Identifier | Ground (GroundExpr a) deriving (Show, Eq, Functor)

data Expr a = Atom (AtomExpr a) | App (Expr a) (Expr a) deriving (Show, Eq, Functor)

type Mode = Bool

type Identifier = String

data Fact a = Fact Identifier [a] deriving (Show, Eq, Functor)
-- data Premise = Invocation | Let | Exsts Identifer
-- Invocation = Ident [Ident | Lit]

data Literal = LInt Int | LString String | LBool Bool deriving (Show, Eq)

type LitProgram = Program Literal
type LitDeclaration = Declaration Literal
type LitExpr = Expr Literal
type LitInstantiation = Instantiation Literal

instance Bifunctor Rule where
  bimap f g (Rule lhs rhs) = Rule (map (fmap f) lhs) $ fmap g rhs

{- PartialOrd instances for expressions and proposition -}

instance (PartialOrd a) => PartialOrd (GroundExpr a) where
  leq (GroundExpr a) (GroundExpr b) = leq a b

instance (PartialOrd a) => PartialOrd (AtomExpr a) where
  leq (Id x) (Id y) = x == y
  leq (Ground g) (Ground h) = leq g h
  leq (Ground _) (Id _) = True
  leq _ _ = False

instance (PartialOrd a) => PartialOrd (Expr a) where
  leq (Atom a) (Atom b) = leq a b
  leq a@(App _ _) b@(App _ _) = a == b
  leq _ _ = False

  comparable a b = case (a,b) of
    (Atom _, Atom _) -> True
    (App _ _, App _ _) -> True
    _ -> False

instance (PartialOrd a) => PartialOrd (Fact a) where 
  leq (Fact x es) (Fact y hs) | x == y = and $ zipWith leq es hs
  leq _ _ = False

{- Pretty instances for syntax -}

instance Pretty Literal where
  pretty (LInt n) = pretty n
  pretty (LString s) = pretty s
  pretty (LBool b) = pretty b 

instance (Pretty a) => Pretty (GroundExpr a) where
  pretty (GroundExpr a) = pretty a

instance (Pretty a) => Pretty (AtomExpr a) where
  pretty (Id name)  = pretty name
  pretty (Ground g) = pretty g

instance (Pretty a) => Pretty (Expr a) where
  pretty (Atom a)  = pretty a
  pretty (App f x) = "(" <> go f <+> pretty x <> ")"
    where go (App g y) = go g <+> pretty y
          go e         = pretty e

prettyCommaSep :: [Doc ann] -> Doc ann
prettyCommaSep = hcat . punctuate ", "

prettyProposition :: (Pretty a) => Fact a -> Doc ann
prettyProposition (Fact name args) =
  pretty name <+> hsep (map pretty args)

instance (Pretty a) => Pretty (Instantiation a) where
  pretty (Instantiation name insts) = pretty name <+> "{" <+> (prettyCommaSep . map prettyAssgn) insts <+> "}"
    where prettyAssgn (id_i, expr_i) = pretty id_i <+> "=" <+> pretty expr_i

instance (Pretty a) => Pretty (Declaration a) where
  pretty (LatDecl name) =
    "lat" <+> pretty name
  pretty (RelDecl name types) =
    "rel" <+> pretty name <> (if null types then mempty else ":" <+> prettyCommaSep (map pretty types))
  pretty (RuleDecl name (Rule lhs rhs)) =
    "rule"  <+> maybe mempty ((<> ":") . pretty) name 
            <+> prettyCommaSep (map prettyProposition lhs)
            <> (if null lhs then mempty else space) 
            <> "|-" <+> prettyProposition rhs
  pretty (QueryDecl name ruleName modes) =
    "query" <+> pretty name 
            <+> "as" <+> pretty ruleName 
            <+> hsep (map (\b -> if b then "+" else "-") modes)
  pretty (OrdDecl premises instl instr) = 
    "ord:" <+> prettyCommaSep (map prettyProposition premises)
           <+> "|-" <+> pretty instl <+> pretty instr

instance Pretty TypeExpr where
  pretty TNat  = "Nat"
  pretty TBool = "Bool"
  pretty TString = "String"
  pretty (TVar s) = pretty s

instance (Pretty a) => Pretty (Program a) where
  pretty (Program decls) = vsep $ map pretty decls
