{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Syntax where

import Prettyprinter
import Prettyprinter.Render.Text
import Data.Text.IO as T

newtype Program = Program [Declaration] deriving (Show)

data Declaration
  = LatDecl Identifier
  | RelDecl Identifier [TypeExpr]
  | RuleDecl (Maybe Identifier) Rule
  | OrdDecl [Proposition] Instantiation Instantiation
  | QueryDecl Identifier Identifier [Mode]
  deriving (Show)

data Rule = Rule [Proposition] Proposition deriving (Show)

data TypeExpr = TNat | TDist deriving Show

data Instantiation = Instantiation Identifier [(Identifier, Expr)] deriving (Show)

data AtomExpr = Id Identifier | Int Int deriving (Show)

data Expr = Atom AtomExpr | App Expr Expr deriving (Show)

type Mode = Bool

type Identifier = String

type Proposition = (Identifier, [Expr])
-- data Premise = Invocation | Let | Exsts Identifer
-- Invocation = Ident [Ident | Lit]

instance Pretty AtomExpr where
  pretty (Id name) = pretty name
  pretty (Int n)   = pretty n

instance Pretty Expr where
  pretty (Atom a) = pretty a
  pretty (App f x) = "(" <> go f <+> pretty x <> ")"
    where go (App g y) = go g <+> pretty y
          go e         = pretty e

prettyCommaSep :: [Doc ann] -> Doc ann
prettyCommaSep = hcat . punctuate ", "

prettyProposition :: Proposition -> Doc ann
prettyProposition (name, args) =
  pretty name <+> hsep (map pretty args)

prettyInstantiation :: Instantiation -> Doc ann
prettyInstantiation (Instantiation name insts) = pretty name <+> "{" <+> (prettyCommaSep . map prettyAssgn) insts <+> "}"
  where prettyAssgn (id_i, expr_i) = pretty id_i <+> "=" <+> pretty expr_i

instance Pretty Declaration where
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
           <+> "|-" <+> prettyInstantiation instl <+> prettyInstantiation instr

instance Pretty TypeExpr where
  pretty :: TypeExpr -> Doc ann
  pretty TNat  = "Nat"
  pretty TDist = "Dist"

instance Pretty Program where
  pretty :: Program -> Doc ann
  pretty (Program decls) = vsep $ map pretty decls

-- Render a document as Text
renderPretty :: Doc ann -> IO ()
renderPretty = T.putStrLn . renderStrict . layoutPretty defaultLayoutOptions
