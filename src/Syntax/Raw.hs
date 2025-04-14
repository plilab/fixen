module Syntax.Raw (
  Declaration(..),
  RawProgram(..),
  RawExpr, RawAtomExpr, RuleClause,
  getDataDef, getSignature, getRule, getOrd, getQuery
) where

import Syntax.Common
import Prettyprinter

data RawProgram = RawProgram { moduleDecl :: Module, unRawProgram :: [Declaration] }
  deriving (Show)

data Declaration
  = Imp Import
  | Def DataDef
  | Rel Signature
  | Rul (Maybe Identifier) RuleClause
  | Ord (OrdClause Identifier)
  | Qry ModalDef
  deriving (Show)

type RawExpr = Expr Identifier
type RawAtomExpr = AtomExpr Identifier

getDataDef :: Declaration -> Maybe DataDef
getDataDef (Def d) = Just d
getDataDef _ = Nothing

getSignature :: Declaration -> Maybe Signature
getSignature (Rel r) = Just r
getSignature _ = Nothing

getRule :: Declaration -> Maybe RuleClause
getRule (Rul _ r) = Just r
getRule _ = Nothing

getOrd :: Declaration -> Maybe (OrdClause Identifier)
getOrd (Ord r) = Just r
getOrd _ = Nothing

getQuery :: Declaration -> Maybe ModalDef
getQuery (Qry r) = Just r
getQuery _ = Nothing

{- Pretty instances for raw syntax -}

instance Pretty Declaration where
  pretty (Imp imp) = pretty imp
  pretty (Def def) = pretty def
  pretty (Rel signature) =
    "rel" <+> pretty signature
  pretty (Rul name rule) =
    "rule"  <> maybe mempty ((space <>) . pretty) name
            <+> "=" <+> pretty rule
  pretty (Qry modDef) =
    "query" <+> pretty modDef
  pretty (Ord rule) = 
    "ord:" <+> pretty rule

instance Pretty RawProgram where
  pretty (RawProgram modName decls) = vsep $ 
    pretty modName : map pretty decls
