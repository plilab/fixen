module Syntax.Sorted (
  Program(..)
) where

import Syntax.Common
import Prettyprinter

data Program = Program 
  { moduleDecl :: Module
  , imports    :: [Import]
  , dataDefs   :: [DataDef]
  , signatures :: [Signature]
  , rules      :: [(Identifier, RuleClause)]
  , priorities :: [PriorityClause]
  , querries   :: [ModalDef]
  } deriving (Show)

{- type PriorityProgram = Program (Either (FactOrdClause Var) (PriorityClause Var))
type OrderingProgram = Program (FactOrdClause Var) -}

instance Pretty Program where
  pretty program = vsep
    [ pretty (moduleDecl program)
    , vsep . map pretty $ imports program
    , vsep . map pretty $ dataDefs program
    , "rels" <+> align (vsep . map pretty $ signatures program)
    , "rules" <+> align (vsep . map prettyPair $ rules program)
    , "ords" <+> align (vsep . map pretty $ priorities program)
    , "queries" <+> align (vsep . map pretty $ querries program)
    ]
    where prettyPair (name, rule) = pretty name <+> "=" <+> pretty rule
