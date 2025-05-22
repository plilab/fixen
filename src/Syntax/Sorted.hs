module Syntax.Sorted (
  Program(..),
  PriorityProgram,
  OrderingProgram
) where

import Syntax.Common
import Prettyprinter

data Program ord = Program 
  { moduleDecl :: Module
  , imports    :: [Import]
  , dataDefs   :: [DataDef]
  , signatures :: [Signature]
  , rules      :: [(Maybe Identifier, RuleClause)]
  , ordClauses :: [ord]
  , querries   :: [ModalDef]
  } deriving (Show)

type PriorityProgram = Program (PriorityClause Var)
type OrderingProgram = Program (FactOrdClause Var)

instance (Pretty ord) => Pretty (Program ord) where
  pretty program = vsep
    [ pretty (moduleDecl program)
    , vsep . map pretty $ imports program
    , vsep . map pretty $ dataDefs program
    , "rels" <+> align (vsep . map pretty $ signatures program)
    , "rules" <+> align (vsep . map prettyPair $ rules program)
    , "ords" <+> align (vsep . map pretty $ ordClauses program)
    , "queries" <+> align (vsep . map pretty $ querries program)
    ]
    where prettyPair (name, rule) = maybe "_" pretty name <+> "=" <+> pretty rule
