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
  , rules      :: [(Maybe Identifier, RuleClause)]
  , ordClauses :: [OrdClause Identifier]
  , querries   :: [ModalDef]
  } deriving (Show)

instance Pretty Program where
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
