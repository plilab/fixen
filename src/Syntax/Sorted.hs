module Syntax.Sorted where

import Syntax.Common
import Syntax.Raw
import Prettyprinter

data Program = Program 
  { dataDefs   :: [DataDef]
  , signatures :: [Signature]
  , rules      :: [(Maybe Identifier, RuleClause)]
  , ordClauses :: [OrdClause Identifier]
  , querries   :: [ModalDef]
  } deriving (Show)

sortRawProgram :: RawProgram -> Program
sortRawProgram (RawProgram decls) = foldl sortDecl (Program [] [] [] [] []) decls
  where
    sortDecl prog decl = case decl of
      (Def d) -> prog { dataDefs = d : dataDefs prog }
      (Rel r) -> prog { signatures = r : signatures prog }
      (Rul name r) -> prog { rules = (name, r) : rules prog }
      (Ord r) -> prog { ordClauses = r : ordClauses prog }
      (Qry q) -> prog { querries = q : querries prog }

instance Pretty Program where
  pretty program = vsep
    [ "data" <+> align (vsep . map pretty $ dataDefs program)
    , "rels" <+> align (vsep . map pretty $ signatures program)
    , "rules" <+> align (vsep . map prettyPair $ rules program)
    , "ords" <+> align (vsep . map pretty $ ordClauses program)
    , "queries" <+> align (vsep . map pretty $ querries program)
    ]
    where prettyPair (name, rule) = maybe "_" pretty name <+> "=" <+> pretty rule
