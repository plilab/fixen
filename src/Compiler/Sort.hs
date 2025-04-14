module Compiler.Sort ( sortRawProgram ) where

import Data.Functor.Compose
import Syntax.Common
import Syntax.Raw
import Syntax.Sorted

sortRawProgram :: RawProgram -> Program
sortRawProgram (RawProgram modName decls) = foldl sortDecl (Program modName [] [] [] [] [] []) decls
  where
    sortDecl prog decl = case decl of
      (Imp i) -> prog { imports  = i : imports prog }
      (Def d) -> prog { dataDefs = d : dataDefs prog }
      (Rel r) -> prog { signatures = sortSignature r : signatures prog }
      (Rul name r) -> prog { rules = (name, sortRule r) : rules prog }
      (Ord r) -> prog { ordClauses = r : ordClauses prog }
      (Qry q) -> prog { querries = sortQry q : querries prog }

sortSignature :: Signature -> Signature
sortSignature (Signature name typs) = Signature (capitalize name) typs

{- we wanna sort out whats a rel and whats a function, 
    probably move this to a later pass -}
sortRule :: RuleClause -> RuleClause
sortRule (Rule prems concl) = Rule (sortProp <$> prems) (sortProp concl)

sortProp :: PropositionOf e Identifier -> PropositionOf e Identifier
sortProp (Compose (Proposition name args)) = 
  Compose (Proposition (capitalize name) args)

sortQry :: ModalDef -> ModalDef
sortQry (ModalDef rel name modes) = ModalDef (capitalize rel) name modes