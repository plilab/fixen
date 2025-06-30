module Compiler.Sort ( sortRawProgram ) where

import Data.Functor.Compose
import Syntax.Common
import Syntax.Raw
import Syntax.Sorted
import Control.Monad.State
import Data.Bifunctor (Bifunctor(second))

sortRawProgram :: RawProgram -> Program
sortRawProgram (RawProgram modName decls) = 
  evalState 
    (foldM sortDecl (Program modName [] [] [] [] [] []) decls) 
    (0 :: Int)
  where
    sortDecl prog decl = 
      case decl of
        (Imp i) -> return prog { imports  = i : imports prog }
        (Def d) -> return prog { dataDefs = d : dataDefs prog }
        (Rel r) -> return prog { signatures = sortSignature r : signatures prog }
        (Rul name r) -> let
          mkDefaultName = do
            n <- get
            modify succ
            return $ "Anonymous" ++ show n
          in do
            name' <- maybe mkDefaultName (return . capitalize) name
            return prog { rules = (name', sortRule r) : rules prog }
        (Ord r) -> return prog { priorities = sortPriority r : priorities prog }
        (Qry q) -> return prog { querries = sortQry q : querries prog }

sortSignature :: Signature -> Signature
sortSignature (Signature name typs compl) = Signature (capitalize name) typs compl

{- we wanna sort out whats a rel and whats a function, 
    probably move this to a later pass -}
sortRule :: Rule (PropositionOf e1 Var) (PropositionOf e2 Var)
         -> Rule (PropositionOf e1 Var) (PropositionOf e2 Var)
sortRule (Rule prems concl) = Rule (sortProp <$> prems) (sortProp concl)

sortPriority :: PriorityClause -> PriorityClause
sortPriority (RulePriority r) = RulePriority $ second (fmap sortInstantiation) r
sortPriority (FactPriority r) = FactPriority $ second (fmap sortProp) r

sortProp :: PropositionOf e Var -> PropositionOf e Var
sortProp (Compose (Proposition name args)) = 
  Compose (Proposition (capitalize name) args)

sortInstantiation :: Instantiation -> Instantiation
sortInstantiation (Instantiation rulName binds) = Instantiation (capitalize rulName) binds

sortQry :: ModalDef -> ModalDef
sortQry (ModalDef rel name modes) = ModalDef (capitalize rel) name modes