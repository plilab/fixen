module Compiler.GenerateUniqueNames ( generateUniqueNames ) where

import Syntax.Common
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Numeric.Natural (Natural)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (traverse_)
import Syntax.Sorted (Program (rules))

type Count = Natural

type Env = State (HashMap Var Count, HashSet Var)

initState :: (HashMap k a1, HashSet a2)
initState = (M.empty, S.empty)

generateUniqueNames :: Program -> Program
generateUniqueNames prog = prog { rules = makeUniqueNames $ rules prog  }

makeUniqueNames :: [(Maybe Identifier, RuleClause)] -> [(Maybe Identifier, RuleClause)]
makeUniqueNames ruls = evalState (go ruls) initState
  where
    go :: [(Maybe Identifier, RuleClause)] -> Env [(Maybe Identifier, RuleClause)]
    go = mapM $ \(name, r) -> do
      res <- goRule r
      incState
      return (name, res)

    goAtomProp :: Assumption Var -> Env (Assumption Var)
    goAtomProp = traverse current

    goProp :: Conclusion Var -> Env (Conclusion Var)
    goProp = traverse current

    goRule :: RuleClause -> Env RuleClause
    goRule (Rule prems conc) = do
      prems' <- mapM goAtomProp prems
      conc'  <- goProp conc
      return $ Rule prems' conc'

countOf :: Var -> Env Count
countOf name = gets (maybe 0 succ . M.lookup name . fst)

incCount :: Var -> Env ()
incCount name = modify (first $ M.alter (Just . maybe 0 succ) name)

current :: Var -> Env Var
current name = do
  modify (second $ S.insert name)
  count <- countOf name
  return $ Variable (unVariable name ++ show count)

incState :: Env ()
incState = do
  names <- gets snd
  res <- traverse_ incCount names
  modify (second $ const S.empty)
  return res