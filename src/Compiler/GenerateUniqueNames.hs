module Compiler.GenerateUniqueNames ( generateUniqueNames ) where

import Syntax.Common
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Numeric.Natural (Natural)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (traverse_)
import Syntax.Sorted (Program (rules))

type Env = State (Map Identifier Natural, Set Identifier)

initState :: (Map k a1, Set a2)
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

    goAtomProp :: Assumption Identifier -> Env (Assumption Identifier)
    goAtomProp = traverse current

    goProp :: Conclusion Identifier -> Env (Conclusion Identifier)
    goProp = traverse current

    goRule :: RuleClause -> Env RuleClause
    goRule (Rule prems conc) = do
      prems' <- mapM goAtomProp prems
      conc'  <- goProp conc
      return $ Rule prems' conc'

countOf :: Identifier -> Env Natural
countOf name = gets (maybe 0 succ . M.lookup name . fst)

incCount :: Identifier -> Env ()
incCount name = modify (first $ M.alter (Just . maybe 0 succ) name)

current :: Identifier -> Env Identifier
current name = do
  modify (second $ S.insert name)
  (name ++) . show <$> countOf name

incState :: Env ()
incState = do
  names <- gets snd
  res <- traverse_ incCount names
  modify (second $ const S.empty)
  return res