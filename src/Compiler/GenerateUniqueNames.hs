module Compiler.GenerateUniqueNames where

import Syntax.Common
import Syntax.Raw (RawAtomExpr, RawExpr, RuleClause)
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Numeric.Natural (Natural)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (traverse_)

type Env = State (Map Identifier Natural, Set Identifier)

initState = (M.empty, S.empty)

makeUniqueNames :: [RuleClause] -> [RuleClause]
makeUniqueNames rules = evalState (go rules) initState
  where
    go :: [RuleClause] -> Env [RuleClause]
    go = mapM $ \r -> do
      res <- goRule r
      incState
      return res

    goAtomProp :: Proposition RawAtomExpr -> Env (Proposition RawAtomExpr)
    goAtomProp = traverse $ traverse current

    goProp :: Proposition RawExpr -> Env (Proposition RawExpr)
    goProp = traverse (traverse current)

    goRule :: RuleClause -> Env RuleClause
    goRule (RawRule prems conc) = do
      prems' <- mapM goAtomProp prems
      conc'  <- goProp conc
      return $ RawRule prems' conc'

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