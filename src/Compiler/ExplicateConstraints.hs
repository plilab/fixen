module Compiler.ExplicateConstraints (
  explicateConstraints
) where

import Syntax.Compact
import Syntax.Common
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.Reader (Reader, runReader, local, asks)
import Control.Monad.State (State, evalState, modify, gets, unless, runState)
import Data.Bifunctor (Bifunctor(first, second))

explicateConstraints :: ImplicitCompactProgram -> ExplicitCompactProgram
explicateConstraints program = program { ruleForest = explicateForest $ ruleForest program }

explicateForest :: ImplicitRuleForest -> ExplicitRuleForest
explicateForest (RF trees) =
  RF $ runReader (explicateTrees trees) S.empty

type ReaderIds = Reader (S.Set Identifier)
type StateIds = State (S.Set Identifier)

explicateTrees :: [ImplicitRuleTree] -> ReaderIds [ExplicitRuleTree]
explicateTrees = mapM go
  where
    go :: ImplicitRuleTree -> ReaderIds ExplicitRuleTree
    go tree = case tree of
      (Result cs) -> Result <$> explicateConclusion cs
      (Branch p ts) -> do
        (p', seenVars) <- asks . runState $ explicateAssumption p
        ts' <- local (const seenVars) $ mapM go ts
        return $ Branch p' ts'
    
    -- switch to state to maintain dependency between variables within each proposition
    explicateConclusion = asks . evalState . mapM explicateId
    
--    explicateAssumption = asks . runState . mapM explicateId
explicateAssumption :: Assumption Identifier -> StateIds (CAssumption CVar)
explicateAssumption assump = do
  -- explicate external dependencies
  assump' <- mapM explicateId assump
  -- explicate internal dependencies
  let (assump'', (_, eqs)) = runState (mapM explicateEqualities assump') (M.empty, [])
  return $ CAssumption assump'' eqs

explicateId :: Identifier -> StateIds CVar
explicateId name = do
  seen <- gets (S.member name)
  unless seen $ modify (S.insert name)
  return $ 
    (if seen then Constrained else First) name

-- only matters for variables which occur First in current premise!!
explicateEqualities :: CVar -> State (M.Map Identifier Int, [(Identifier, Identifier)]) CVar
explicateEqualities (First v) = do
  modify (first $ M.insert v 1)
  return $ First v
explicateEqualities (Constrained v) = do
  count <- gets (M.findWithDefault 0 v . fst)
  -- if the first occurence is not from this premise then ignore
  if count == 0 then 
    return $ Constrained v
  -- else concoct a fresh variable and make a new equation
  else do
    let v' = v ++ replicate count '\''
    modify (first $ M.adjust succ v)
    modify $ second ((v, v'):)
    return $ First v'