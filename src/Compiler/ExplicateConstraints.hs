module Compiler.ExplicateConstraints (
  explicateConstraints
) where

import Syntax.Compact
import Syntax.Common
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Control.Monad.Reader (Reader, runReader, local, asks, MonadReader (ask))
import Control.Monad.State (State, modify, gets, unless, runState)

explicateConstraints :: ImplicitCompactProgram -> ExplicitCompactProgram
explicateConstraints program = 
  program { ruleForest = explicateForest $ ruleForest program }

explicateForest :: ImplicitRuleForest -> ExplicitRuleForest
explicateForest (RF trees) =
  RF $ runReader (explicateTrees trees) S.empty

type ExplicationReader = Reader (S.HashSet Var)
type ExplicationState = State (S.HashSet Var)

explicateTrees :: [ImplicitRuleTree] -> ExplicationReader [ExplicitRuleTree]
explicateTrees = mapM go
  where
    go :: ImplicitRuleTree -> ExplicationReader ExplicitRuleTree
    go tree = case tree of
      (Result cs) -> do
        bound <- ask
        let unbound = filter (not . (`S.member` bound)) (arguments cs)
        if null unbound
        then return $ Result cs
        else error ("some variables are unbound: " ++ show unbound)
      (Branch p ts) -> do
        -- switch to state to maintain dependency between variables within each proposition
        (p', seenVars) <- asks . runState $ explicateAssumption p
        ts' <- local (const seenVars) $ mapM go ts
        return $ Branch p' ts'

explicateAssumption :: Assumption Var -> ExplicationState (CAssumption CVar)
explicateAssumption assump = do
  -- explicate external dependencies
  assump' <- mapM explicateId assump
  -- explicate internal dependencies
  let (assump'', eqs) = runState (mapM explicateEqualities assump') M.empty
  -- clean up the eqs: remove singleton mappings
      eqs' = M.filter ((> 1) . S.size) eqs
  return $ CAssumption assump'' eqs'

explicateId :: Var -> ExplicationState CVar
explicateId name = do
  seen <- gets (S.member name)
  unless seen $ modify (S.insert name)
  return $
    (if seen then Constrained else First) (unVariable name)

-- only matters for variables which occur First in current premise!!
explicateEqualities :: CVar -> State Constraints CVar
explicateEqualities (First v) = do
  modify $ M.insert v (S.singleton v)
  return $ First v
explicateEqualities (Constrained v) = do
  count <- gets (S.size . M.findWithDefault S.empty v)
  -- if the first occurence is not from this premise then ignore
  if count == 0 then
    return $ Constrained v
  -- else concoct a fresh variable and make a new equation
  else do
    let v' = v ++ replicate count '\''
    modify (M.adjust (S.insert v') v)
    return $ First v'