module Compiler.GenerateUniqueNames ( generateUniqueNames ) where

import Syntax.Common
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Numeric.Natural (Natural)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Bifunctor (Bifunctor(first, second))
import Data.Foldable (traverse_, Foldable (toList))
import qualified Syntax.Sorted as P (Program (rules, priorities))

type Count = Natural

data StateData = StateData
  { countMap    :: HashMap Var Count
  , varsInScope :: HashSet Var
  , varMapping  :: HashMap Var Var
  , priorities  :: [PriorityClause]
  }

type Env = State StateData

initState :: P.Program -> StateData
initState = StateData M.empty S.empty M.empty . P.priorities

generateUniqueNames :: P.Program -> P.Program
generateUniqueNames prog = let
  (rules', st) = runState (makeUniqueNames $ P.rules prog) (initState prog)
  in prog { P.rules = rules', P.priorities = priorities st }

makeUniqueNames :: [(Identifier, RuleClause)] -> Env [(Identifier, RuleClause)]
makeUniqueNames =
  mapM $ \(name, r) -> do
    res <- goRule r
    adjustPriorityClauses name
    incState
    return (name, res)
  where
    goRule :: RuleClause -> Env RuleClause
    goRule (Rule prems conc) = do
      prems' <- mapM (traverse current) prems
      conc'  <- traverse current conc
      let varMap = M.fromList $ zip
            (concatMap toList prems  ++ toList conc)
            (concatMap toList prems' ++ toList conc')
      modify (\st -> st { varMapping = varMap })
      return $ Rule prems' conc'

adjustPriorityClauses :: Identifier -> Env ()
adjustPriorityClauses rulName = do
  prs <- gets priorities
  varMap <- gets varMapping
  let
    adjust (RulePriority r) = RulePriority $ second (fmap $ mapBound varMap) r
    adjust f = f
    prs' = map adjust prs
  modify (\st -> st { priorities = prs' })
  where
    mapBound varMap (Instantiation name binds) | rulName == name =
      Instantiation name $ map (first $ \v -> M.lookupDefault v v varMap) binds
    mapBound _ inst = inst

countOf :: Var -> Env Count
countOf name = gets (maybe 0 succ . M.lookup name . countMap)

incCount :: Var -> Env ()
incCount name = modify $ \st ->
  st { countMap = M.alter (Just . maybe 0 succ) name (countMap st) }

current :: Var -> Env Var
current name = do
  modify (\st -> st { varsInScope = S.insert name (varsInScope st) })
  count <- countOf name
  return $ Variable (unVariable name ++ show count)

incState :: Env ()
incState = do
  names <- gets varsInScope
  res <- traverse_ incCount names
  modify (\st -> st { varsInScope = S.empty, varMapping = M.empty })
  return res