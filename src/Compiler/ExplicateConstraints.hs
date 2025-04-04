module Compiler.ExplicateConstraints (
  explicateConstraints
) where

import Syntax.Compact
import Syntax.Common
import Data.Bifunctor
import Data.Functor ((<&>))
import qualified Data.Set as S
import Control.Monad.Reader (Reader, runReader, local, asks)
import Control.Monad.State (State, evalState, modify, gets, unless, runState)
import Data.Foldable (Foldable(foldl'))

explicateConstraints :: CompactProgram Identifier -> CompactProgram CVar
explicateConstraints program = program 
  { ruleForest = explicateForest $ ruleForest program
    -- just make all variables `First`; hacky workaround
  , ordClauses = map (first $ fmap First) $ ordClauses program
  }
  where

  explicateForest :: RuleForest Identifier -> RuleForest CVar
  explicateForest (RF forest) = 
    let 
      explicatedForest = forest <&> 
        \(prop, trees) -> (First <$> prop, explicateTrees prop trees)  
    in RF explicatedForest

  explicateTrees :: Assumption Identifier -> [RuleTree Identifier] -> [RuleTree CVar]
  explicateTrees prop = 
    map $ \tree -> runReader (go tree) (collectVars prop)
    where
      collectVars :: (Traversable expr) => PropositionOf expr Identifier -> S.Set Identifier
      collectVars = foldl' (flip S.insert) S.empty

      go :: RuleTree Identifier -> Reader (S.Set Identifier) (RuleTree CVar)
      go (RT tree) = RT <$> case tree of
        (Result cs) -> Result <$> mapM explicateConclusion cs
        (Branch p ts) -> do
          (p', seenVars) <- explicateAssumption p
          ts' <- local (const seenVars) $ mapM go ts
          return $ Branch p' ts'
      
      -- switch to state to maintain dependency between variables within each proposition
      explicateAssumption = asks . runState . mapM explicateId
      explicateConclusion = asks . evalState . mapM explicateId

      explicateId :: Identifier -> State (S.Set Identifier) CVar
      explicateId name = do
        seen <- gets (S.member name)
        unless seen $ modify (S.insert name)
        return $ 
          (if seen then Constrained else First) name