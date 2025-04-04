module Compiler.ExplicateConstraints (
  explicateConstraints
) where

import Syntax.Compact
import Syntax.Common
import Data.Bifunctor
import Data.Functor ((<&>))
import Data.Traversable (forM)
import qualified Data.Set as S
import Control.Monad.Reader (Reader, runReader, local, asks)
import Control.Monad.State (State, evalState, modify, gets, unless)
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
      explicateTrees :: Assumption Identifier -> [RuleTree Identifier] -> [RuleTree CVar]
      explicateTrees prop = 
        map $ \tree -> runReader (go tree) vars
        where
          -- collected variables ocurring in prop
          vars = foldMap (foldl' (flip S.insert) S.empty) prop

          go :: RuleTree Identifier -> Reader (S.Set Identifier) (RuleTree CVar)
          go (RT tree) = RT <$> case tree of
            (Result cs) -> Result <$> mapM explicateProp cs
            (Branch p ts) -> do
              p'  <- explicateProp p
              ts' <- local (S.insert $ headSymbol p) $ mapM go ts
              return $ Branch p' ts'

          explicateId :: Identifier -> State (S.Set Identifier) CVar
          explicateId name = do
            seen <- gets (S.member name)
            unless seen $ modify (S.insert name)
            return $ 
              (if seen then Constrained else First) name
          
          -- switch to state to maintain dependency between variables within each proposition
          explicateProp :: (Traversable expr) =>
            Proposition (expr Identifier) -> Reader (S.Set Identifier) (Proposition (expr CVar))
          explicateProp prop = asks (evalState explicate)
            where explicate = forM prop (mapM explicateId)

      explicatedForest = forest <&> 
        \(prop, trees) -> (fmap First <$> prop, explicateTrees prop trees)
      
      in RF explicatedForest