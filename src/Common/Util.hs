{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common.Util where

import Algebra.PartialOrd (PartialOrd, leq)
import Control.Monad.State
import Data.Traversable (for)
import qualified Data.HashMap.Strict as M
import Numeric.Natural (Natural)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(foldl'))

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

lt :: (PartialOrd a) => a -> a -> Bool
lt x y = leq x y && not (leq y x)

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes = flip lt

equiv :: (PartialOrd a) => a -> a -> Bool
equiv x y = leq x y && leq y x

toDBLvl :: (Hashable a, Traversable t) => t a -> t Natural
toDBLvl t = evalState go M.empty
  where
    go = for t $ \a -> do
      tbl <- get
      if M.member a tbl then
        return $ tbl M.! a
      else do
        let lvl = fromIntegral $ M.size tbl
        modify (M.insert a lvl)
        return lvl

alphaEq :: (Hashable a, Traversable t, Eq (t Natural)) => t a -> t a -> Bool
alphaEq e1 e2 = toDBLvl e1 == toDBLvl e2

substitute :: (Traversable t, Eq a) => a -> a -> t a -> t a
substitute x y = fmap $ \a -> if x == a then y else a

substituteAll :: (Traversable t, Hashable a) => M.HashMap a a -> t a -> t a
substituteAll env = fmap $ \a -> fromMaybe a (M.lookup a env)

foldrFromTraversable :: (Traversable t) => (a -> b -> b) -> b -> t a -> b
foldrFromTraversable f e t = foldr f e . reverse $ execState (traverse (modify . (:)) t) []

foldlFromTraversable' :: (Traversable t) => (b -> a -> b) -> b -> t a -> b
foldlFromTraversable' f e t = foldl' f e $ execState (traverse (modify . (:)) t) []