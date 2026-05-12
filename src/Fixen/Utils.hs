{-# LANGUAGE FunctionalDependencies #-}

-- |
--     Module      : Fixen.Utils
--     Description : Miscellaneous definitions
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module contains a bunch of random stuff that we can't be bothered
--     to re-define over and over again.
module Fixen.Utils where

import Control.Monad
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- * Fun Unicode Symbols

-- $funUnicodeSymbols
--
-- Unicode symbols are fun, so we are defining some of them here.

-- | Logical AND
(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

infixr 3 ∧

{-# INLINE (∧) #-}

-- | Logical OR
(∨) :: Bool -> Bool -> Bool
(∨) = (||)

infixr 2 ∨

{-# INLINE (∨) #-}

-- | Logical negation
(¬) :: Bool -> Bool
(¬) = not
{-# INLINE (¬) #-}

-- | Not equal to
(≠) :: Eq α => α -> α -> Bool
(≠) = (/=)
{-# INLINE (≠) #-}

infix 4 ≠

-- | Function composition
(∘) :: (β -> χ) -> (α -> β) -> α -> χ
(∘) = (.)
{-# INLINE (∘) #-}

infixr 9 ∘

class Memberable φ α | φ -> α where
  (∈) :: α -> φ -> Bool
  (∉) :: α -> φ -> Bool

instance Eq α => Memberable [α] α where
  (∈) = elem
  (∉) = notElem
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

instance Eq α => Memberable (NonEmpty.NonEmpty α) α where
  (∈) = elem
  (∉) = notElem
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

instance Ord α => Memberable (Set.Set α) α where
  (∈) = Set.member
  (∉) = Set.notMember
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

instance Memberable IntSet.IntSet Int where
  (∈) = IntSet.member
  (∉) = IntSet.notMember
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

instance Ord α => Memberable (Map.Map α ε) α where
  (∈) = Map.member
  (∉) = Map.notMember
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

instance Memberable (IntMap.IntMap ε) Int where
  (∈) = IntMap.member
  (∉) = IntMap.notMember
  {-# INLINE (∈) #-}
  {-# INLINE (∉) #-}

infix 4 ∈
infix 4 ∉

-- * Container functions

-- | Class of map types whose values can be obtained
class HasValues m a | m -> a where
  -- | Obtains the values of a map
  values :: m -> [a]

instance HasValues (Map.Map k a) a where
  values m = snd <$> Map.toList m

instance HasValues (IntMap.IntMap a) a where
  values m = snd <$> IntMap.toList m

-- | Class of container types that have a union.
class Unionable m where
  -- | Unions
  (⋃) :: Foldable f => f m -> m

  -- | Union
  (∪) :: m -> m -> m

instance Unionable (IntMap.IntMap v) where
  (⋃) = IntMap.unions
  (∪) = IntMap.union
  {-# INLINE (⋃) #-}
  {-# INLINE (∪) #-}

instance Ord k => Unionable (Map.Map k v) where
  (⋃) = Map.unions
  (∪) = Map.union
  {-# INLINE (⋃) #-}
  {-# INLINE (∪) #-}

instance Ord k => Unionable (Set.Set k) where
  (⋃) = Set.unions
  (∪) = Set.union
  {-# INLINE (⋃) #-}
  {-# INLINE (∪) #-}

instance Unionable IntSet.IntSet where
  (⋃) = IntSet.unions
  (∪) = IntSet.union
  {-# INLINE (⋃) #-}
  {-# INLINE (∪) #-}

class Differenceable a where
  (∖) :: a -> a -> a

instance Differenceable IntSet.IntSet where
  (∖) = (IntSet.\\)
  {-# INLINE (∖) #-}

instance Ord a => Differenceable (Set.Set a) where
  (∖) = (Set.\\)
  {-# INLINE (∖) #-}

foldListMap :: (Monad m, Foldable f) => (a -> k -> b -> m a) -> a -> Map.Map k (f b) -> m a
foldListMap f e m = foldMWithKey f' e m
  where
    f' a k ls = foldM (\a' b' -> f a' k b') a ls

    foldMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map.Map k b -> m a
    foldMWithKey f1 e1 m1 = foldM (\a' (k, b) -> f1 a' k b) e1 (Map.toList m1)

foldMWith :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
foldMWith f = flip (foldM f)
