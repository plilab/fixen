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

concatNonEmpty :: NonEmpty.NonEmpty (NonEmpty.NonEmpty a) -> NonEmpty.NonEmpty a
concatNonEmpty (x NonEmpty.:| xs) = foldl' NonEmpty.append x xs

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-------------------------------------------------------------------------------
-- Sum types
-------------------------------------------------------------------------------

-- | Right-associated sum type, an alias for 'Either'.
--
--   Used to combine two types into a single type where a value may be
--   one or the other. This is the building block for constructing
--   state types ala-carte: each piece of state is a type, and
--   ':*:' products of ':+:' sums let you compose them flexibly.
--
--   Convention: right-associative, so @a :+: b :+: c@ parses as
--   @a :+: (b :+: c)@. The left argument should not itself be a sum.
--
-- @since 0.0.1
type (:+:) = Either

type (⊕) = Either

infixr 3 :+:, ⊕

-------------------------------------------------------------------------------
-- Product types
-------------------------------------------------------------------------------

-- | Right-associated product type, an alias for '(,)'.
--
--   Used to pair two types together. Like ':+:', this is used
--   in building state types ala-carte.
--
--   Convention: right-associative, so @a :*: b :*: c@ parses as
--   @a :*: (b :*: c)@. The left argument should not itself be a product.
--
-- @since 0.0.1
type (:*:) = (,)

type (×) = (,)

(×) :: a -> b -> a × b
(×) = (,)

infixr 3 :*:, ×

-------------------------------------------------------------------------------
-- Injection class
-------------------------------------------------------------------------------

-- | Class representing that a type @ϕ@ is a /component/ of a larger
-- type @γ@.
--
--   This class provides three operations:
--
--   * '↑' — inject a value of type @ϕ@ into a value of type @γ@
--   * '↓?' — optionally project a value of type @ϕ@ from a value of
--     type @γ@, returning 'Nothing' if @ϕ@ is not present
--   * '+<-:' — replace the @ϕ@ component inside a @γ@ value with a
--     new @ϕ@ value
--
--   Instances exist for:
--
--   * @ϕ :<: ϕ@ — identity (overlapping): a type is a component of itself
--   * @ϕ :<: (ϕ :+: γ)@ — left injection into a sum (overlapping)
--   * @ϕ :<: γ@ implies @ϕ :<: (α :+: γ)@ — deep injection through
--     sums (overlapping)
--
--   This is used to access individual pieces of the state type @σ@
--   in 'FixenPass'. For example, if @σ = PositionEnv :*: NodeId :*:
--   FixenErrors@, then:
--
--   * 'Fixen.Monad.Env.Errors.FixenErrors' is a component of @σ@
--     (via the 'Errored' constraint from 'Fixen.Monad.Env.Errors')
--   * 'Fixen.Monad.Env.Position.PositionEnv' is a component of @σ@
--     (via the 'Positioned' constraint)
--   * 'Fixen.Data.NodeId.NodeId' is a component of @σ@
--     (via the 'NodeIded' constraint from 'Fixen.Monad.Env.NodeId')
--
--   The '↑' and '↓?' operators let you move values in and out of the
--   state, while '+<-:' lets you update just one component without
--   touching the others.
--
-- @since 0.0.1
class ϕ :<: γ where
  -- | Inject a value of type @ϕ@ into a value of type @γ@.
  --
  --   For the 'a :<: a' instance this is the identity function.
  --   For the 'a :<: (a :+: b)' instance this wraps in 'Left'.
  --   For the deep sum instance this recursively wraps in 'Right'.
  --
  -- @since 0.0.1
  (↑) :: ϕ -> γ

  -- | Optionally project a value of type @ϕ@ from a value of type @γ@.
  --
  --   Returns 'Just' the projected value if @ϕ@ is present, 'Nothing'
  --   otherwise. For the 'a :<: (a :+: b)' instance, this pattern-
  --   matches on 'Left' and returns 'Nothing' for 'Right'.
  --
  -- @since 0.0.1
  (↓?) :: γ -> Maybe ϕ

  -- | Replace the @ϕ@ component inside a @γ@ value with a new @ϕ@ value.
  --
  --   For the 'a :<: a' instance, this discards the old @γ@ and returns
  --   the new @ϕ@. For the sum instances, it replaces the @ϕ@ component
  --   at the appropriate depth, leaving other components untouched.
  --
  -- @since 0.0.1
  (+<-:) :: γ -> ϕ -> γ

infixr 3 :<:

instance {-# OVERLAPPING #-} a :<: a where
  (↑) = id
  (↓?) = Just
  _ +<-: a = a

instance {-# OVERLAPS #-} a :<: (a :+: b) where
  (↑) = Left
  (↓?) (Left x) = Just x
  (↓?) _ = Nothing
  Left _ +<-: a = Left a
  x +<-: _ = x

instance {-# OVERLAPPABLE #-} a :<: c => (a :<: (b :+: c)) where
  (↑) = Right . (↑)
  (↓?) (Left _) = Nothing
  (↓?) (Right x) = (↓?) x
  Left x +<-: _ = Left x
  Right x +<-: a = Right (x +<-: a)

-------------------------------------------------------------------------------
-- Projection class
-------------------------------------------------------------------------------

-- | Class representing that a type @α@ can be projected from a product type @β@.
--
--   This is the dual of ':<:': while ':<:' works with sums (where a type
--   may appear at any depth), ':>:' works with products (where a type
--   appears as one element of a right-associated chain of pairs).
--
--   This class provides three operations:
--
--   * '↓' — project a value of type @α@ from a value of type @β@
--   * '↑?' — optionally inject a value of type @α@ into a value of
--     type @β@ (returns 'Nothing' for non-trivial products)
--   * '*<-:' — replace the @α@ component inside a @β@ product with a
--     new @α@ value, preserving all other components
--
--   Instances exist for:
--
--   * @α :>: α@ — identity (overlapping)
--   * @(α :*: β) :>: α@ — left projection from a product (overlapping)
--   * @(χ :*: β) :>: α@ when @β :>: α@ — deep projection through
--     products (overlapping)
--
--   This is used to access individual components of the state type @σ@
--   when the state is a product. For example, given a state of type
--   @PositionEnv :*: NodeId :*: FixenErrors@:
--
--   * 'PositionEnv' can be projected via the 'Positioned' type alias
--     from 'Fixen.Monad.Env.Position', which is defined as
--     @Positioned a = a :>: PositionEnv@
--   * Similarly, 'NodeId' and 'FixenErrors' have their own projection
--     type aliases in 'Fixen.Monad.Env.NodeId' and
--     'Fixen.Monad.Env.Errors'
--
--   The '*<-:' operator is particularly useful for updating one piece
--   of state without reconstructing the entire product:
--
--   @
--   new_state = old_state *<-: new_position_env
--   @
--
-- @since 0.0.1
class α :>: β where
  -- | Project a value of type @α@ from a value of type @β@.
  --
  --   For the 'α :>: α' instance this is the identity function.
  --   For the '(α :*: β) :>: α' instance this is 'fst'.
  --   For the deep product instance this projects from the right side
  --   and then recursively projects.
  --
  -- @since 0.0.1
  (↓) :: α -> β

  -- | Optionally inject a value of type @α@ into a value of type @β@.
  --
  --   Returns 'Just' the value for the trivial case (@α :>: α@).
  --   Returns 'Nothing' for non-trivial products, since there is no
  --   unambiguous way to embed a value into a product without knowing
  --   the other components.
  --
  -- @since 0.0.1
  (↑?) :: β -> Maybe α

  -- | Replace the @α@ component inside a @β@ product with a new @α@ value.
  --
  --   For the 'α :>: α' instance, this discards the old @β@ and returns
  --   the new @α@. For the '(α :*: β) :>: α' instance, this replaces
  --   the left component while preserving the right. For the deep
  --   product instance, this recursively updates the right component.
  --
  -- @since 0.0.1
  (*<-:) :: α -> β -> α

instance {-# OVERLAPPING #-} α :>: α where
  (↓) = id
  (↑?) = Just
  _ *<-: a = a

instance {-# OVERLAPS #-} (α :*: β) :>: α where
  (↓) = fst
  (↑?) = const Nothing
  (_, b) *<-: a = (a, b)

instance {-# OVERLAPPABLE #-} β :>: α => (χ :*: β) :>: α where
  (↓) = (↓) . snd
  (↑?) = const Nothing
  (a, b) *<-: c = (a, b *<-: c)
