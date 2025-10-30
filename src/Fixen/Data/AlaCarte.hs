{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Fixen.Data.AlaCarte (
  -- ** Sums
  -- $sum
  (:+:),
  (::+::) (..),

  -- ** Products
  -- $prod
  (:*:),

  -- ** Recursion
  -- $recursion
  Fixpoint (..),

  -- ** Injections
  -- $injection
  (:<:) (..),
  (::<::) (..),
  (↑↑),
  (↓↓?),

  -- ** Projections
  -- $projection
  (:>:) (..),
) where

-------------------------------------------------------------------------------
--
-- Sum Types

-- $sum
-- We obviously need to sum types together, otherwise we would be wasting
-- time. We have two simple sums:
--
--    (1) @a ':+:' b@ is just 'Either' a b, the most basic sum
--    (2) @a '::+::' b@ is the sum of two functors a and b, giving a new functor.
--
-- We use these sums to keep each branch of algebraic data types modular.
--
-- The convention we use is that these are right-associative, and the left
-- injection cannot be itself a sum. Thus, just use these types in a flattened
-- manner, like
-- @a :+: b :+: c :+: d@
-- where all of @a@, @b@, @c@ and @d@ are not sums.

-------------------------------------------------------------------------------

-- | @a ':+:' b@ is @Either a b@.
type (:+:) = Either

infixr 3 :+:

-- | The sum of two functors. Useful for creating sums of recursive types.
data (ϕ ::+:: γ) α
  = -- | Left injection
    Inl
      (ϕ α)
  | -- | Right injection
    Inr
      (γ α)
  deriving (Show, Eq)

infixr 3 ::+::

-------------------------------------------------------------------------------
--
-- Products

-- $prod
-- For ad-hoc cases (particularly, annotations) we also define products. There
-- probably isn't a use case for the product of functors, so we just define
-- an alias for the usual product type. Just like for sums, we use the
-- convention that products are right-associative and the left projection
-- cannot be itself a product. Thus, just use the type alias in a flattened
-- manner, like
-- @a :*: b :*: c :*: d@
-- where all of @a@, @b@, @c@ and @d@ are not products.

-------------------------------------------------------------------------------

-- | @a ':*:' b@ is @(a, b)@
type (:*:) = (,)

infixr 3 :*:

-------------------------------------------------------------------------------
--
-- Recursion

-- $recursion
-- Now we define the recursive items. The key here is that types that are to
-- be recursive
-- must be functors, and the last type parameter is needed for recursion. If
-- the compound type has the recursive type for subterms, use that type
-- parameter. If it doesn't just don't use that type parameter.
--
-- The 'Fixpoint' type is what drives the recursion. To make a type recurse,
-- apply 'Fixpoint' to it.
--
-- For example, a recursive type built using a var-like thing ('CoreItem')
-- and an app-like thing ('CoreDouble') can be written something like the
-- following:
--
-- @
-- type Var = 'CoreItem' \"Var\" () 'String'
-- type App = 'CoreDouble' \"App\" ()
-- type Expr = 'Fixpoint' (Var ::+:: App)
-- @

-------------------------------------------------------------------------------

-- | The 'Fixpoint' of a functor
newtype Fixpoint ϕ = Fixpoint {unFixpoint :: ϕ (Fixpoint ϕ)}

instance (Show (ϕ (Fixpoint ϕ))) => Show (Fixpoint ϕ) where
  show (Fixpoint x) = show x

instance (Eq (ϕ (Fixpoint ϕ))) => Eq (Fixpoint ϕ) where
  (Fixpoint x) == (Fixpoint y) = x == y

-------------------------------------------------------------------------------
--
-- Injections

-- $injection
-- Convenience classes and functions for injecting types into sums.

-- | @f ':<:' g@ means that @f@ is included in @g@, and thus a term of type @f@
-- can be injected into @g@.
class ϕ :<: γ where
  -- | Injection into a term
  (↑) :: ϕ -> γ

  -- | Projection from a term
  (↓?) :: γ -> Maybe ϕ

  -- | Replacement of type-equal terms. I frankly have no idea what you will
  -- ever use this for.
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

instance {-# OVERLAPPABLE #-} (a :<: c) => (a :<: (b :+: c)) where
  (↑) = Right . (↑)
  (↓?) (Left _) = Nothing
  (↓?) (Right x) = (↓?) x
  Left x +<-: _ = Left x
  Right x +<-: a = Right (x +<-: a)

-- | @f '::<::' g@ means that @f@ is included in @g@, and thus a term of type
-- @f@ can be injected into @g@. It is the same as ':<:' except for functors.
-- It is unlikely that you will use these methods. See '(↑↑)' and '(↓↓?)'.
class ϕ ::<:: γ where
  -- | Injection into functors
  (↑↑..) :: ϕ a -> γ a

  -- | Projection from functors
  (↓↓?..) :: γ a -> Maybe (ϕ a)

  -- | Replacement of type-equal terms. I frankly have no idea what you will
  -- ever use this for.
  (+<-::) :: γ a -> ϕ a -> γ a

instance {-# OVERLAPPING #-} ϕ ::<:: ϕ where
  (↑↑..) = id
  (↓↓?..) = Just
  _ +<-:: a = a

instance {-# OVERLAPS #-} ϕ ::<:: (ϕ ::+:: γ) where
  (↑↑..) = Inl
  (↓↓?..) (Inl x) = Just x
  (↓↓?..) _ = Nothing
  Inl _ +<-:: a = Inl a
  x +<-:: _ = x

instance {-# OVERLAPPABLE #-} (ϕ ::<:: θ) => (ϕ ::<:: (γ ::+:: θ)) where
  (↑↑..) = Inr . (↑↑..)
  (↓↓?..) (Inr x) = (↓↓?..) x
  (↓↓?..) _ = Nothing
  Inl x +<-:: _ = Inl x
  Inr x +<-:: a = Inr (x +<-:: a)

(↑↑) :: (γ ::<:: ϕ) => γ (Fixpoint ϕ) -> Fixpoint ϕ
(↑↑) = Fixpoint . (↑↑..)

(↓↓?) :: (γ ::<:: ϕ) => Fixpoint ϕ -> Maybe (γ (Fixpoint ϕ))
(↓↓?) = (↓↓?..) . unFixpoint

infixr 3 ::<::

-------------------------------------------------------------------------------
--
-- Projections

-- $projection
-- Convenience classes and functions for projecting types from products.
-- Useful for compound annotations. We haven't seen a use-case for doing so
-- with functors, so that is omitted.

-- | @f ':>:' g@ means that @f@ can be projected into @g@.
class α :>: β where
  -- | Projection
  (↓) :: α -> β

  -- | Injection
  (↑?) :: β -> Maybe α

  -- | Replacement of type-equal terms. Useful for replacing annotations.
  (*<-:) :: α -> β -> α

instance {-# OVERLAPPING #-} α :>: α where
  (↓) = id
  (↑?) = Just
  _ *<-: a = a

instance {-# OVERLAPS #-} (α :*: β) :>: α where
  (↓) = fst
  (↑?) = const Nothing
  (_, b) *<-: a = (a, b)

instance {-# OVERLAPPABLE #-} (β :>: α) => (χ :*: β) :>: α where
  (↓) = (↓) . snd
  (↑?) = const Nothing
  (a, b) *<-: c = (a, b *<-: c)
