{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--     Module      : Mozzarella.IR.Core
--     Description : Building blocks for Mozzarella intermediate representations
--                   (IRs)
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Building blocks for intermediate representations for Mozzarella. This
--     module uses hardcore abstractions and data types a la carte.
module Mozzarella.IR.Core (
  -- * Building blocks
  -- $buildingblocks

  -- ** Basics
  CoreItem (..),
  CorePair (..),
  CoreDouble (..),

  -- ** Common Program Constructs
  -- $programconstructs
  CoreRelation (..),
  CoreRule (..),

  -- * Composition and Recursion

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

  -- * Annotations
  -- $ann
  GetAnnotation (..),
  SetAnnotation (..),
  getAnnotationOf,
  setAnnotationOf,
) where

import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)

-------------------------------------------------------------------------------
--
-- Building blocks

-- $buildingblocks
-- Here we define the basic building blocks of Mozzarella intermediate
-- representations. The reason we want to do this is so that common patterns
-- like annotated items, pairs and recursive types are captured all in the same
-- type to be re-used and for common operations to be easily defined.
--
-- You know things get serious when we use greek symbols for type parameters!
-- The core types use clever tricks on types to compose and yet be
-- differentiated easily. In particular, they all have a name 'Symbol' kind
-- which allows two disparate types built from the same building blocks to
-- still be distinguishable at the type level. The type parameters of the
-- constructors are also explicitly ordered so that you can apply the correct
-- symbol to specialize them.
--
-- See each type here for more information on usage.

-------------------------------------------------------------------------------

-- | Describes an annotated item. The type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CoreItem' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@a@] The type of the item being annotated.
--  [@ρ@] The recursor.
--
--  These are used as building blocks of larger types. For instance,
--  term-level identifiers can be expressed as the type
--
--  @type Identifier = 'CoreItem' "identifier" 'Position' 'String' 'Void'@
--
--  which simply annotates
--  a string with a source position ('Void' disables recursion). It can also be
--  used in recursive types by using 'Fixpoint' and not applying the last
--  type parameter.
--
--  To construct a 'CoreItem' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CoreItem' constructor
--  to construct a @'CoreItem' "identifier" ...@, you can write:
--
--  @'CoreItem' \@"identifier" ...@
data CoreItem (ℓ :: Symbol) μ α (ρ :: κ) where
  CoreItem
    :: forall name ann a rec
     . !ann
    -- ^ The annotation
    -> !a
    -- ^ The annotated term
    -> CoreItem name ann a rec
  deriving (Eq, Typeable)

instance (KnownSymbol a, Show b, Show c) => Show (CoreItem a b c d) where
  show (CoreItem b c) =
    let type_name = show $ typeRep (Proxy :: Proxy a)
    in  "(CoreItem @" ++ drop 1 (init type_name) ++ " (" ++ show b ++ ") (" ++ show c ++ "))"

-- | Describes an annotated pair. The type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CorePair' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@α@] The type of the left projection
--  [@β@] The type of the right projection.
--
--  These are used as building blocks of pairs. For instance,
--  an @('Int', 'Bool')@ pair annotated with a 'String' can be expressed as
--
--  @type MyPair = 'CorePair' "p" 'String' 'Int' 'Bool'@
--
--  For recursive pairs (where both projections are the same recursive type),
--  see 'CoreDouble'.
--
--  To construct a 'CorePair' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CorePair' constructor
--  to construct a @'CorePair' "identifier" ...@, you can write:
--
--  @'CorePair' \@"identifier" ...@
data CorePair (ℓ :: Symbol) μ α β where
  CorePair
    :: forall name ann left right
     . !ann
    -- ^ The annotation
    -> !left
    -- ^ The left projection
    -> !right
    -- ^ The right projection
    -> CorePair name ann left right
  deriving (Show, Eq)

-- | Describes an annotated pair whose projections are of the same type. The
-- type parameters are:
--
--  [@ℓ@] A name used to disambiguate multiple occurrences of
--        'CorePair' that occur in a sum.
--  [@μ@] The type of the annotation.
--  [@ρ@] The type of the projections.
--
--  These are used as building blocks of pairs with the same type. For instance,
--  an 'Int' pair annotated with a 'String' can be expressed as
--
--  @type MyPair = 'CoreDouble' "p" 'String' 'Int'@
--
--  The projections type can be recursive when built with 'Fixpoint' and by not
--  applying the last type parameter.
--
--  To construct a 'CoreDouble' with a specific name, just apply it to the
--  type symbol. For instance, if you want to use the 'CoreDouble' constructor
--  to construct a @'CoreDouble' "identifier" ...@, you can write:
--
--  @'CoreDouble' \@"identifier" ...@
--
--  The reason we do not re-use 'CorePair' and specialize it to one type
--  argument using a type synonym is because the type-checker can become unable
--  to terminate in those scenarios.
data CoreDouble (ℓ :: Symbol) μ ρ where
  CoreDouble
    :: forall name ann e
     . !ann
    -- ^ The annotation
    -> !e
    -- ^ The left projection
    -> !e
    -- ^ The right projection
    -> CoreDouble name ann e
  deriving (Show, Eq)

-------------------------------------------------------------------------------
--
-- Common program constructs

-- $programconstructs
-- Many parts of the intermediate representation have constructs that are
-- frequently occurring. We define them here.

-------------------------------------------------------------------------------

-- | A relation in the program.
data CoreRelation (ℓ :: Symbol) α β π χ where
  CoreRelation
    :: forall name ann rel_name param comp
     . ann
    -- ^ The annotation
    -> rel_name
    -- ^ The name of the relation
    -> param
    -- ^ The arguments to the relation
    -> comp
    -- ^ The completion
    -> CoreRelation name ann rel_name param comp
  deriving (Show, Eq)

-- | A rule in the program.
data CoreRule (ℓ :: Symbol) α ν β π χ where
  CoreRule
    :: forall name ann rule_name rule_bound_vars premise concl
     . ann
    -- ^ The annotation
    -> rule_name
    -- ^ The name of the rule
    -> rule_bound_vars
    -- ^ The bound variables of the rule
    -> premise
    -- ^ The premises of the rule
    -> concl
    -- ^ The conclusion of the rule
    -> CoreRule name ann rule_name rule_bound_vars premise concl
  deriving (Show, Eq)

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

--------------------------------------------------------------------------------
--
-- Annotations

-- $ann
-- The core building blocks and program constructs are annotated. These classes
-- provide useful utility for retrieving and modifying them.

-- | A type that can obtain an annotation
class GetAnnotation ann e | e -> ann where
  -- | Get the annotation
  getAnnotation :: e -> ann

-- | The 'SetAnnotation' class witnesses the ability to set the annotation for
-- a type, potentially yielding a different type.
class SetAnnotation ann x y | ann x -> y where
  -- | Set the annotation
  setAnnotation
    :: ann
    -- ^ The annotation
    -> x
    -- ^ The term whose annotation is to be modified
    -> y

instance (GetAnnotation ann l, GetAnnotation ann r) => GetAnnotation ann (l :+: r) where
  getAnnotation (Left x) = getAnnotation x
  getAnnotation (Right x) = getAnnotation x

instance (SetAnnotation ann l l', SetAnnotation ann r r') => SetAnnotation ann (l :+: r) (l' :+: r') where
  setAnnotation ann (Left x) = Left (setAnnotation ann x)
  setAnnotation ann (Right x) = Right (setAnnotation ann x)

instance (GetAnnotation ann (f a), GetAnnotation ann (g a)) => GetAnnotation ann ((f ::+:: g) a) where
  getAnnotation (Inl x) = getAnnotation x
  getAnnotation (Inr x) = getAnnotation x

instance (SetAnnotation ann (f a) (f' a'), SetAnnotation ann (g a) (g' a')) => SetAnnotation ann ((f ::+:: g) a) ((f' ::+:: g') a') where
  setAnnotation ann (Inl x) = Inl (setAnnotation ann x)
  setAnnotation ann (Inr x) = Inr (setAnnotation ann x)

instance (GetAnnotation ann (f (Fixpoint f))) => GetAnnotation ann (Fixpoint f) where
  getAnnotation (Fixpoint x) = getAnnotation x

instance (SetAnnotation ann (f (Fixpoint f)) (f' (Fixpoint f'))) => SetAnnotation ann (Fixpoint f) (Fixpoint f') where
  setAnnotation ann (Fixpoint x) = Fixpoint (setAnnotation ann x)

instance GetAnnotation ann (CoreItem name ann a rec) where
  getAnnotation (CoreItem ann _) = ann

instance GetAnnotation ann (CoreDouble name ann a) where
  getAnnotation (CoreDouble ann _ _) = ann

instance SetAnnotation ann (CoreItem name ann' a rec) (CoreItem name ann a rec) where
  setAnnotation ann (CoreItem _ x) = CoreItem ann x

instance SetAnnotation ann (CoreDouble name ann' e) (CoreDouble name ann e) where
  setAnnotation ann (CoreDouble _ a b) = CoreDouble ann a b

instance GetAnnotation ann (CorePair name ann a rec) where
  getAnnotation (CorePair ann _ _) = ann

-- | Since annotations are frequently composed with products, this convenience
-- function retrieves and focuses of exactly one component of the larger
-- annotation.
getAnnotationOf :: forall target group e. (group :>: target, GetAnnotation group e) => e -> target
getAnnotationOf = (↓) . getAnnotation

-- | Since annotations are frequently composed with products, this convenience
-- function focuses and sets exactly one component of the larger
-- annotation.
setAnnotationOf
  :: forall target group e e'
   . (group :>: target, SetAnnotation group e e', GetAnnotation group e)
  => target
  -- ^ The annotation
  -> e
  -- ^ The term whose annotation is to be modified
  -> e'
setAnnotationOf ann e =
  let old_ann_group = getAnnotation e
      new_ann_group = old_ann_group *<-: ann
  in  setAnnotation new_ann_group e
