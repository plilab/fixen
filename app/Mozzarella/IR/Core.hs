{-# LANGUAGE DataKinds #-}
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
) where

import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)
import Mozzarella.IR.Core.Annotations

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

--------------------------------------------------------------------------------
--
-- Annotations
--
--------------------------------------------------------------------------------

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
