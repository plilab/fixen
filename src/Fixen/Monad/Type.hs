-- |
--     Module      : Fixen.Monad.Type
--     Description : Monads and type combinators for the Fixen compiler pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the monads and type combinators used throughout the
--     Fixen compiler. There are two monads:
--
--     * 'FixenM' — the top-level pipeline monad, used by the entry point
--       ('Fixen.Pipeline.parse') to orchestrate the entire compilation.
--       It is fail-fast: the first error terminates the pipeline.
--
--     * 'FixenPass' — the monad for individual compiler passes (parsing,
--       include resolution, symbol solving, etc.). It carries a state
--       parameter @σ@ that is built from composable pieces using the
--       type combinators defined in this module.
--
--     The module also defines a set of lightweight type combinators
--     (':+:', ':*:', ':<:', ':>:') used to build the state type @σ@
--     in an ala-carte style, allowing each pass to declare only the
--     pieces of state it needs.
module Fixen.Monad.Type where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Maybe (MaybeT)
import Error.Diagnose.Diagnostic (Diagnostic)

-------------------------------------------------------------------------------
-- Pipeline monad
-------------------------------------------------------------------------------

-- | The error type carried by 'FixenM'. This is a 'Diagnostic' of 'String'
-- values, produced by the 'Error.Diagnose' library.
type FixenErrorResult = Diagnostic String

-- | The top-level monad used by the Fixen compiler pipeline.
--
--   This is the monad in which the pipeline entry point
--   ('Fixen.Pipeline.parse') runs. It is an 'IO' monad that can fail
--   with a 'FixenErrorResult'. It is /fail-fast/: the first error
--   short-circuits the entire pipeline.
--
--   The pipeline calls individual passes (which run in 'FixenPass') via
--   'Fixen.Monad.Env.Errors.runFixenPass', converting their results back
--   into 'FixenM'.
type FixenM α = ExceptT FixenErrorResult IO α

-- | Run a 'FixenM' computation, returning the result as an 'IO' action.
--
--   On success, returns 'Right' with the computed value. On failure,
--   returns 'Left' with a 'FixenErrorResult' containing diagnostic
--   information.
runFixenM :: FixenM α -> IO (Either FixenErrorResult α)
runFixenM = runExceptT

-------------------------------------------------------------------------------
-- Pass monad
-------------------------------------------------------------------------------

-- | The monad used by individual compiler passes.
--
--   A pass is a computation that:
--
--   1. Carries mutable state of type @σ@ (via 'StateT')
--   2. Can short-circuit on failure (via 'MaybeT')
--   3. Performs 'IO' for file reads and error printing
--
--   The state type @σ@ is typically a right-associated product of
--   components built with ':*:', such as:
--
--   @
--   PositionEnv :*: NodeId :*: FixenErrors
--   @
--
--   Each component provides a specific capability (position tracking,
--   node ID allocation, error accumulation) and can be accessed
--   independently using the ':>:' and ':<:' classes.
--
--   A pass can fail in two ways:
--
--   * /Soft failure/ — the 'MaybeT' layer fails (e.g. 'failS' is called),
--     propagating 'Nothing' up the stack.
--   * /Error accumulation/ — errors are added to the 'FixenErrors'
--     component of the state via 'accumR' / 'accumD', and checked
--     later by 'runFixenPass'.
--
--   The canonical runner for passes is
--   'Fixen.Monad.Env.Errors.runFixenPass', which checks both the
--   'MaybeT' result and the accumulated errors before deciding whether
--   to continue or short-circuit into 'FixenM'.
type FixenPass σ α = MaybeT (StateT σ IO) α

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
type (:*:) = (,)

type (×) = (,)

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
class ϕ :<: γ where
  -- | Inject a value of type @ϕ@ into a value of type @γ@.
  --
  --   For the 'a :<: a' instance this is the identity function.
  --   For the 'a :<: (a :+: b)' instance this wraps in 'Left'.
  --   For the deep sum instance this recursively wraps in 'Right'.
  (↑) :: ϕ -> γ

  -- | Optionally project a value of type @ϕ@ from a value of type @γ@.
  --
  --   Returns 'Just' the projected value if @ϕ@ is present, 'Nothing'
  --   otherwise. For the 'a :<: (a :+: b)' instance, this pattern-
  --   matches on 'Left' and returns 'Nothing' for 'Right'.
  (↓?) :: γ -> Maybe ϕ

  -- | Replace the @ϕ@ component inside a @γ@ value with a new @ϕ@ value.
  --
  --   For the 'a :<: a' instance, this discards the old @γ@ and returns
  --   the new @ϕ@. For the sum instances, it replaces the @ϕ@ component
  --   at the appropriate depth, leaving other components untouched.
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
class α :>: β where
  -- | Project a value of type @α@ from a value of type @β@.
  --
  --   For the 'α :>: α' instance this is the identity function.
  --   For the '(α :*: β) :>: α' instance this is 'fst'.
  --   For the deep product instance this projects from the right side
  --   and then recursively projects.
  (↓) :: α -> β

  -- | Optionally inject a value of type @α@ into a value of type @β@.
  --
  --   Returns 'Just' the value for the trivial case (@α :>: α@).
  --   Returns 'Nothing' for non-trivial products, since there is no
  --   unambiguous way to embed a value into a product without knowing
  --   the other components.
  (↑?) :: β -> Maybe α

  -- | Replace the @α@ component inside a @β@ product with a new @α@ value.
  --
  --   For the 'α :>: α' instance, this discards the old @β@ and returns
  --   the new @α@. For the '(α :*: β) :>: α' instance, this replaces
  --   the left component while preserving the right. For the deep
  --   product instance, this recursively updates the right component.
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
