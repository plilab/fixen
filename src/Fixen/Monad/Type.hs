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
-- @since 0.0.1
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
--
-- @since 0.0.1
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
--
-- @since 0.0.1
type FixenM α = ExceptT FixenErrorResult IO α

-- | Run a 'FixenM' computation, returning the result as an 'IO' action.
--
--   On success, returns 'Right' with the computed value. On failure,
--   returns 'Left' with a 'FixenErrorResult' containing diagnostic
--   information.
--
-- @since 0.0.1
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
--
-- @since 0.0.1
type FixenPass σ α = MaybeT (StateT σ IO) α
