{-# LANGUAGE LambdaCase #-}

-- |
--     Module      : Mozzarella.Monad
--     Description : Monads for the Mozzarella compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines monads for the Mozzarella compiler.
module Mozzarella.Monad where

import Control.Monad
import Control.Monad.Except qualified as Except
import Control.Monad.IO.Class
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Maybe as Maybe
import Data.List (foldl', partition)
import Error.Diagnose.Diagnostic qualified as Diagnostic
import Error.Diagnose.Report qualified as Report
import Mozzarella.Data.AlaCarte

-- * Pipeline monad

-- $ Essential monad used by the compiler pipeline. Clients using
-- the pipeline need only care about this section.

-- | The monad used for the pipeline. It is fail-fast, i.e. once the first
-- failing component in the monad fails, the entire monad fails. The
-- type argument @α@ is the type of the term being returned from the monad if
-- it succeeds. If it does not succeed, then it returns a
-- @'Diagnostic.Diagnostic' 'String'@.
type MozzarellaM α = Except.ExceptT (Diagnostic.Diagnostic String) IO α

-- | Deconstructs 'MozzarellaM' into an 'IO'. The result is 'Either' an error
-- message stored in a @'Diagnostic.Diagnostic' 'String'@, or the successful
-- result.
runMozzarellaM :: MozzarellaM α -> IO (Either (Diagnostic.Diagnostic String) α)
runMozzarellaM = Except.runExceptT

-- * Pass monad

-- $ Monad used for compiler passes/phases.

-- | A monad used for passes in the pipeline. It is a combination of 'Maybe',
-- 'State.State' and 'IO'. The monad supports both fail-fast and failure
-- accumulation (as long as the store is 'Errored'). The first type argument is
-- the state used, the second argument is the type of the term being returned
-- from the monad.
type MozzarellaPass σ α = Maybe.MaybeT (State.StateT σ IO) α

-- | The type of states that can contain errors.
type Errored σ = σ :>: MozzarellaErrors

-- | The canonical way to run a pass in the pipeline. If the pass terminated
-- with no result, or if errors are present in the resulting state then it
-- terminates with the errors. For a run that succeeds as long as the monad
-- returns a value, see 'runMozzarellaPass'' and 'runMozzarellaPass'''.
runMozzarellaPass :: (Errored σ) => σ -> MozzarellaPass σ α -> MozzarellaM (α, σ)
runMozzarellaPass state monad = do
  (res, state') <- Except.ExceptT $ Right <$> State.runStateT (Maybe.runMaybeT monad) state
  -- Get the errors resulting from the state.
  let errs :: MozzarellaErrors = (↓) state'
  case res of
    -- just re-raise all the errors since no result was received
    Nothing -> Except.liftEither (Left (mozErrorsDiagnostic errs))
    Just res' ->
      -- check if there are errors, if there are, fail
      if mozHasErrors errs
        then Except.liftEither (Left (mozErrorsDiagnostic errs))
        else return (res', state')

-- | Runs a pass in the pipeline. If the pass terminated
-- with no result then it
-- terminates with the errors in the state.
runMozzarellaPass' :: (Errored σ) => σ -> MozzarellaPass σ α -> MozzarellaM (α, σ)
runMozzarellaPass' state monad = do
  (res, state') <-
    Except.ExceptT $
      Right
        <$> State.runStateT (Maybe.runMaybeT monad) state
  -- Get the errors resulting from the state.
  let errs :: MozzarellaErrors = (↓) state'
  case res of
    -- just re-raise all the errors since no result was received
    Nothing -> Except.liftEither (Left (mozErrorsDiagnostic errs))
    Just res' -> return (res', state')

-- | Runs a pass in the pipeline. If the pass terminated
-- with no result then it
-- terminates with the empty error message. This is useful if the state does
-- not contain errors.
runMozzarellaPass'' :: σ -> MozzarellaPass σ α -> MozzarellaM (α, σ)
runMozzarellaPass'' state monad = do
  (res, state') <-
    Except.ExceptT $
      Right
        <$> State.runStateT (Maybe.runMaybeT monad) state
  case res of
    -- just re-raise all the errors since no result was received
    Nothing -> Except.liftEither (Left mempty)
    Just res' -> return (res', state')

-- | Runs a stateful computation with more state in a monad with less state by
-- providing the additional state.
adapter :: ε -> MozzarellaPass (σ :*: ε) a -> MozzarellaPass σ (a, ε)
adapter st' m = Maybe.MaybeT $ State.StateT $ \st ->
  (\(x, (y, z)) -> ((,z) <$> x, y)) <$> State.runStateT (Maybe.runMaybeT m) (st, st')

-- | Fail fast with a report.
failR
  :: (Errored σ, State.MonadState σ μ, MonadPlus μ)
  => Report.Report String
  -> μ a
failR rep = accumR rep >> mzero

-- | Fail fast with a diagnostic
failD
  :: (Errored σ, State.MonadState σ μ, MonadPlus μ)
  => Diagnostic.Diagnostic String
  -> μ a
failD rep = accumD rep >> mzero

-- | Fail fast if there have been accumulated errors
failIfErrored
  :: (Errored σ, State.MonadState σ μ, MonadPlus μ)
  => μ ()
failIfErrored = do
  moz_state <- State.get
  let errs :: MozzarellaErrors = (↓) moz_state
  when (mozHasErrors errs) mzero

-- | Add a 'Report.Report' __without failing__. This is useful for accumulating
-- error messages.
accumR
  :: (State.MonadState σ μ, σ :>: MozzarellaErrors)
  => Report.Report String
  -> μ ()
accumR rep = do
  moz_state <- State.get
  let errs :: MozzarellaErrors = (↓) moz_state
      new_errs =
        errs
          { mozErrorsDiagnostic =
              Diagnostic.addReport (mozErrorsDiagnostic errs) rep
          }
      new_state = moz_state *<-: new_errs
  State.put new_state

-- | Add a 'Diagnostic.Diagnostic' 'String' __without failing__. This is useful
-- for accumulating error messages.
accumD
  :: (State.MonadState σ μ, Errored σ)
  => Diagnostic.Diagnostic String
  -> μ ()
accumD d = do
  moz_state <- State.get
  let errs :: MozzarellaErrors = (↓) moz_state
      new_errs = errs {mozErrorsDiagnostic = mozErrorsDiagnostic errs <> d}
      new_state = moz_state *<-: new_errs
  State.put new_state

-- * Errors

-- $ The main error tracker used by the compiler.

-- | The type of errors produced by Mozzarella passes. The main error type
-- are 'Diagnostic.Diagnostic' 'String's; to be able to reset the diagnostics
-- (e.g., flushing out warnings in between passes), we also store a map of
-- file names to file contents ('MozzarellaFileMap').
data MozzarellaErrors = MozzarellaErrors
  { mozErrorsFileMap :: MozzarellaFileMap
  , mozErrorsDiagnostic :: Diagnostic.Diagnostic String
  }

-- | Just a list of pairs mapping file paths to file contents.
type MozzarellaFileMap = [(FilePath, String)]

-- | Clears all errors from a 'MozzarellaErrors'.
mozResetErrors :: MozzarellaErrors -> MozzarellaErrors
mozResetErrors MozzarellaErrors {mozErrorsFileMap = ls} = mozEmptyErrors ls

-- | Creates an empty set of errors from a 'MozzarellaFileMap'.
mozEmptyErrors :: MozzarellaFileMap -> MozzarellaErrors
mozEmptyErrors ls =
  let d = foldl' (\r (fp, s) -> Diagnostic.addFile r fp s) mempty ls
  in  MozzarellaErrors {mozErrorsFileMap = ls, mozErrorsDiagnostic = d}

-- | Determines if a set of errors has error messages (warnings are not counted)
mozHasErrors :: MozzarellaErrors -> Bool
mozHasErrors MozzarellaErrors {mozErrorsDiagnostic = b} =
  let rep = Diagnostic.reportsOf b
      -- partition the reports by errors and warnings
      -- to check if there are any errors
      (errs, _) =
        partition
          ( \case
              Report.Err {} -> True
              _ -> False
          )
          rep
  in  not (null errs)

instance Semigroup MozzarellaErrors where
  (MozzarellaErrors a b) <> (MozzarellaErrors a' b') =
    MozzarellaErrors (a <> a') (b <> b')

instance Monoid MozzarellaErrors where
  mempty = MozzarellaErrors mempty mempty
