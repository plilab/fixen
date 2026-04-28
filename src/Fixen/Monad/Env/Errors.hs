{-# LANGUAGE LambdaCase #-}

-- |
--     Module      : Fixen.Monad.Env.Errors
--     Description : Error tracking and pass execution for the Fixen compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides:
--
--     * 'FixenErrors' — the error accumulator carried in the pass state, which
--       holds both a 'Diagnostic' and a file map for context
--
--     * 'Errored' — a type alias for states that contain 'FixenErrors'
--
--     * 'runFixenPass' and variants — functions for executing a 'FixenPass'
--       and converting the result into 'FixenM'
--
--     * 'failS', 'failR', 'failD', 'failIfErrored' — helpers for failing
--       a pass (with or without accumulating errors first)
--
--     * 'accumR', 'accumD' — helpers for adding diagnostics / reports
--       to the error accumulator without immediately failing
--
--     * 'fixenInsertFileMap', 'fixenResetErrors', 'fixenEmptyErrors' —
--       utilities for managing the file map and error state
--
--     The overall error model supports two modes:
--
--     * /Fail-fast/ — calling any 'fail*' function immediately short-circuits
--       the pass via 'mzero' in the 'MaybeT' layer, after optionally
--       accumulating the error in the state first.
--
--     * /Error accumulation/ — calling 'accumR' or 'accumD' adds errors
--       to the state without failing, allowing a pass to collect multiple
--       diagnostics before terminating. The 'runFixenPass' family checks
--       for accumulated errors and fails if any are present.
module Fixen.Monad.Env.Errors (
  module Fixen.Monad.Env.Errors,
  Diagnostic,
  Position (..),
  Marker (..),
  Note (..),
  Report (..),
) where

import Control.Monad (
  MonadPlus,
  mzero,
  when,
 )
import Control.Monad.Except (
  ExceptT (ExceptT),
  liftEither,
 )
import Control.Monad.IO.Class (
  MonadIO,
 )
import Control.Monad.State.Strict (
  MonadState,
  get,
  put,
  runStateT,
 )
import Control.Monad.Trans.Maybe (
  MaybeT (MaybeT),
  runMaybeT,
 )
import Data.List (partition)
import Error.Diagnose.Diagnostic (
  Diagnostic,
  addFile,
  addReport,
  reportsOf,
 )
import Error.Diagnose.Position (Position (..))
import Error.Diagnose.Report (
  Marker (..),
  Note (..),
  Report (Err, Warn),
 )
import Fixen.Monad.Type
import Prettyprinter (Pretty)

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

-- | A map from file paths to their contents, used as context for diagnostic
-- messages.
--
--   Each pass that encounters errors stores the relevant files here so that
--   the diagnostic renderer can display source snippets. The list is
--   /unbounded/ — files are prepended on each call to 'fixenInsertFileMap',
--   so the list may contain duplicates if the same file is inserted multiple
--   times.
type FixenFileMap = [(FilePath, String)]

-- | The error accumulator carried in the pass state.
--
--   This type bundles two pieces of information:
--
--   * 'fixenErrorsFileMap' — the file map ('FixenFileMap') providing source
--     context for diagnostic messages
--
--   * 'fixenErrorsDiagnostic' — the actual diagnostics ('FixenErrorResult'),
--     which is a 'Diagnostic' of 'String' values
--
--   The 'Semigroup' and 'Monoid' instances combine two 'FixenErrors' values
--   by merging both the file maps and the diagnostics. This means that
--   errors from multiple passes accumulate seamlessly.
--
--   The ability to /reset/ errors (via 'fixenResetErrors') while preserving
--   the file map is used to flush warnings between passes — see
--   'runFixenPassFlushWarnings'.
data FixenErrors = FixenErrors
  { fixenErrorsFileMap :: FixenFileMap
  -- ^ The file map providing source context for diagnostic messages.
  --   Files are stored as (path, contents) pairs.
  , fixenErrorsDiagnostic :: FixenErrorResult
  -- ^ The accumulated diagnostic messages. This includes both errors
  --   and warnings; use 'fixenHasErrors' to distinguish between them.
  }

-- | Constraint alias indicating that a state type @σ@ contains 'FixenErrors'.
--
--   This is a shorthand for @σ :>: FixenErrors@, meaning that 'FixenErrors'
--   can be projected from @σ@ using the ':>:' class.
--
--   This constraint is required by all functions that need to read or
--   modify the error accumulator. It is satisfied by any state type that
--   includes 'FixenErrors' as a component, such as:
--
--   @
--   PositionEnv :*: NodeId :*: FixenErrors
--   @
type Errored σ = σ :>: FixenErrors

-- | Insert a file into the error accumulator's file map.
--
--   This prepends a (file path, file contents) pair to the 'FixenFileMap'
--   inside the state. The file is also attached to the current diagnostics
--   via 'addFile', so that any diagnostic messages produced after this
--   call will have the file available for source context.
--
--   This is typically called when a pass reads a file (e.g., an included
--   file in 'Fixen.ModuleSystem') so that error messages referencing
--   that file can display its source.
fixenInsertFileMap :: (Errored σ, MonadState σ μ) => FilePath -> String -> μ ()
fixenInsertFileMap fp s = do
  -- Retrieve the current state
  st <- get
  -- Extract the current FixenErrors from the state
  let errs :: FixenErrors = (↓) st
      -- Create a new FixenErrors with the file added to the map
      -- and attached to the diagnostics
      new_errs =
        errs
          { fixenErrorsFileMap = (fp, s) : fixenErrorsFileMap errs
          , fixenErrorsDiagnostic = addFile (fixenErrorsDiagnostic errs) fp s
          }
      -- Update the FixenErrors component in the state
      new_state = st *<-: new_errs
  -- Write the updated state back
  put new_state

-- | Reset all diagnostics in a 'FixenErrors' while preserving the file map.
--
--   This is used to flush warnings between passes. The file map is kept
--   so that subsequent diagnostics still have source context available.
--
--   See 'runFixenPassFlushWarnings' for the primary use case.
fixenResetErrors :: FixenErrors -> FixenErrors
fixenResetErrors FixenErrors {fixenErrorsFileMap = ls} = fixenEmptyErrors ls

-- | Create a fresh 'FixenErrors' from a file map, with all diagnostics cleared.
--
--   The file map is re-attached to the empty diagnostic so that source
--   context is preserved.
fixenEmptyErrors :: FixenFileMap -> FixenErrors
fixenEmptyErrors ls =
  -- Fold over the file map, attaching each file to the empty diagnostic
  let d = foldl' (\r (fp, s) -> addFile r fp s) mempty ls
  in  FixenErrors {fixenErrorsFileMap = ls, fixenErrorsDiagnostic = d}

-- | Check whether a 'FixenErrors' contains any error reports (warnings are ignored).
--
--   This inspects the 'Diagnostic' and partitions its reports into errors
--   ('Report.Err') and non-errors (warnings, notes). Returns 'True' if
--   there is at least one error.
--
--   This is used by 'runFixenPass' to decide whether to short-circuit
--   after a pass completes successfully (i.e., when the pass returned
--   a value but errors were accumulated silently).
fixenHasErrors :: FixenErrors -> Bool
fixenHasErrors FixenErrors {fixenErrorsDiagnostic = b} =
  -- Extract all reports from the diagnostic
  let rep = reportsOf b
      -- Partition reports into errors (Err) and non-errors (warnings, notes)
      (errs, _) =
        partition
          ( \case
              Err {} -> True
              _ -> False
          )
          rep
  in  -- Return True if there is at least one error
      not (null errs)

-- | Combine two 'FixenErrors' by merging both their file maps and diagnostics.
--
--   This enables seamless error accumulation across multiple passes:
--   errors from earlier passes are preserved when later passes add their own.
instance Semigroup FixenErrors where
  (FixenErrors a b) <> (FixenErrors a' b') =
    FixenErrors (a <> a') (b <> b')

-- | The 'mempty' value has empty file map and empty diagnostics.
instance Monoid FixenErrors where
  mempty = FixenErrors mempty mempty

-- | The canonical runner for a compiler pass.
--
--   This is the main function used to execute a 'FixenPass' and convert
--   its result into the top-level 'FixenM' monad. It implements the
--   two-phase failure check:
--
--   1. If the pass short-circuited (returned 'Nothing' via the 'MaybeT'
--      layer), fail with the accumulated diagnostics
--   2. If the pass returned a value but the state contains errors
--      (checked via 'fixenHasErrors'), also fail with the accumulated diagnostics
--   3. Otherwise, return the result and the final state
--
--   This is the most strict runner: it treats both short-circuiting and
--   accumulated errors as fatal. Use 'runFixenPass'' or 'runFixenPass'''
--   for more lenient behavior.
--
--   See 'Fixen.Pipeline.parse' for the primary usage.
runFixenPass :: Errored σ => σ -> FixenPass σ α -> FixenM (α, σ)
runFixenPass state monad = do
  -- Unwrap the MaybeT and StateT layers, running the computation with the
  -- initial state. The result is (Maybe α, σ).
  (res, state') <- ExceptT $ Right <$> runStateT (runMaybeT monad) state
  -- Extract the FixenErrors from the resulting state
  let errs :: FixenErrors = (↓) state'
  case res of
    -- The pass short-circuited (Nothing) — fail with accumulated errors
    Nothing -> liftEither (Left (fixenErrorsDiagnostic errs))
    -- The pass returned a value — check if there are accumulated errors
    Just res' ->
      if fixenHasErrors errs
        then -- Errors were accumulated silently — fail with them
          liftEither (Left (fixenErrorsDiagnostic errs))
        else -- No errors — return the result and the final state
          return (res', state')

-- | Wrap a pass result in 'Just', converting a hard failure ('Nothing')
-- into a soft failure ('Just Nothing').
--
--   This is useful when you want to attempt a pass but treat failure
--   as a recoverable outcome rather than an immediate error. The caller
--   must then handle the 'Maybe' result explicitly.
--
--   For example, in 'Fixen.ModuleSystem.loop', 'fixenPassTry' is used to
--   attempt parsing an included file — if parsing fails, the file is
--   simply skipped rather than aborting the entire pass.
fixenPassTry :: FixenPass s a -> FixenPass s (Maybe a)
fixenPassTry fp = MaybeT $ Just <$> runMaybeT fp

-- | Like 'runFixenPass', but prints and flushes all diagnostics (including
-- warnings) after the pass completes.
--
--   The @error_printer@ argument is a function that renders diagnostics
--   to the console (or wherever appropriate). It is called with the full
--   diagnostic set before it is reset.
--
--   This is the runner used by the pipeline entry point for the parsing
--   and include-resolution phases, where warnings should be displayed
--   to the user but should not cause the pipeline to fail.
runFixenPassFlushWarnings
  :: Errored σ
  => (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -- ^ Function for printing diagnostics (e.g., to the console)
  -> σ
  -- ^ Initial state
  -> FixenPass σ α
  -- ^ Pass to run
  -> FixenM (α, σ)
  -- ^ Result: (pass output, state with errors reset)
runFixenPassFlushWarnings error_printer state monad = do
  -- Run the pass using the canonical runner
  (res, state') <- runFixenPass state monad
  -- Extract the FixenErrors from the resulting state
  let errs :: FixenErrors = (↓) state'
  -- Print all diagnostics to the user
  error_printer (fixenErrorsDiagnostic errs)
  -- Reset the diagnostics (keeping the file map) to flush warnings
  let new_errs = fixenResetErrors errs
  -- Update the state with the reset errors
  let new_state = state' *<-: new_errs
  return (res, new_state)

-- | Like 'runFixenPass', but does not check for accumulated errors.
--
--   The pass fails only if it short-circuited (returned 'Nothing'). If it
--   returned a value, the result is returned regardless of whether errors
--   were accumulated in the state.
--
--   Use this when you want to collect errors from a pass but continue
--   processing regardless. The caller is responsible for checking the
--   resulting state for errors.
runFixenPass' :: Errored σ => σ -> FixenPass σ α -> FixenM (α, σ)
runFixenPass' state monad = do
  -- Unwrap and run the pass with the initial state
  (res, state') <-
    ExceptT $
      Right
        <$> runStateT (runMaybeT monad) state
  -- Extract the FixenErrors from the resulting state
  let errs :: FixenErrors = (↓) state'
  case res of
    -- The pass short-circuited — fail with accumulated errors
    Nothing -> liftEither (Left (fixenErrorsDiagnostic errs))
    -- The pass returned a value — return it (ignore accumulated errors)
    Just res' -> return (res', state')

-- | Like 'runFixenPass', but fails with an empty diagnostic if the pass
-- short-circuited.
--
--   This is useful when the state does not contain 'FixenErrors' and you
--   only care about whether the pass returned a value or not.
runFixenPass'' :: σ -> FixenPass σ α -> FixenM (α, σ)
runFixenPass'' state monad = do
  -- Unwrap and run the pass with the initial state
  (res, state') <-
    ExceptT $
      Right
        <$> runStateT (runMaybeT monad) state
  case res of
    -- The pass short-circuited — fail with empty diagnostic
    Nothing -> liftEither (Left mempty)
    -- The pass returned a value — return it
    Just res' -> return (res', state')

-- | Fail the pass with a plain string message.
--
--   Converts the string into a 'Report.Err' with no source position,
--   no notes, and no extra context, then accumulates it and short-
--   circuits via 'mzero'.
--
--   This is the simplest way to fail a pass — use it for assertion
--   violations or internal errors where you don't have structured
--   diagnostic information.
failS
  :: (Errored σ, MonadState σ μ, MonadPlus μ)
  => String
  -- ^ The error message
  -> μ a
failS msg =
  failR (Err Nothing msg [] [])

-- | Fail the pass with a 'Report'.
--
--   This is the core failure function: it calls 'accumR' to add the
--   report to the error accumulator, then fails fast via 'mzero'.
--   Use this when you have a structured 'Report' (with positions, notes,
--   etc.) that you want to include in the diagnostic output.
failR
  :: (Errored σ, MonadState σ μ, MonadPlus μ)
  => Report String
  -- ^ The error report
  -> μ a
failR rep = accumR rep >> mzero

-- | Fail the pass with an error message. Internally, this calls 'failR'.
failErr
  :: (Errored σ, MonadState σ μ, MonadPlus μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  -> String
  -- ^ The error message, shown at the very top
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  -> μ a
failErr err_code err_msg markers notes = failR $ Err err_code err_msg markers notes

-- | Fail the pass with a 'Diagnostic'.
--
--   Like 'failR' but accepts a full 'Diagnostic' (which may contain
--   multiple reports). Use this when you have a pre-built diagnostic
--   object, such as one produced by the parser.
failD
  :: (Errored σ, MonadState σ μ, MonadPlus μ)
  => Diagnostic String
  -- ^ The diagnostic
  -> μ a
failD rep = accumD rep >> mzero

-- | Fail the pass if errors have been accumulated in the state.
--
--   This is a conditional failure: it checks whether the 'FixenErrors'
--   component of the state contains any error reports (warnings are
--   ignored). If errors are present, it short-circuits via 'mzero';
--   otherwise, it does nothing.
--
--   This is useful at the boundary between two logical sections of a
--   pass: you can accumulate errors freely in the first section, then
--   call 'failIfErrored' before proceeding to the second section.
failIfErrored
  :: (Errored σ, MonadState σ μ, MonadPlus μ)
  => μ ()
failIfErrored = do
  -- Get the current state
  fixen_state <- get
  -- Extract the FixenErrors from the state
  let errs :: FixenErrors = (↓) fixen_state
  -- If there are errors, short-circuit
  when (fixenHasErrors errs) mzero

-- | Add a 'Report' to the error accumulator __without failing__.
--
--   This is the non-fatal counterpart to 'failR'. It retrieves the
--   current state, extracts the 'FixenErrors', adds the report to the
--   diagnostic, and writes the updated state back.
--
--   Unlike 'failR', this does __not__ short-circuit the pass. Errors
--   accumulated via 'accumR' can be checked later by 'failIfErrored'
--   or by 'runFixenPass', which will fail if any errors are present.
--
--   This is the primary way to collect diagnostics during a pass —
--   multiple 'accumR' calls can be made, and the errors are combined
--   via the 'Semigroup' instance.
accumR
  :: (MonadState σ μ, Errored σ)
  => Report String
  -- ^ The report to add
  -> μ ()
accumR rep = do
  -- Get the current state
  fixen_state <- get
  -- Extract the FixenErrors from the state
  let errs :: FixenErrors = (↓) fixen_state
      -- Create a new FixenErrors with the report added to the diagnostic
      new_errs =
        errs
          { fixenErrorsDiagnostic =
              addReport (fixenErrorsDiagnostic errs) rep
          }
      -- Update the FixenErrors component in the state
      new_state = fixen_state *<-: new_errs
  -- Write the updated state back
  put new_state

-- | Add an error to the error accumulator __without failing__.
--
--   This is the non-fatal counterpart to 'failErr'. It retrieves the
--   current state, extracts the 'FixenErrors', adds the report to the
--   diagnostic, and writes the updated state back. Internally, it uses 'accumR'.
accumErr
  :: (Errored σ, MonadState σ μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  -> String
  -- ^ The error message, shown at the very top
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  -> μ ()
accumErr err_code err_msg markers notes = accumR $ Err err_code err_msg markers notes

-- | Add a warning to the error accumulator __without failing__. Internally,
-- it uses 'accumR'.
accumWarn
  :: (Errored σ, MonadState σ μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  -> String
  -- ^ The warning message, shown at the very top
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  -> μ ()
accumWarn err_code err_msg markers notes = accumR $ Warn err_code err_msg markers notes

-- | Add a 'Diagnostic' to the error accumulator __without failing__.
--
--   Like 'accumR' but accepts a full 'Diagnostic' (which may contain
--   multiple reports). The diagnostics are combined via the 'Semigroup'
--   instance ('<>').
--
--   This is used when you have a pre-built 'Diagnostic' object, such as
--   one produced by the parser's error reporting.
accumD
  :: (MonadState σ μ, Errored σ)
  => Diagnostic String
  -- ^ The diagnostic to add
  -> μ ()
accumD d = do
  -- Get the current state
  fixen_state <- get
  -- Extract the FixenErrors from the state
  let errs :: FixenErrors = (↓) fixen_state
      -- Create a new FixenErrors with the diagnostic combined
      new_errs = errs {fixenErrorsDiagnostic = fixenErrorsDiagnostic errs <> d}
      -- Update the FixenErrors component in the state
      new_state = fixen_state *<-: new_errs
  -- Write the updated state back
  put new_state
