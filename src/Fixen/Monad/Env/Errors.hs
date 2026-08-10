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
--     * 'FixenErrors'—the error accumulator carried in the pass state, which
--       holds both a 'Diagnostic' and a file map for context
--
--     * 'WithErrors'—a type alias for states that contain 'FixenErrors'
--
--     * 'runFixenPass'—executes a 'FixenPass' and converts the result into
--       'FixenM'
--
--     * 'failS', 'failR', 'failD', 'failErr', 'failIfErrored'—helpers for failing
--       a pass (with or without accumulating errors first)
--
--     * 'accumR', 'accumD', 'accumErr', 'accumWarn'—helpers for adding
--       diagnostics / reports to the error accumulator without immediately
--       failing
--
--     * 'fixenInsertFileMap', 'fixenResetErrors', 'fixenEmptyErrors'—utilities
--       for managing the file map and error state
--
--     The overall error model supports two modes:
--
--     * /Fail-fast/ — calling any 'fail*' function immediately short-circuits
--       the pass via 'mzero' in the 'MaybeT' layer, after optionally
--       accumulating the error in the state first.
--
--     * /Error accumulation/ — calling 'accumR' or 'accumD' adds errors
--       to the state without failing, allowing a pass to collect multiple
--       diagnostics before terminating. 'runFixenPass' checks
--       for accumulated errors and fails if any are present.
--
-- @since 26.7
module Fixen.Monad.Env.Errors (
  -- * Errors
  FixenFileMap,
  FixenErrors (..),
  WithErrors,

  -- * Working Within the Errored Monad
  getFixenErrors,
  setFixenErrors,
  insertFileMap,
  runFixenPass,
  fixenTry,
  failS,
  failR,
  failErr,
  failD,
  failIfErrored,
  accumR,
  accumErr,
  accumWarn,
  accumD,

  -- * Working with 'FixenErrors'
  resetErrors,
  emptyErrors,
  fixenHasErrors,

  -- * Re-exports From Diagnose
  Diagnostic,
  Position (..),
  Marker (..),
  Note (..),
  Report (..),
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.List
import Error.Diagnose.Diagnostic
import Error.Diagnose.Position
import Error.Diagnose.Report
import Fixen.Fields
import Fixen.Monad.Type
import Fixen.Utils
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
--
-- @since 26.7
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
-- @since 26.7
data FixenErrors = FixenErrors
  { fixenErrorsFileMap :: FixenFileMap
  -- ^ The file map providing source context for diagnostic messages.
  --   Files are stored as (path, contents) pairs.
  --
  -- @since 26.7
  , fixenErrorsDiagnostic :: FixenErrorResult
  -- ^ The accumulated diagnostic messages. This includes both errors
  --   and warnings; use 'fixenHasErrors' to distinguish between them.
  --
  -- @since 26.7
  }

instance HasFileMap FixenErrors FixenFileMap where
  fileMap = lens fixenErrorsFileMap (\s i -> s {fixenErrorsFileMap = i})

instance HasDiagnostic FixenErrors FixenErrorResult where
  diagnostic = lens fixenErrorsDiagnostic (\s i -> s {fixenErrorsDiagnostic = i})

instance Semigroup FixenErrors where
  (FixenErrors a b) <> (FixenErrors a' b') =
    FixenErrors (a <> a') (b <> b')

instance Monoid FixenErrors where
  mempty = FixenErrors mempty mempty

-- | Constraint alias indicating that a state type @σ@ contains 'FixenErrors'.
--
-- @since 26.7
type WithErrors σ = σ :>: FixenErrors

-------------------------------------------------------------------------------

-- Working Within the Errored Monad

-------------------------------------------------------------------------------

-- | Gets the 'FixenErrors' from the state.
--
-- @since 26.7
getFixenErrors :: (WithErrors σ, MonadState σ μ) => μ FixenErrors
getFixenErrors = do
  st <- get
  return $ (↓) st

-- | Sets a new 'FixenErrors' to the state.
--
-- @since 26.7
setFixenErrors :: (WithErrors σ, MonadState σ μ) => FixenErrors -> μ ()
setFixenErrors e = do
  st <- get
  let st' = st *<-: e
  put st'

{- FOURMOLU_DISABLE -}
-- | Insert a file into the error accumulator's file map.
--
-- @since 26.7
insertFileMap
  :: (WithErrors σ, MonadState σ μ)
  => FilePath
  -- ^ The file's path
  --
  -- @since 26.7
  -> String
  -- ^ The file's contents
  --
  -- @since 26.7
  -> μ ()
insertFileMap fp s = do
  errs <- getFixenErrors
  let new_errs =
        errs
          & fileMap    %~ ((fp, s) :)
          & diagnostic %~ (\i -> addFile i fp s)
  setFixenErrors new_errs
{- FOURMOLU_ENABLE -}

-- | The canonical runner for a compiler pass.
--
-- @since 26.7
runFixenPass
  :: WithErrors σ
  => (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -- ^ Function for printing diagnostics (e.g., to the console)
  --
  -- @since 26.7
  -> σ
  -- ^ The state to run with
  --
  -- @since 26.7
  -> FixenPass σ α
  -- ^ The compiler pass
  --
  -- @since 26.7
  -> FixenM (α, σ)
runFixenPass print_errs st monad = do
  (res, state') <- ExceptT $ Right <$> runStateT (runMaybeT monad) st
  let errs :: FixenErrors = (↓) state'
  case res of
    Nothing -> liftEither (Left (fixenErrorsDiagnostic errs))
    Just res' ->
      if fixenHasErrors errs
        then liftEither (Left (fixenErrorsDiagnostic errs))
        else do
          errs ^. diagnostic & print_errs
          let new_errs = resetErrors errs
          let new_state = state' *<-: new_errs
          return (res', new_state)

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
--
-- @since 26.7
fixenTry :: FixenPass s a -> FixenPass s (Maybe a)
fixenTry fp = MaybeT $ Just <$> runMaybeT fp

-- | Fail the pass with a plain string message.
--
--   This is the simplest way to fail a pass—use it for assertion
--   violations or internal errors where you don't have structured
--   diagnostic information.
--
-- @since 26.7
failS
  :: (WithErrors σ, MonadState σ μ, MonadPlus μ)
  => String
  -- ^ The error message
  -> μ a
failS msg =
  failR (Err Nothing msg [] [])

-- | Fail the pass with a 'Report'.
--
-- @since 26.7
failR
  :: (WithErrors σ, MonadState σ μ, MonadPlus μ)
  => Report String
  -- ^ The error report
  -> μ a
failR rep = accumR rep >> mzero

-- | Fail the pass with an error message. Internally, this calls 'failR'.
--
-- @since 26.7
failErr
  :: (WithErrors σ, MonadState σ μ, MonadPlus μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  --
  -- @since 26.7
  -> String
  -- ^ The error message, shown at the very top
  --
  -- @since 26.7
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  --
  -- @since 26.7
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  --
  -- @since 26.7
  -> μ a
failErr err_code err_msg markers notes = failR $ Err err_code err_msg markers notes

-- | Fail the pass with a 'Diagnostic'.
--
-- @since 26.7
failD
  :: (WithErrors σ, MonadState σ μ, MonadPlus μ)
  => Diagnostic String
  -- ^ The diagnostic
  --
  -- @since 26.7
  -> μ a
failD rep = accumD rep >> mzero

-- | Fail the pass if errors have been accumulated in the state.
--
--   This is useful at the boundary between two logical sections of a
--   pass: you can accumulate errors freely in the first section, then
--   call 'failIfErrored' before proceeding to the second section.
--
-- @since 26.7
failIfErrored
  :: (WithErrors σ, MonadState σ μ, MonadPlus μ)
  => μ ()
failIfErrored = do
  errs <- getFixenErrors
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
--   This is the primary way to collect diagnostics during a pass—multiple
--   'accumR' calls can be made, and the errors are combined via the 'Semigroup'
--   instance.
--
-- @since 26.7
accumR
  :: (MonadState σ μ, WithErrors σ)
  => Report String
  -- ^ The report to add
  --
  -- @since 26.7
  -> μ ()
accumR rep = do
  -- Get the current state
  --
  -- @since 26.7
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
--
-- @since 26.7
accumErr
  :: (WithErrors σ, MonadState σ μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  --
  -- @since 26.7
  -> String
  -- ^ The error message, shown at the very top
  --
  -- @since 26.7
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  --
  -- @since 26.7
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  --
  -- @since 26.7
  -> μ ()
accumErr err_code err_msg markers notes = accumR $ Err err_code err_msg markers notes

-- | Add a warning to the error accumulator __without failing__. Internally,
-- it uses 'accumR'.
--
-- @since 26.7
accumWarn
  :: (WithErrors σ, MonadState σ μ)
  => Maybe String
  -- ^ Optional error code to be shown right next to \"error\" or \"warning\"
  --
  -- @since 26.7
  -> String
  -- ^ The warning message, shown at the very top
  --
  -- @since 26.7
  -> [(Position, Marker String)]
  -- ^ A list associating positions with markers
  --
  -- @since 26.7
  -> [Note String]
  -- ^ A potentially empty list of notes/hints added to the end of the report
  -- ^ The error report
  --
  -- @since 26.7
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
--
-- @since 26.7
accumD
  :: (MonadState σ μ, WithErrors σ)
  => Diagnostic String
  -- ^ The diagnostic to add
  --
  -- @since 26.7
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

-------------------------------------------------------------------------------

-- * Working with 'FixenErrors'

-------------------------------------------------------------------------------

-- | Reset all diagnostics in a 'FixenErrors' while preserving the file map.
--
-- @since 26.7
resetErrors :: FixenErrors -> FixenErrors
resetErrors FixenErrors {fixenErrorsFileMap = ls} = emptyErrors ls

-- | Create a fresh 'FixenErrors' from a file map, with all diagnostics cleared.
--
-- @since 26.7
emptyErrors :: FixenFileMap -> FixenErrors
emptyErrors ls =
  let d = foldl' (\r (fp, s) -> addFile r fp s) mempty ls
   in FixenErrors {fixenErrorsFileMap = ls, fixenErrorsDiagnostic = d}

-- | Determines if a 'FixenErrors' contains any error reports (warnings are ignored).
--
-- @since 26.7
fixenHasErrors :: FixenErrors -> Bool
fixenHasErrors FixenErrors {fixenErrorsDiagnostic = b} =
  let rep = reportsOf b
      (errs, _) =
        partition
          ( \case
              Err {} -> True
              _ -> False
          )
          rep
   in (¬) (null errs)
