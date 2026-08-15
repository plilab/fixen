-- |
-- Module      : Fixen.Pipeline
-- Description : The Fixen pipeline
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- The Fixen compiler pipeline. The pipeline performs the following:
--
-- 1. Parses the file contents ('parse'), giving a 'Program'
-- 2. Imports other Fixen programs included via 'Include' statements
--    ('getIncludes'), adding more definitions to the 'Program'
-- 3. Performs symbol solving and type checking ('solveSymbols'), initializing
--    the 'SymbolEnv'. The 'SymbolEnv' contains all the core information about
--    the 'Program', essentially it is the source of truth for information of
--    the 'Program'.
-- 4. Builds the 'RuleForest' from the 'Program' ('getRuleForest') (strictly
--    speaking, it builds a 'RuleForest' for each phase in the 'Program').
-- 5. Obtains the 'RelationRepresentation' for 'RelationDeclaration's in the
--    'Program' ('getRelationRepresentation').
-- 6. Performs code generation ('codeGen'), yielding 'Text' to write to the
--    output file.
--
-- Each step in the pipeline is fail-fast ('FixenM' is a fail-fast monad!).
-- Therefore, as long as errors are emitted by a step, the pipeline terminates,
-- yielding errors. Note that a step may choose not to be
-- fail-fast internally ('FixenPass' is not necessarily fail-fast).
--
-- Some passes will emit warnings without causing the pipeline to terminate.
-- These are automatically flushed to stderr (see 'runFixenPass').
module Fixen.Pipeline (pipeline, pipelineWithSymbols) where

import Control.Monad.IO.Class (MonadIO)
import Data.IntMap.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty)
import Data.Text
import Error.Diagnose
import Fixen.CodeGen
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.ModuleSystem (getIncludes)
import Fixen.Monad
import Fixen.Parser (parse)
import Fixen.RelationRepresentation
import Fixen.RuleForest
import Fixen.SymbolSolver
import Fixen.Utils
import Prettyprinter

-- | The main compiler pipeline.
--
-- @since 26.7
pipeline
  :: FilePath
  -- ^ The path of the compiled file
  --
  -- @since 26.7
  -> String
  -- ^ The contents of the compiled file
  --
  -- @since 26.7
  -> (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -- ^ The warnings printer
  --
  -- @since 26.7
  -> FixenM (Program, NonEmpty RuleForest, RelationRepresentation, Text)
pipeline file_path contents error_printer = do
  let file_map = [(file_path, contents)]
      init_errs = emptyErrors file_map
      init_pos_env = Map.empty
      init_node_id = 0
      init_env = init_pos_env × init_node_id × init_errs
  (program, st) <- run init_env (parse file_path (pack contents))
  (program', st') <- run st (getIncludes program)
  (env, st'') <- run st' (solveSymbols program')
  (rt, st''') <- run (env, st'') (getRuleForest program')
  (db, _) <- run st''' getRelationRepresentation
  (t, _) <- run st''' (codeGen rt db program')
  return (program', rt, db, t)
  where
    run :: WithErrors a => a -> FixenPass a b -> FixenM (b, a)
    run = runFixenPass error_printer

pipelineWithSymbols
  :: FilePath
  -> String
  -> (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -> FixenM
      ( Program
      , SymbolEnv
      , NonEmpty RuleForest
      , RelationRepresentation
      , Text
      )
pipelineWithSymbols file_path contents error_printer = do
  let file_map = [(file_path, contents)]
      init_errs = emptyErrors file_map
      init_pos_env = Map.empty
      init_node_id = 0
      init_env = init_pos_env × init_node_id × init_errs
  (program, st) <- run init_env (parse file_path (pack contents))
  (program', st') <- run st (getIncludes program)
  (env, st'') <- run st' (solveSymbols program')
  (rt, st''') <- run (env, st'') (getRuleForest program')
  (db, _) <- run st''' getRelationRepresentation
  (t, _) <- run st''' (codeGen rt db program')
  return (program', env, rt, db, t)
  where
    run :: WithErrors a => a -> FixenPass a b -> FixenM (b, a)
    run = runFixenPass error_printer
