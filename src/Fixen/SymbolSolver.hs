-- |
-- Module      : Fixen.SymbolSolver
-- Description : Symbol solving for the Fixen compiler
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides the main entry point 'solveSymbols'.
--
-- @since 26.7
module Fixen.SymbolSolver where

import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Lattice
import Fixen.SymbolSolver.PartialOrdDeclaration
import Fixen.SymbolSolver.Phases
import Fixen.SymbolSolver.Priorities
import Fixen.SymbolSolver.Query
import Fixen.SymbolSolver.Relation
import Fixen.SymbolSolver.Rule
import Fixen.SymbolSolver.Validation
import Fixen.Utils

--------------------------------------------------------------------------------

-- * The Symbol Solver

--------------------------------------------------------------------------------

-- | Solves the symbols in the program, obtaining a 'SymbolEnv'.
--
-- The symbol solver obtains information about the symbols in the program, and
-- \"canonicalizes\" the program so that any information we can possibly need
-- is computed. Invariants about the program are also maintained, and errors
-- and warnings are emitted whenever the invariants are violated.
--
-- The invariants for each syntactic category are found in the submodules of
-- the symbol solver.
--
-- @since 26.7
solveSymbols
  :: SymbolState σ
  => Program
  -- ^ The program whose symbols are being solved
  --
  -- @since 26.7
  -> FixenPass σ SymbolEnv
solveSymbols prog = do
  env_with_rels_and_pords <-
    foldMWith initEnvWithPartialOrd (prog ^. partialOrdDeclarations) emptySymbolEnv
      >>= foldMWith initEnvWithLattice (prog ^. latticeDeclarations)
      >>= foldMWith initEnvWithRelation (prog ^. relationDeclarations)
  -- flush all errors, and continue. Now, we are certain that the relations and
  -- partialOrdDeclarations are correctly added to the symbol environment.
  failIfErrored
  env_with_queries_and_rules <-
    foldMWith initEnvWithQuery (prog ^. queries) env_with_rels_and_pords
      >>= foldMWith initEnvWithRule (prog ^. rules)
  -- flush all errors, and continue. Queries and rules have been added.
  failIfErrored
  env_with_phases_and_priorities <-
    foldMWith initEnvWithPriorities (prog ^. priorities) env_with_queries_and_rules
      >>= initEnvWithPhases (prog ^. phases)
  -- flush all errors, and continue.
  failIfErrored
  -- now that everything has been added, we can warn for unused rule parameters
  warnUnusedRuleParameters env_with_phases_and_priorities
  return env_with_phases_and_priorities
