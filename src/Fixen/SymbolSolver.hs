-- |
-- Module      : Fixen.SymbolSolver
-- Description : Symbol solving for the Fixen compiler
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides the main entry point 'solveSymbols'.
module Fixen.SymbolSolver where

import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.PartialOrdDeclaration
import Fixen.SymbolSolver.Phases
import Fixen.SymbolSolver.Priorities
import Fixen.SymbolSolver.Query
import Fixen.SymbolSolver.Relation
import Fixen.SymbolSolver.Rule
import Fixen.SymbolSolver.Validation

-- | Solves the symbols in the program, obtaining a 'SymbolEnv'.
--
-- The symbol solver obtains information about the symbols in the program, and
-- \"canonicalizes\" the program so that any information we can possibly need
-- is computed. Invariants about the program are also maintained, and errors
-- and warnings are emitted whenever the invariants are violated.
--
-- The invariants for each syntactic category are found in the submodules of
-- the symbol solver.
solveSymbols :: Program -> FixenPass SymbolState SymbolEnv
solveSymbols prog = do
  env_with_rels_and_pords <-
    pure emptySymbolEnv
      >>= foldMWith initEnvWithPartialOrd (prog ^. partialOrdDeclarations)
      >>= foldMWith initEnvWithRelation (prog ^. relationDeclarations)
  -- flush all errors, and continue. Now, we are certain that the relations and
  -- partialOrdDeclarations are correctly added to the symbol environment.
  failIfErrored
  env_with_queries_and_rules <-
    pure env_with_rels_and_pords
      >>= foldMWith initEnvWithQuery (prog ^. queries)
      >>= foldMWith initEnvWithRule (prog ^. rules)
  -- flush all errors, and continue. Queries and rules have been added.
  failIfErrored
  env_with_phases_and_priorities <-
    pure env_with_queries_and_rules
      >>= foldMWith initEnvWithPriorities (prog ^. priorities)
      >>= initEnvWithPhases (prog ^. phases)
  -- flush all errors, and continue.
  failIfErrored
  -- now that everything has been added, we can warn for unused rule parameters
  warnUnusedRuleParameters env_with_phases_and_priorities
  return env_with_phases_and_priorities
