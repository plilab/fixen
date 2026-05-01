module Fixen.SymbolSolver where

import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.PartialOrdDeclaration
import Fixen.SymbolSolver.Phases
import Fixen.SymbolSolver.Query
import Fixen.SymbolSolver.Relation
import Fixen.SymbolSolver.Rule

solveSymbols :: Program -> FixenPass SymbolState SymbolEnv
solveSymbols prog = do
  env_with_rels_and_pords <-
    pure emptySymbolEnv
      -- gather partial ord declarations
      >>= foldMWith initEnvWithPartialOrd (partialOrdDeclarations prog)
      -- gather relations
      >>= foldMWith initEnvWithRelation (relations prog)
  -- flush all errors, and continue. Now, we are certain that the relations and partialOrdDeclarations
  -- are correctly added to the symbol environment.
  failIfErrored
  env_with_queries_and_rules <-
    pure env_with_rels_and_pords
      >>= foldMWith initEnvWithQuery (queries prog)
      >>= foldMWith initEnvWithRule (rules prog)
  failIfErrored
  env_with_phases <-
    pure env_with_queries_and_rules
      >>= initEnvWithPhases (phases prog)
  -- TODO: Priorities + check for unused rule params
  return env_with_phases
