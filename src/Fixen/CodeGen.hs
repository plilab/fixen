{-# LANGUAGE OverloadedStrings #-}

-- | Haskell code-generation entry point. Rule matching, queries and solver
-- control flow live in separate modules; embedded Haskell stays opaque.
module Fixen.CodeGen (codeGen) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Database
import Fixen.CodeGen.Debug (codeGenDebugDefinitions)
import Fixen.CodeGen.Fact
import Fixen.CodeGen.Haskell.Match
import Fixen.CodeGen.Haskell.Query
import Fixen.CodeGen.Haskell.Solver
import Fixen.CodeGen.HsBlock
import Fixen.CodeGen.Import
import Fixen.CodeGen.ModuleDeclaration
import Fixen.CodeGen.MultiPhase
import Fixen.CodeGen.RuleInstance
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.IR.RuleForest
import Fixen.Monad

codeGen :: CodeGenOptions -> NonEmpty RuleForest -> RelationRepresentation -> Program -> FixenPass CodeGenState Text
codeGen options forests layouts program = do
  database <- codeGenDb layouts
  instances <- codeGenRuleInstance
  step <- codeGenStep options forests layouts
  stepAll <- codeGenStepAll
  solver <- codeGenLoopAndSolve options
  reSolve <- codeGenReSolve forests
  queries <- mapM (codeGenQuery layouts) (programQueries program)
  phases <- codeGenMultiPhase
  pure $
    Text.intercalate "\n\n" $
      [ codeGenModuleDeclaration program
      , codeGenImports options program
      , codeGenHsBlocks program
      , codeGenFacts layouts
      , database
      , instances
      , codeGenDebugDefinitions options
      , "----- STEP FUNCTION -----"
      , step
      , stepAll
      , "----- SOLVER -----"
      , solver
      , reSolve
      , "----- QUERIES -----"
      ]
        ++ queries
        ++ [phases]
