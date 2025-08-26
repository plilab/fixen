-- |
--     Module      : Mozzarella.Pipeline
--     Description : The Mozzarella pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
module Mozzarella.Pipeline (pipeline) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text
import Error.Diagnose
import Mozzarella.BoundVarExplicitor (makeBoundVarsExplicit)
import Mozzarella.IR.ExplicitBoundVars qualified as Explicit
import Mozzarella.Monad
import Mozzarella.Parser (parse)
import Mozzarella.Sorter (sort)
import Mozzarella.SymbolSolver

-- | The compilation pipeline. The code as a connector for each phase of
-- the pipeline (which may use different monads):
--
-- 1. 'parse': Parses the program, yielding a 'Mozzarella.IR.AST.Program'.
--
-- 2. 'sort': Puts the top-level declarations of the 'Mozzarella.IR.AST.Program'
--            into their respective buckets in a 'Mozzarella.IR.Sorted.Program'
--            and ensures
--
--              a. There is at most one extern declaration
--
--              b. There is at least one relation
--
--              c. There is at least one rule.
pipeline
  :: FilePath
  -- ^ The path of the compiled file
  -> String
  -- ^ The contents of the compiled file
  -> MozzarellaM SymbolEnv
pipeline file_path contents = do
  let file_map = [(file_path, contents)]
      init_errs = mozEmptyErrors file_map
  (program, err_after) <- runMozzarellaPass init_errs $ parse file_path (pack contents)
  (program', err_after) <- runMozzarellaPass err_after $ sort program
  let pp = makeBoundVarsExplicit program'
  (env, ess) <- runMozzarellaPass err_after $ solveSymbols pp
  when (hasReports (mozErrorsDiagnostic ess)) $
    liftIO $
      printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle (mozErrorsDiagnostic ess)
  return env
