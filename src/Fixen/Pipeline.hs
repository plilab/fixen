-- |
--     Module      : Fixen.Pipeline
--     Description : The Fixen pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
module Fixen.Pipeline (pipeline) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text
import Error.Diagnose
import Fixen.IR.AST

-- import Fixen.BoundVarExplicitor (makeBoundVarsExplicit)
import Fixen.Monad
import Fixen.Parser (parse)

-- import Fixen.Sorter (sort)
-- import Fixen.SymbolSolver

-- | The compilation pipeline. The code as a connector for each phase of
-- the pipeline (which may use different monads):
--
-- 1. 'parse': Parses the program, yielding a 'Fixen.IR.AST.Program'.
--
-- 2. 'sort': Puts the top-level declarations of the 'Fixen.IR.AST.Program'
--            into their respective buckets in a 'Fixen.IR.Sorted.Program'
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
  -> FixenM Program
pipeline file_path contents = do
  let file_map = [(file_path, contents)]
      init_errs = mozEmptyErrors file_map
  (program, err_after1) <- runFixenPass init_errs $ parse file_path (pack contents)
  -- (program', err_after2) <- runFixenPass err_after1 $ sort program
  -- let pp = makeBoundVarsExplicit program'
  -- (env, ess) <- runFixenPass err_after2 $ solveSymbols pp
  when (hasReports (mozErrorsDiagnostic err_after1)) $
    liftIO $
      printDiagnostic stderr WithUnicode (TabSize 4) defaultStyle (mozErrorsDiagnostic err_after1)
  return program
