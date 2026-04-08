-- |
--     Module      : Fixen.Pipeline
--     Description : The Fixen pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
module Fixen.Pipeline (pipeline) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text
import Error.Diagnose
import Fixen.IR.AST
import Prettyprinter

-- import Fixen.BoundVarExplicitor (makeBoundVarsExplicit)

import Fixen.ModuleSystem (getIncludes)
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
  -> (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -> FixenM Program
pipeline file_path contents error_printer = do
  let file_map = [(file_path, contents)]
      init_errs = mozEmptyErrors file_map
  (program, _) <- runFixenPassFlushWarnings error_printer init_errs (parse file_path (pack contents))
  -- NOTE THAT IN SUBSEQUENT PASSES WE MUST USE THE RESULTING STATE THAT HAS THE NEW FILEMAP
  (program', _) <- runFixenPassFlushWarnings error_printer init_errs (getIncludes program)
  -- (program', err_after2) <- runFixenPass err_after1 $ sort program
  -- let pp = makeBoundVarsExplicit program'
  -- (env, ess) <- runFixenPass err_after2 $ solveSymbols pp
  return program'
