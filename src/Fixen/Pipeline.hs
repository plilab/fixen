-- |
--     Module      : Fixen.Pipeline
--     Description : The Fixen pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
module Fixen.Pipeline (pipeline) where

import Control.Monad.IO.Class (MonadIO)
import Data.IntMap.Strict qualified as Map
import Data.Text
import Error.Diagnose
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.ModuleSystem (getIncludes)
import Fixen.Monad
import Fixen.Parser (parse)
import Prettyprinter

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
      init_errs = fixenEmptyErrors file_map
      init_pos_env :: PositionEnv = Map.empty
      init_node_id :: NodeId = 0
      init_env = (init_pos_env, (init_node_id, init_errs))
  (program, st) <- runFixenPassFlushWarnings error_printer init_env (parse file_path (pack contents))
  -- NOTE THAT IN SUBSEQUENT PASSES WE MUST USE THE RESULTING STATE THAT HAS THE NEW FILEMAP
  (program', _) <- runFixenPassFlushWarnings error_printer st (getIncludes program)
  return program'
