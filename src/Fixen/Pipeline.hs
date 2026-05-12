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
import Prettyprinter

{- FOURMOLU_DISABLE -}
pipeline
  :: FilePath
  -- ^ The path of the compiled file
  -> String
  -- ^ The contents of the compiled file
  -> (forall m msg. (MonadIO m, Pretty msg) => Diagnostic msg -> m ())
  -> FixenM (Program, NonEmpty RuleForest, RelationRepresentation, Text)
pipeline file_path contents error_printer = do
  let file_map     = [(file_path, contents)]
      init_errs    = emptyErrors file_map
      init_pos_env = Map.empty
      init_node_id = 0
      init_env     = (init_pos_env, (init_node_id, init_errs))
  (program, st)   <- run init_env    (parse file_path (pack contents))
  (program', st') <- run st          (getIncludes program)
  (env, st'')     <- run st'         (solveSymbols program')
  (rt, st''')     <- run (env, st'') (getRuleForest program')
  (db, _)         <- run st'''       getDatabaseRepresentation
  (t, _)          <- run st'''       (codeGen rt db program')
  return (program', rt, db, t)
  where
    run :: WithErrors a => a -> FixenPass a b -> FixenM (b, a)
    run = runFixenPass error_printer
{- FOURMOLU_ENABLE -}
