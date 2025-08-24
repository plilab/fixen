-- |
--     Module      : Mozzarella.Pipeline
--     Description : The Mozzarella pipeline
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
module Mozzarella.Pipeline (pipeline) where

import Data.Text
import Mozzarella.BoundVarExplicitor (makeBoundVarsExplicit)
import Mozzarella.IR.ExplicitBoundVars qualified as Explicit
import Mozzarella.Monad
import Mozzarella.Parser (parse)
import Mozzarella.Sorter (sort)

-- | The compilation pipeline. It consists of several phases:
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
  -> MozzarellaM Explicit.Program
pipeline file_path contents = do
  program <- parse file_path (pack contents)
  program' <- sort file_path contents program
  return $ makeBoundVarsExplicit program'
