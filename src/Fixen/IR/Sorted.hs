-- |
--     Module      : Fixen.Sorted
--     Description : AST programs sorted into buckets
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     A program in this context is sorted into the right top-level
--     declarations. The invariants are that:
--
--     1. There must be no more than one extern declaration
--     2. There is a nonempty set of relations
--     3. There is a nonempty set of rules
module Fixen.IR.Sorted (Program (..)) where

import Fixen.IR.AST qualified as AST

-- | A 'Program' in this context is split into the different kinds of
-- 'AST.TopLevel' declarations.
data Program = Program
  { externs :: Maybe AST.Extern
  , relations :: [AST.Relation]
  , rules :: [AST.Rule]
  }
  deriving (Show, Eq)
