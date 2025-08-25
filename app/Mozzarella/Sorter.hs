-- |
--     Module      : Mozzarella.Sorter
--     Description : Sorts top-level declarations of Mozzarella programs
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module sorts Mozzarella top-level declarations into buckets, and
--     verifies that
--
--     1. There is only one @extern@ declaration (why have more)?
--     2. There is at least one @rel@
--     3. There is at least one @rule@
--
--     Currently the error messages subtly do not ask the user to __declare__
--     relations/rules when they are not present. This so that in the future,
--     when Mozzarella programs can be made into modules and imported, the
--     error messages can suggest the programmer to import them.
module Mozzarella.Sorter (
  sort,
  putTopLevelInBuckets,
) where

import Control.Monad
import Error.Diagnose as Diag
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Sorted qualified as Sorted
import Mozzarella.Monad

type SorterM a = MozzarellaPass MozzarellaErrors a

-- | Sorts the top-level declarations into a 'SortedProgram'
sort
  :: AST.Program
  -- ^ The parsed AST
  -> SorterM Sorted.Program
sort AST.Program {AST.topLevels = top_levels} = do
  -- static-argument transform the folder for efficiency gains
  -- let go = putTopLevelInBuckets errs
  -- Run the fold on the empty program
  pgm <- foldM putTopLevelInBuckets (Sorted.Program Nothing [] []) top_levels
  -- Throw errors when there are no relations/rules
  when (null (Sorted.relations pgm)) $
    accumR
      ( Diag.Err
          Nothing
          "no relations found"
          []
          [Diag.Note "every program must contain relations"]
      )

  when (null (Sorted.rules pgm)) $
    accumR
      ( Diag.Err
          Nothing
          "no rules found"
          []
          [Diag.Note "every program must contain rules"]
      )

  -- The fold caused the relations and rules to be reversed, so we un-reverse
  -- them
  return $
    pgm
      { Sorted.relations = reverse (Sorted.relations pgm)
      , Sorted.rules =
          reverse (Sorted.rules pgm)
      }

-- | Puts a 'AST.TopLevel' into a 'SortedProgram'
putTopLevelInBuckets
  :: Sorted.Program
  -- ^ The intermediate sorted program
  -> AST.TopLevel
  -- ^ The top-level declaration to put into the sorted program
  -> SorterM Sorted.Program
putTopLevelInBuckets pgm (AST.TopLevelExtern e) = do
  let exts = Sorted.externs pgm
  case exts of
    Just x -> do
      -- There can only be one extern declaration. throw an error! here
      -- we show the current extern declaration and the one we already
      -- have as part of the error message.
      let pos_x = AST.getPosition x
          pos_e = AST.getPosition e
          err_fst_decl = (pos_x, Diag.Where "an extern declaration")
          err_snd_decl = (pos_e, Diag.This "another extern declaration")
      accumR
        ( Diag.Err
            Nothing
            "cannot have multiple extern declarations"
            [ err_fst_decl
            , err_snd_decl
            ]
            [Diag.Hint "merge these extern declarations"]
        )
      return pgm {Sorted.externs = Just e}
    Nothing -> return pgm {Sorted.externs = Just e}
-- Obvious.
putTopLevelInBuckets pgm (AST.TopLevelRule r) =
  return pgm {Sorted.rules = r : Sorted.rules pgm}
putTopLevelInBuckets pgm (AST.TopLevelRelation r) =
  return pgm {Sorted.relations = r : Sorted.relations pgm}
