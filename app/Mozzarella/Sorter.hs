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

-- | Sorts the top-level declarations into a 'SortedProgram'
sort
  :: FilePath
  -- ^ The file path of the program used for showing errs
  -> String
  -- ^ The file contents for showing errs
  -> AST.Program
  -- ^ The parsed AST
  -> MozzarellaM Sorted.Program
sort file_path contents AST.Program {AST.topLevels = top_levels} = do
  -- static-argument transform the folder for efficiency gains
  let go = putTopLevelInBuckets file_path contents
  -- Run the fold on the empty program
  pgm <- foldM go (Sorted.Program Nothing [] []) top_levels
  -- Throw errors when there are no relations/rules
  when (null (Sorted.relations pgm)) $
    mozzarellaError $
      Diag.addReport (Diag.addFile mempty file_path contents) $
        Diag.Err
          Nothing
          "no relations found"
          []
          [Diag.Note "every program must contain relations"]
  when (null (Sorted.rules pgm)) $
    mozzarellaError $
      Diag.addReport (Diag.addFile mempty file_path contents) $
        Diag.Err
          Nothing
          "no rules found"
          []
          [Diag.Note "every program must contain rules"]
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
  :: FilePath
  -- ^ The file path of the program used for showing errors
  -> String
  -- ^ The file contents for showing errors
  -> Sorted.Program
  -- ^ The intermediate sorted program
  -> AST.TopLevel
  -- ^ The top-level declaration to put into the sorted program
  -> MozzarellaM Sorted.Program
putTopLevelInBuckets file_path contents pgm (AST.TopLevelExtern e) = do
  let exts = Sorted.externs pgm
  case exts of
    Just x ->
      -- There can only be one extern declaration. throw an error! here
      -- we show the current extern declaration and the one we already
      -- have as part of the error message.
      let pos_x = AST.getPosition x
          pos_e = AST.getPosition e
          err_fst_decl = (pos_x, Diag.Where "an extern declaration")
          err_snd_decl = (pos_e, Diag.This "a second extern declaration")
      in  mozzarellaError $
            Diag.addReport (Diag.addFile mempty file_path contents) $
              Diag.Err
                Nothing
                "cannot have multiple extern declarations"
                [ err_fst_decl
                , err_snd_decl
                ]
                [Diag.Hint "merge the two extern declarations"]
    Nothing -> return pgm {Sorted.externs = Just e}
-- Obvious.
putTopLevelInBuckets _ _ pgm (AST.TopLevelRule r) =
  return pgm {Sorted.rules = r : Sorted.rules pgm}
putTopLevelInBuckets _ _ pgm (AST.TopLevelRelation r) =
  return pgm {Sorted.relations = r : Sorted.relations pgm}
