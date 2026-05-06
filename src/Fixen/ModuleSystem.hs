-- |
--     Module      : Fixen.ModuleSystem
--     Description : Resolves @include@ statements and merges multi-file Fixen programs
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module handles /include resolution/ for Fixen programs. When a
--     Fixen source file contains @include@ statements, this module:
--
--     1. /Resolves/ each include path to a canonical absolute file path
--     2. /Recursively parses/ each included file
--     3. /Merges/ all parsed programs into a single unified 'AST.Program'
--
--     The module also deduplicates imports, tracks visited files to avoid
--     infinite cycles, and accumulates errors (e.g. missing files) without
--     failing fast so that all errors can be reported together.
--
--     /Key functions./
--
--     * 'getIncludes' — the main entry point; processes all includes in a program
--     * 'resolveIncludeToPath' — resolves a relative include path to an absolute path
--     * 'loop' — recursive include processing loop
--     * 'combineProgram' — merges two programs (rules, relations, etc.)
--     * 'safeReadFile' — reads a file with graceful error handling
module Fixen.ModuleSystem where

import Control.Exception qualified as Exception
import Control.Monad (when)
import Control.Monad.State
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Error.Diagnose.Report
import Fixen.Data.NodeId (NodeId)
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core qualified as Core
import Fixen.Monad
import Fixen.Parser (parse)
import System.Directory (canonicalizePath)
import System.FilePath
import System.IO (readFile')

-- | Process all @include@ statements in a parsed Fixen program.
--
--   This is the main entry point for the module system. Given a program
--   that has been freshly parsed from a single file, it:
--
--   1. /Extracts/ the canonical file path of the source file from the
--      error-tracking file map (which records which file each parse
--      result came from)
--   2. /Resolves/ all include paths in the program to absolute canonical
--      paths using 'resolveIncludeToPath'
--   3. /Recursively processes/ each include via 'loop' — reading, parsing,
--      and combining included files
--
--   The result is a single unified 'AST.Program' containing all rules,
--   relations, and other constructs from the original file and all
--   transitive includes.
--
--   /Checks./ The result is also checked for having the minimum declarations
--   required to generate a solver, using 'highLevelStructuralChecks'.
--
--   /Error handling./ If the file map is empty or contains more than one
--   entry, this function fails with an assertion error (this should never
--   happen in normal operation since the parser always records exactly one
--   file per parse result).
getIncludes :: AST.Program -> FixenPass (PositionEnv :*: NodeId :*: FixenErrors) AST.Program
getIncludes in_program = do
  -- Step 1: Extract the canonical file path of the source file from the
  -- error-tracking file map. The parser records the file path for each
  -- parse result, so we can look it up here.
  file_path_of_in_program <- do
    file_map <- gets (fixenErrorsFileMap . (↓))
    case file_map of
      -- This should never happen — the parser always records at least one file.
      [] -> failS "assertion violation: file map is empty!"
      -- Normal case: exactly one file entry.
      [(file_path, _)] -> liftIO $ canonicalizePath file_path
      -- This should never happen — the parser records exactly one file per result.
      _ -> failS "assertion violation: file map has more than one file!"
  -- Step 2: Initialize the visited set with the source file's path.
  -- This ensures we don't try to re-process the file we just parsed.
  let visited_set = Set.singleton file_path_of_in_program
  -- Step 3: Resolve all include paths in the program to absolute canonical
  -- paths. Each include is relative to the source file's directory.
  in_program_includes <-
    mapM (resolveIncludeToPath file_path_of_in_program) $
      AST.includes in_program
  -- Step 4: Enter the recursive processing loop.
  result <- loop in_program visited_set in_program_includes
  -- Step 5: Perform high-level structural checks on the final result.
  highLevelStructuralChecks result

-- | Resolve a path written in an @include@ statement into a canonical
-- absolute file path.
--
-- Include paths in Fixen are stored as 'Text' without the @.fix@ extension.
-- This function:
--
-- 1. /Extracts/ the raw path text from the 'AST.Include' node
-- 2. /Strips/ any file extension (include paths are stored without extensions)
-- 3. /Joins/ the path with the parent directory of the current file
-- 4. /Appends/ the @.fix@ extension
-- 5. /Canonicalizes/ the resulting path (resolves @.@, @..@, symlinks, etc.)
--
-- The returned pair includes both the resolved 'FilePath' and the original
-- 'AST.Include' node, so the caller retains the source position information.
--
-- /Example./ If the current file is @/home/user/project/main.fix@ and the
-- include path is @lib/utils@, the resolved path will be
-- @/home/user/project/lib/utils.fix@.
resolveIncludeToPath
  :: MonadIO μ
  => FilePath
  -- ^ The file path of the current program (used as the base directory for
  -- resolving relative include paths).
  -> AST.Include
  -- ^ The include statement node, containing the path text and source position.
  -> μ (FilePath, AST.Include)
  -- ^ The resolved canonical file path paired with the original include node.
resolveIncludeToPath file_path incl = do
  -- Extract the raw path text, strip any extension, and convert to 'FilePath'.
  let rel_path = dropExtension $ unpack $ Core.includePath incl
      -- Join with the parent directory of the current file and append .fix.
      complete_path = takeDirectory file_path </> (rel_path -<.> "fix")
  -- Canonicalize the path (resolves ., .., symlinks, normalizes separators).
  (,incl) <$> liftIO (canonicalizePath complete_path)

-- | Recursively process a queue of include files.
--
--   This is the core of the include resolution algorithm. It processes
--   includes one at a time from a queue, handling each of the following
--   cases:
--
--   * /Empty queue/ — when there are no more includes to process, return
--     the accumulated program.
--
--   * /Already visited/ — if the file path is already in the visited set
--     (a cycle or duplicate include), skip it and continue with the rest.
--
--   * /New file/ — read the file, parse it, combine it into the program,
--     resolve its own includes, and add them to the front of the queue.
--
--   /Parameters./
--
--   * @in_prog@ — the accumulated program so far
--   * @v@ — the set of already-visited file paths (cycle detection)
--   * The third argument is a list of @(filePath, includeNode)@ pairs
--     representing the queue of includes to process. New includes from
--     parsed files are prepended to the front of this queue.
--
--   /Error handling./ If a file cannot be read or a parse fails, the
--   error is recorded (via 'safeReadFile') and processing continues
--   with the remaining includes. The function never fails due to a
--   single bad include — all errors are accumulated.
loop :: AST.Program -> Set.Set FilePath -> [(FilePath, AST.Include)] -> FixenPass (PositionEnv :*: NodeId :*: FixenErrors) AST.Program

-- | Base case: no more includes to process. Return the accumulated program.
loop in_prog _ [] = return in_prog
--  Process the next include in the queue.
loop in_prog v ((p, i) : ps)
  -- Already visited: skip this file to avoid cycles and duplicates.
  | p `Set.member` v = loop in_prog v ps
  | otherwise = do
      -- Step 1: Read the file contents safely (with error handling).
      f <- safeReadFile p i
      case f of
        -- Read failed (file not found, permission error, etc.):
        -- mark as visited and continue with remaining includes.
        Nothing -> loop in_prog (p `Set.insert` v) ps
        -- Read succeeded: proceed to parse and combine.
        Just c -> do
          -- Step 2: Record the file contents in the file map so the
          -- parser can correlate errors with source positions.
          fixenInsertFileMap p c
          -- Step 3: Attempt to parse the file contents. If parsing
          -- fails, the error is accumulated and we continue.
          parse_result <- fixenPassTry (parse p (pack c))
          case parse_result of
            -- Parse failed: mark as visited and continue with remaining includes.
            Nothing ->
              loop in_prog (p `Set.insert` v) ps
            -- Parse succeeded: combine the parsed program with the
            -- accumulated program, resolve its includes, and continue.
            Just parsed_prog -> do
              -- Merge the newly parsed program into the accumulated one.
              let new_prog = combineProgram in_prog parsed_prog
              -- Resolve the new program's own includes and add them to
              -- the front of the queue (depth-first processing).
              parsed_program_includes <- mapM (resolveIncludeToPath p) $ AST.includes parsed_prog
              -- Continue processing with the combined program and
              -- the expanded include queue.
              loop new_prog (p `Set.insert` v) (parsed_program_includes ++ ps)

-- | Merge two 'AST.Program' values into a single program.
--
--   This function combines the constructs from @new_prog@ (the included
--   file) into @in_prog@ (the accumulated program). The merge strategy
--   for each construct type is:
--
--   /Concatenated (new before old)/ — rules, relations, partial order
--   declarations, Haskell code blocks, queries and extern symbols. New
--   constructs are placed first so they take precedence during symbol
--   resolution.
--
--   /Deduplicated/ — Haskell imports. Import statements that already exist
--   in @in_prog@ are filtered out from @new_prog@ to avoid duplicates.
--   Deduplication is based on the full module identifier (e.g. @Data.List@).
--
--   /Omitted/ — priorities, and phases from @new_prog@ are not
--   merged. These are not included because they are typically file-scoped
--   declarations that should not be combined across files.
--
--   /Record update./ The function uses Haskell record update syntax to
--   produce a new 'AST.Program' with only the relevant fields replaced.
--   All other fields (module name, includes, etc.) are preserved from
--   @in_prog@.
combineProgram :: AST.Program -> AST.Program -> AST.Program
combineProgram in_prog new_prog =
  -- Extract constructs from the new (included) program.
  let new_rules = AST.rules new_prog
      new_rels = AST.relations new_prog
      new_partial_ords = AST.partialOrdDeclarations new_prog
      -- Extract constructs from the existing (accumulated) program.
      old_import_set =
        -- Build a set of already-imported module identifiers for deduplication.
        Set.fromList
          ( Core.fullIdentifier
              . Core.hsImportImport
              <$> AST.hsImports in_prog
          )
      -- Filter new imports to exclude any that already exist in the old program.
      new_imports =
        filter
          ( \x ->
              Core.fullIdentifier (Core.hsImportImport x)
                `Set.notMember` old_import_set
          )
          (AST.hsImports new_prog)
      new_hs_blocks = AST.hsBlocks new_prog
      new_externs = AST.extern new_prog
      new_queries = AST.queries new_prog
      -- Concatenate new constructs before old ones (new takes precedence).
      total_rules = new_rules ++ AST.rules in_prog
      total_rels = new_rels ++ AST.relations in_prog
      total_partial_ords =
        new_partial_ords
          ++ AST.partialOrdDeclarations in_prog
      total_imports = new_imports ++ AST.hsImports in_prog
      total_hs_blocks = new_hs_blocks ++ AST.hsBlocks in_prog
      total_externs = new_externs ++ AST.extern in_prog
      total_queries = new_queries ++ AST.queries in_prog
   in -- Produce a new program with merged fields, preserving all other fields
      -- (module name, includes, queries, priorities, phases) from in_prog.
      in_prog
        { AST.rules = total_rules
        , AST.relations = total_rels
        , AST.partialOrdDeclarations = total_partial_ords
        , AST.hsImports = total_imports
        , AST.hsBlocks = total_hs_blocks
        , AST.extern = total_externs
        , AST.queries = total_queries
        }

-- | Read a file's contents with graceful error handling.
--
--   Unlike a plain 'readFile'' call, this function catches 'Exception.IOException'
--   and converts it into an accumulated diagnostic error rather than crashing
--   the compiler. This is essential for the include system: if one included
--   file is missing or unreadable, the compiler should continue processing
--   the remaining includes and report all errors together.
--
--   /Return value./ Returns 'Just' the file contents on success, or 'Nothing'
--   on failure (after recording the error). The caller ('loop') uses this to
--   decide whether to continue processing or skip the file.
--
--   /Error details./ When a read fails, the accumulated error includes:
--
--   * The error category @\"IOException\"@
--   * A human-readable message @\"Failed to read file\"@
--   * A source marker pointing to the include statement in the source file
--   * A note with the resolved file path and the exception details
--
--   This gives the user enough information to diagnose missing files,
--   permission errors, or other I/O problems.
safeReadFile
  :: FilePath
  -- ^ The canonical file path to read.
  -> AST.Include
  -- ^ The include node, used to extract the source position for error reporting.
  -> FixenPass (PositionEnv :*: NodeId :*: FixenErrors) (Maybe String)
  -- ^ 'Just' the file contents on success, or 'Nothing' on failure.
safeReadFile file_path include = do
  -- Attempt to read the file, catching any IOException.
  result <- liftIO $ Exception.try (readFile' file_path)
  case result of
    -- Read succeeded: return the contents wrapped in 'Just'.
    Right !content -> return (Just content)
    -- Read failed: accumulate an error and return 'Nothing'.
    -- We do NOT fail fast here — other includes may still be processed
    -- so that all I/O errors can be reported in a single pass.
    Left (e :: Exception.IOException) -> do
      -- Get the source position of the include statement for error reporting.
      pos <- fixenGetPosition include
      -- Accumulate an error with full diagnostic information.
      accumErr
        (Just "IOException")
        "Failed to read file"
        [(pos, This "This include statement")]
        [ Note
            ( "The included path resolves to: "
                ++ file_path
                ++ "\nThe following IOException was thrown:"
                ++ "\n"
                ++ show e
            )
        ]
      -- Return Nothing to signal the caller to skip this file.
      return Nothing

-- | Perform high-level structural checks of the final result. Essentially,
-- every program must have:
-- 1. At least one relation
-- 2. At least one rule
highLevelStructuralChecks :: AST.Program -> FixenPass (PositionEnv :*: NodeId :*: FixenErrors) AST.Program
highLevelStructuralChecks prog = do
  when (Prelude.length (AST.relations prog) == 0) $
    accumErr
      Nothing
      "no rel declarations found"
      []
      [Note "all programs must have at least one rel declaration"]
  when (Prelude.length (AST.rules prog) == 0) $
    accumErr
      Nothing
      "no rule declarations found"
      []
      [Note "all programs must have at least one rule declaration"]
  return prog
