module Fixen.ModuleSystem where

import Control.Exception qualified as Exception
import Control.Monad.State
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Error.Diagnose.Report
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core qualified as Core
import Fixen.Monad
import Fixen.Parser (parse)
import System.Directory (canonicalizePath)
import System.FilePath
import System.IO (readFile')

getIncludes :: AST.Program -> FixenPass FixenErrors AST.Program
getIncludes in_program = do
  -- at this point, we only have one file, whose path can be obtained in the
  -- FixenErrors file map and whose contents are parsed as in_program.
  -- The first thing to define is the starting file path, since we need that
  -- to resolve relative paths
  file_path_of_in_program <- do
    file_map <- gets mozErrorsFileMap
    case file_map of
      [] -> failS "assertion violation: file map is empty!"
      [(file_path, _)] -> liftIO $ canonicalizePath file_path
      _ -> failS "assertion violation: file map has more than one file!"
  -- ignore the current program as we don't need to parse it anymore.
  let visited_set = Set.singleton file_path_of_in_program
  -- next, we get all the include paths of this program
  in_program_includes <-
    mapM (resolveIncludeToPath file_path_of_in_program) $
      AST.includes in_program
  -- run the loop to recursive parse and add all includes.
  loop in_program visited_set in_program_includes

-- | Resolve a path written in an include statement into a canonical file path.
resolveIncludeToPath
  :: MonadIO μ
  => FilePath
  -- ^ The file path of the current program (used as a base for the relative
  -- path in the include statement)
  -> AST.Include
  -- ^ The include statement in the current program
  -> μ (FilePath, AST.Include)
resolveIncludeToPath file_path incl = do
  let rel_path = dropExtension $ unpack $ Core.includePath incl
      complete_path = takeDirectory file_path </> (rel_path -<.> "fix")
  (,incl) <$> liftIO (canonicalizePath complete_path)

-- | The main loop to recursively get all includes.
loop :: AST.Program -> Set.Set FilePath -> [(FilePath, AST.Include)] -> FixenPass FixenErrors AST.Program
loop in_prog _ [] = return in_prog
loop in_prog v ((p, i) : ps)
  | p `Set.member` v = loop in_prog v ps
  | otherwise = do
      -- read the file
      f <- safeReadFile p i
      case f of
        Nothing -> loop in_prog (p `Set.insert` v) ps
        Just c -> do
          -- reading the file was a success. proceed to parse
          -- insert file map
          fixenInsertFileMap p c
          liftIO $ putStrLn ("path: " ++ p)
          st <- get
          let fm = mozErrorsFileMap st
          liftIO $ print fm
          parse_result <- fixenPassTry (parse p (pack c))
          case parse_result of
            Nothing ->
              -- parse failed, proceed with other includes
              loop in_prog (p `Set.insert` v) ps
            Just parsed_prog -> do
              -- parse succeeded, combine resulting program with original program
              let new_prog = combineProgram in_prog parsed_prog
              parsed_program_includes <- mapM (resolveIncludeToPath p) $ AST.includes parsed_prog
              loop new_prog (p `Set.insert` v) (parsed_program_includes ++ ps)

-- | Take a new program and combine it with the current one. Only the folllowing
-- are added from the new program into the current one:
--   - Rules
--   - Relations
--   - Partial order declarations
--   - Haskell imports (unique ones only)
--   - Externs (unique ones only)
--   - Haskell code blocks
--
-- Other stuff like queries, priorities, phases are omitted
combineProgram :: AST.Program -> AST.Program -> AST.Program
combineProgram in_prog new_prog =
  let new_rules = AST.rules new_prog
      new_rels = AST.relations new_prog
      new_partial_ords = AST.partialOrdDeclarations new_prog
      -- The new imports need to be filtered out so we don't have duplicate
      -- import statements
      old_import_set =
        Set.fromList
          ( Core.fullIdentifierAsString
              . Core.hsImportImport
              <$> new_imports
          )
      new_imports =
        filter
          ( \x ->
              Core.fullIdentifierAsString (Core.hsImportImport x)
                `Set.notMember` old_import_set
          )
          (AST.hsImports new_prog)
      new_hs_blocks = AST.hsBlocks new_prog
      new_externs = AST.extern new_prog
      total_rules = new_rules ++ AST.rules in_prog
      total_rels = new_rels ++ AST.relations in_prog
      total_partial_ords =
        new_partial_ords
          ++ AST.partialOrdDeclarations in_prog
      total_imports = new_imports ++ AST.hsImports in_prog
      total_hs_blocks = new_hs_blocks ++ AST.hsBlocks in_prog
      -- Similar to the imports, we need to filter out the duplicate extern
      -- symbols
      total_externs = case AST.extern in_prog of
        Nothing -> new_externs
        Just e -> case new_externs of
          Nothing -> Just e
          Just e' ->
            let old_set =
                  Set.fromList
                    ( NonEmpty.toList
                        (Core.fullIdentifierAsString <$> Core.externSymbols e)
                    )
                externs_to_add =
                  filter
                    ( \x ->
                        Core.fullIdentifierAsString x `Set.notMember` old_set
                    )
                    (NonEmpty.toList (Core.externSymbols e'))
                new_symbols =
                  NonEmpty.prependList
                    externs_to_add
                    (Core.externSymbols e)
            in  Just e {Core.externSymbols = new_symbols}
  in  in_prog
        { AST.rules = total_rules
        , AST.relations = total_rels
        , AST.partialOrdDeclarations = total_partial_ords
        , AST.hsImports = total_imports
        , AST.hsBlocks = total_hs_blocks
        , AST.extern = total_externs
        }

-- | Reads a file without crashing the entire thing if an exception occurs
safeReadFile :: FilePath -> AST.Include -> FixenPass FixenErrors (Maybe String)
safeReadFile file_path include = do
  result <- liftIO $ Exception.try (readFile' file_path)
  case result of
    Right !content -> return (Just content)
    Left (e :: Exception.IOException) -> do
      -- do not fail fast here, as we can read the other includes in parallel
      -- before failing overall.
      accumR $
        Err
          (Just "IOException")
          "Failed to read file"
          [(AST.getPosition include, This "This include statement")]
          [ Note
              ( "The included path resolves to: "
                  ++ file_path
                  ++ "\nThe following IOException was thrown:"
                  ++ "\n"
                  ++ show e
              )
          ]
      return Nothing
