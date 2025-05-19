{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Compiler.Compile where

import Compiler.CodeGen.Generate ( generateProgram )
import Compiler.Compactify ( compactify )
import Compiler.Sort (sortRawProgram)
import Data.Function ((&))
import Data.Text (pack)
import Parsing (parseTopLevel)
import Compiler.ExplicateConstraints (explicateConstraints)
import Compiler.GenerateUniqueNames (generateUniqueNames)
import Language.Haskell.Exts (prettyPrint)
import Prettyprinter (Pretty(pretty))
import Control.Exception (throw)

type SourcePath = FilePath
type DestPath = FilePath
type GeneratedCode = String
type Debug = Bool

compile :: FilePath -> FilePath -> IO ()
compile src = compileWith src . writeFile

compileWith :: FilePath -> (String -> IO ()) -> IO ()
compileWith src out = do
  programStr <- readFile src
  compileStrWith programStr out

compileStr :: String -> FilePath -> IO ()
compileStr programStr = compileStrWith programStr . writeFile

compileStrWith :: String -> (String -> IO ()) -> IO ()
compileStrWith programStr out =
  case parseTopLevel (pack programStr) of
    Right raw -> do
      modul <- raw &
            generateProgram False .
            explicateConstraints .
            compactify .
            generateUniqueNames .
            sortRawProgram
      out $ prettyPrint modul
    Left err -> print err

compileDebug :: SourcePath -> DestPath -> IO ()
compileDebug src dest = do
  programStr <- readFile src
  case parseTopLevel (pack programStr) of
    Right raw -> do
      modul <- raw &
            generateProgram True .
            explicateConstraints .
            compactify .
            generateUniqueNames .
            sortRawProgram
      writeFile dest (prettyPrint modul)
    Left err -> throw err

generateTree :: FilePath -> IO ()
generateTree src = do
  programStr <- readFile src
  case parseTopLevel (pack programStr) of
    Right raw -> do
      let tree = explicateConstraints .
              compactify .
              generateUniqueNames .
              sortRawProgram $ raw
      print . pretty $ tree
    Left err -> print err