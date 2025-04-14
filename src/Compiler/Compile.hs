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
            generateProgram . 
            explicateConstraints .
            compactify .
            generateUniqueNames .
            sortRawProgram
      out $ prettyPrint modul
    Left err -> print err
