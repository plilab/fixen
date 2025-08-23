module Main where

import CommandLine.CommandLineArgs (
  CommandLineArgs (..),
  InFilePath,
 )
import CommandLine.Parser (
  getCommandLineArgs,
 )
import Control.Exception
import Error.Diagnose (
  TabSize (TabSize),
  WithUnicode (WithUnicode, WithoutUnicode),
  defaultStyle,
  printDiagnostic,
  stderr,
  unadornedStyle,
 )
import Mozzarella.Monad
import Mozzarella.Parser
import System.Exit (
  ExitCode (..),
  exitWith,
 )
import System.IO qualified as SIO

-------------------------------------------------------------------------------
--
-- The main driver.
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- parse command line arguments
  CommandLineArgs
    { outFile = _
    , inFile = in_file
    , color = color
    , unicode = unicode
    } <-
    getCommandLineArgs
  -- read the input file. whenever there are exceptions, terminate with the
  -- error messages.
  file_handle <-
    handle (openFileExceptionHandler in_file) $
      SIO.openFile in_file SIO.ReadMode
  in_file_contents <-
    handle (readFileExceptionHandler in_file) $
      SIO.hGetContents' file_handle
  -- for now, just print the file contents so we can see what is going on.
  putStrLn in_file_contents
  ast <- runMozzarellaM $ parse in_file in_file_contents
  -- output styles
  let out_style = if color then defaultStyle else unadornedStyle
  let out_unicode = if unicode then WithUnicode else WithoutUnicode
  case ast of
    Left d -> printDiagnostic stderr out_unicode (TabSize 4) out_style d
    Right a -> print a

-------------------------------------------------------------------------------
--
-- Early-termination IOException handlers.
--
-------------------------------------------------------------------------------

openFileExceptionHandler :: InFilePath -> IOException -> IO a
openFileExceptionHandler file_name e = do
  SIO.hPutStrLn SIO.stderr $ "Cannot open " ++ file_name
  SIO.hPutStr SIO.stderr "    "
  SIO.hPrint SIO.stderr e
  exitWith (ExitFailure 1)

readFileExceptionHandler :: InFilePath -> IOException -> IO a
readFileExceptionHandler file_name e = do
  SIO.hPutStrLn SIO.stderr $ "Cannot read " ++ file_name ++ ":"
  SIO.hPutStr SIO.stderr "    "
  SIO.hPrint SIO.stderr e
  exitWith (ExitFailure 1)
