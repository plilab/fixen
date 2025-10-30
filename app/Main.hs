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
import Fixen.Monad
import Fixen.Pipeline
import System.Directory (canonicalizePath)
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

-- | Go!
main :: IO ()
main = do
  -- parse command line arguments
  CommandLineArgs
    { outFile = out_file_
    , inFile = in_file_
    , color = color
    , unicode = unicode
    } <-
    getCommandLineArgs
  -- read the input file. whenever there are exceptions, terminate with the
  -- error messages.
  in_file <- canonicalizePath in_file_
  _ <- canonicalizePath out_file_
  file_handle <-
    handle (openFileExceptionHandler in_file) $
      SIO.openFile in_file SIO.ReadMode
  in_file_contents <-
    handle (readFileExceptionHandler in_file) $
      SIO.hGetContents' file_handle
  ast <- runFixenM $ pipeline in_file in_file_contents
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

-- | Handler for file opening exceptions
openFileExceptionHandler :: InFilePath -> IOException -> IO a
openFileExceptionHandler file_name e = do
  SIO.hPutStrLn SIO.stderr $ "Cannot open " ++ file_name
  SIO.hPutStr SIO.stderr "    "
  SIO.hPrint SIO.stderr e
  exitWith (ExitFailure 1)

-- | Handler for file reading exceptions
readFileExceptionHandler :: InFilePath -> IOException -> IO a
readFileExceptionHandler file_name e = do
  SIO.hPutStrLn SIO.stderr $ "Cannot read " ++ file_name ++ ":"
  SIO.hPutStr SIO.stderr "    "
  SIO.hPrint SIO.stderr e
  exitWith (ExitFailure 1)
