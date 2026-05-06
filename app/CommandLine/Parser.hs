-- |
--     Module : CommandLine.Parser
--     Description: The parser for command-line arguments
--     Copyright: (c) Programming Languages Innovation Lab@NUS
--     License: MIT
--     Maintainer: yongqi@nus.edu.sg
--     Stability: experimental
--
--     This module parses command line arguiments from the user.
module CommandLine.Parser (
  -- * Parsing Command Line Arguments
  getCommandLineArgs,
) where

import CommandLine.CommandLineArgs
import Control.Monad (when)
import Data.List
import Data.Version (Version (versionBranch))
import Options.Applicative -- See: optparse-applicative package
import Paths_fixen qualified as PF
import System.Exit (ExitCode (..), exitWith)
import System.FilePath
import System.IO (hPutStrLn, stderr)

-- | This function obtains the command line arguments from the user. Currently,
-- the program only accepts two arguments, the output Haskell file (with -o or
-- --output) and the input Fixen file (positional argument). The file
-- extensions are validated via 'validateOutFileExtension' and
-- 'validateInFileExtension'. See Note: [Validating file extensions].
getCommandLineArgs :: IO CommandLineArgs
getCommandLineArgs = do
  -- Run the parser 'opts' using parser preferences 'p'
  cmd_line_args <- customExecParser p opts
  -- Perform validation on the file extensions, terminating early if they are
  -- not what we expect
  validateOutFileExtension (outFile cmd_line_args)
  validateInFileExtension (inFile cmd_line_args)
  -- return successfully
  return cmd_line_args
  where
    -- Preferences for our command-line parser
    p :: ParserPrefs
    p = prefs (showHelpOnEmpty <> showHelpOnError)
    -- Standard boilerplate wrappers around what we want to parse. See
    -- Note: [Constants] on version numbers, program names and descriptions.
    opts :: ParserInfo CommandLineArgs
    opts =
      info
        (cmd <**> simpleVersioner ("fixen " ++ fixenVersion) <**> helper)
        ( fullDesc
            <> progDesc fixenProgramDescription
            <> header fixenProgramHeader
        )
    -- The thing that actually parses the arguments and builds
    -- the 'CommandLineArgs'
    cmd :: Parser CommandLineArgs
    cmd =
      CommandLineArgs
        <$> strOption
          ( long "output" -- --output or -o
              <> short 'o'
              <> metavar "FILENAME"
              <> help "The output Haskell source file"
          )
        <*> argument
          str
          ( metavar "FILENAME"
              <> help "The input Fixen source file"
          )
        <*> fmap
          not
          ( switch
              ( long "no-color"
                  <> short 'c'
                  <> help "Suppress colors in output"
              )
          )
        <*> fmap
          not
          ( switch
              ( long "no-unicode"
                  <> short 'u'
                  <> help "Suppress unicode characters in output"
              )
          )
        <*> ( switch
                ( long "show-ast"
                    <> short 'a'
                    <> help "Show the parsed AST"
                )
            )
        <*> ( switch
                ( long "show-forest"
                    <> short 'f'
                    <> help "Show the rule forest"
                )
            )
        <*> ( switch
                ( long "show-db"
                    <> short 'd'
                    <> help "Show the database representation"
                )
            )

--------------------------------------------------------------------------------
--
-- Note: [Validating file extensions]
--
-- It is probably good for ergonomics to alert the user when the files they
-- have specified are of extensions that we are not expecting. For instance,
-- a user may write
--
-- fixen --output Prog.fix Prog.hs
--
-- by accident, which might give puzzling syntax errors.
--
--------------------------------------------------------------------------------

-- | Validates that the out file path has a file extensions that matches what we
-- are expecting, which are Haskell source files. The program exits with an
-- error message when the file extension is incorrect.
validateOutFileExtension :: OutFilePath -> IO ()
validateOutFileExtension out_file =
  when (takeExtension out_file /= ".hs") $ do
    hPutStrLn stderr $
      "Invalid output source file: "
        ++ out_file
        ++ "\n                            "
        ++ replicate (length out_file) '^'
        ++ "\n  out file must be Haskell source file with .hs extension"
    exitWith (ExitFailure 2)

-- | Validates that the out file path has a file extensions that matches what we
-- are expecting, which are Fixen source files. The program exits with an
-- error message when the file extension is incorrect.
validateInFileExtension :: InFilePath -> IO ()
validateInFileExtension in_file =
  when (takeExtension in_file /= ".fix") $ do
    hPutStrLn stderr $
      "Invalid input source file: "
        ++ in_file
        ++ "\n                           "
        ++ replicate (length in_file) '^'
        ++ "\n  in file must be Fixen source file with .fix extension"
    exitWith (ExitFailure 2)

--------------------------------------------------------------------------------
--
-- Note: [Constants]
--
-- Here we define some basic constants which are shown to the user. Importantly,
-- when we update version numbers of this program, we should also update the
-- version being displayed.
--
--------------------------------------------------------------------------------

-- | The program description that is displayed to the user in the help screen.
fixenProgramDescription :: String
fixenProgramDescription = "Generates Haskell source code from a Fixen program"

-- | The program header that is displayed to the user in the help screen.
fixenProgramHeader :: String
fixenProgramHeader =
  "fixen: A Fixed-Point-Oriented Programming (FPOP)"
    ++ " language for generating work-queue algorithms in Haskell"

-- | The version of this program
fixenVersion :: String
fixenVersion = intercalate "." $ map show $ versionBranch PF.version
