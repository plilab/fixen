-- |
--     Module : CommandLine.CommandLineArgs
--     Description: The data structure(s) and type(s) for representing
--                  command-line arguments
--     Copyright: (c) Programming Languages Innovation Lab@NUS
--     License: MIT
--     Maintainer: yongqi@nus.edu.sg
--     Stability: experimental
--
--     This module defines the data structures and type(s) for representing
--     command-line arguments.
module CommandLine.CommandLineArgs (
  CommandLineArgs (..),
  InFilePath,
  OutFilePath,
) where

--------------------------------------------------------------------------------
--
-- Note: [Command-line arguments]
--
-- This is a rather straightforward implementation of command-line-argument
-- parsing using optparse-applicative. Currently, we only need to receive the
-- input .fix file and the output .hs file names from the user.
--
--------------------------------------------------------------------------------

-- | The 'CommandLineArgs' type stores command-line arguments from the user.
data CommandLineArgs = CommandLineArgs
  { outFile :: OutFilePath
  -- ^ The output Haskell source file path
  , inFile :: InFilePath
  -- ^ The input Fixen source file path
  , color :: Bool
  -- ^ Whether the output should contain color.
  , unicode :: Bool
  -- ^ Whether the output should contain unicode characters.
  , program :: Bool
  -- ^ Whether the output should show the program
  , forest :: Bool
  -- ^ Whether the output should show the rule forest
  , db :: Bool
  -- ^ Whether the output should show the database representation
  }
  deriving (Show)

-- These types are used just to keep the type signatures self-explanatory.

-- | The 'FilePath' of the input Fixen source file
type InFilePath = FilePath

-- | The 'FilePath' of the output Haskell source file
type OutFilePath = FilePath
