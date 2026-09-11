-- | Target selection is independent of parsing and of backend options.
module Fixen.CodeGen.Target (Target (..), targetFromOutputPath) where

import System.FilePath (takeExtension)

data Target = Haskell | Cpp
  deriving (Eq, Show)

targetFromOutputPath :: FilePath -> Either String Target
targetFromOutputPath path = case takeExtension path of
  ".hs" -> Right Haskell
  ext | ext `elem` [".cpp", ".cc", ".cxx", ".c++"] -> Right Cpp
  ".c" -> Left "The .c extension denotes C, not C++; use .cpp for the C++ backend."
  _ -> Left "Output must have a .hs (Haskell) or .cpp, .cc, .cxx, .c++ (C++) extension."
