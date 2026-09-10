{-# LANGUAGE OverloadedStrings #-}

-- | Names allocated while matching a rule or traversing a query index.
-- Temporary index names have their own counter; they are not entries in the
-- map of logical variables. Copying a Bindings value starts an independent
-- branch with the same lexical environment.
module Fixen.CodeGen.Haskell.Bindings where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Haskell.Syntax qualified as Hs

data Bindings = Bindings
  { variableVersions :: IntMap Int
  , nextTemporary :: Int
  }
  deriving (Eq, Show)

emptyBindings :: Bindings
emptyBindings = Bindings IntMap.empty 0

inputBindings :: [Int] -> Bindings
inputBindings ids = Bindings (IntMap.fromList [(i, 0) | i <- ids]) 0

numberedName :: Text -> Int -> Hs.Name
numberedName prefix i = Hs.Name (prefix <> Text.pack (show i))

variableName :: Int -> Int -> Hs.Name
variableName i version = Hs.Name ("_v" <> Text.pack (show i) <> "_" <> Text.pack (show version))

lookupVariable :: Int -> Bindings -> Maybe Hs.Name
lookupVariable i bindings = variableName i <$> IntMap.lookup i (variableVersions bindings)

boundVariable :: Int -> Bindings -> Hs.Name
boundVariable i bindings = case lookupVariable i bindings of
  Just n -> n
  Nothing -> error ("Fixen.CodeGen: unbound logical variable " ++ show i)

bindVariable :: Int -> Bindings -> (Hs.Name, Bindings)
bindVariable i bindings =
  let version = maybe 0 (+ 1) (IntMap.lookup i (variableVersions bindings))
   in (variableName i version, bindings {variableVersions = IntMap.insert i version (variableVersions bindings)})

freshTemporary :: Bindings -> (Hs.Name, Bindings)
freshTemporary bindings =
  (numberedName "step" (nextTemporary bindings), bindings {nextTemporary = nextTemporary bindings + 1})
