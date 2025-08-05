{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module ReducedProduct.ReducedIntervalAndIsEven where

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (singleton, lookup, empty, insertWith, map, fromList, union)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.List as L (head)

import Numeric.Natural

-- The completion should be here too, but we have not yet properly formalised all of completions yet.