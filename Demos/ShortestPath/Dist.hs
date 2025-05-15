{-# LANGUAGE DeriveGeneric #-}
module ShortestPath.Dist where

import Algebra.PartialOrd
import Common.Definitions
import Data.Hashable
import GHC.Generics (Generic)
import Numeric.Natural

data Dist = Inf 
          | DistNat Natural
  deriving (Show, Eq, Ord, Generic)

instance Hashable Dist

instance PartialOrd Dist where
  leq (DistNat m) (DistNat n) = n <= m
  leq Inf         _           = True
  leq _           _           = False

instance MLB Dist where
  mlbs Inf _ = [Inf]
  mlbs _ Inf = [Inf]
  mlbs (DistNat n) (DistNat m) = [DistNat $ max n m]

add :: Dist -> Dist -> Dist
add (DistNat m) (DistNat n) = DistNat $ m + n
add Inf _ = Inf
add _ Inf = Inf

{-
test = compute [mkEdge "b" "c" (DistNat 1), mkEdge "s" "b" (DistNat 5), mkEdge "b" "a" (DistNat 1), mkEdge "s" "a" (DistNat 3), mkEdge "a" "b" (DistNat 1), mkStart "s"]
-}