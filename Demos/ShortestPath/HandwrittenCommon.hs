module ShortestPath.HandwrittenCommon where

import Control.DeepSeq

type Vertex = String

data Dist = Inf | Dist Int
  deriving (Eq, Show)

instance NFData Dist where
  rnf Inf = ()
  rnf (Dist x) = rnf x

instance Ord Dist where
  compare Inf Inf = EQ
  compare Inf _ = GT
  compare _ Inf = LT
  compare (Dist x) (Dist y) = compare x y

add :: Dist -> Dist -> Dist
add Inf _ = Inf
add _ Inf = Inf
add (Dist x) (Dist y) = Dist (x + y)

instance Num Dist where 
  (+) = add
  Inf * _ = Inf
  _ * Inf = Inf
  abs Inf = Inf
  abs (Dist x) = Dist (abs x)
  signum Inf = 1
  signum (Dist x) = Dist $ signum x
  fromInteger = Dist . fromInteger


