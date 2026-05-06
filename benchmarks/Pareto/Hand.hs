module Pareto.Hand where

import Data.Map (Map)
import Data.Map qualified (empty, findWithDefault, insert, lookup)
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified

type Vertex = String
type Dist = Int
type Cost = Int

data MultiWeight = MultiWeight {distance :: Int, cost :: Int}
  deriving (Show)

toPair :: MultiWeight -> (Dist, Cost)
toPair MultiWeight {distance = d, cost = c} = (d, c)

fromPair :: (Dist, Cost) -> MultiWeight
fromPair (d, c) = MultiWeight {distance = d, cost = c}

instance Eq MultiWeight where
  a == b = compare a b == EQ

instance Ord MultiWeight where
  compare (MultiWeight x y) (MultiWeight x' y')
    | x < x' && y <= y' = LT
    | x <= x' && y < y' = LT
    | x >= x' && y > y' = GT
    | x > x' && y >= y' = GT
    | otherwise = EQ

pareto :: Vertex -> Map Vertex [(Vertex, Dist, Cost)] -> Map Vertex (Dist, Cost)
pareto start edges = go Data.Map.empty (Data.PQueue.Min.fromList [(fromPair (0, 0), start)])
  where
    go :: Map Vertex (Dist, Cost) -> MinQueue (MultiWeight, Vertex) -> Map Vertex (Dist, Cost)
    go dists Empty = dists
    go dists ((MultiWeight d c, v) :< work)
      | Just (d', c') <- Data.Map.lookup v dists
      , d' <= d && c' <= c =
          go dists work
      | otherwise =
          let dists' = Data.Map.insert v (d, c) dists
              newWork = fmap (\(v', d', c') -> (fromPair (d + d', c + c'), v')) (Data.Map.findWithDefault [] v edges)
              work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList newWork)
          in  go dists' work'
