module ShortestPath.HandwrittenDijkstra where

import qualified Data.Map.Strict as M
import           Data.Map (Map)
import qualified Data.PQueue.Min as PQ
import           Data.Foldable (foldl')

type Vertex = String

data Dist = Inf | Dist Int
  deriving (Eq, Show)

instance Ord Dist where
  compare Inf     Inf       = EQ
  compare Inf     _         = GT
  compare _       Inf       = LT
  compare (Dist x) (Dist y) = compare x y

add :: Dist -> Dist -> Dist
add Inf _ = Inf
add _ Inf = Inf
add (Dist x) (Dist y) = Dist (x + y)

dijkstra :: Vertex -> Map Vertex [(Vertex, Dist)] -> Map Vertex Dist
dijkstra start edges = go (M.singleton start (Dist 0)) (PQ.singleton (Dist 0, start))
  where
    go :: Map Vertex Dist -> PQ.MinQueue (Dist, Vertex) -> Map Vertex Dist
    go dist pq =
      case PQ.minView pq of
        Nothing -> dist
        Just ((du,u), pq')
          | Just du' <- M.lookup u dist, du /= du' -> go dist pq'
          | otherwise ->
              let nbrs = M.findWithDefault [] u edges
                  (dist', pq'') = foldl' (relax du) (dist, pq') nbrs
              in go dist' pq''

    relax :: Dist -> (Map Vertex Dist, PQ.MinQueue (Dist, Vertex))
          -> (Vertex, Dist)
          -> (Map Vertex Dist, PQ.MinQueue (Dist, Vertex))
    relax du (dist, pq) (v, w) =
      let alt = add du w
      in case M.lookup v dist of
           Nothing           -> (M.insert v alt dist, PQ.insert (alt, v) pq)
           Just dv | alt < dv -> (M.insert v alt dist, PQ.insert (alt, v) pq)
                   | otherwise -> (dist, pq)
