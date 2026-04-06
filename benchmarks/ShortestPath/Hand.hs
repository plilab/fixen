module ShortestPath.Hand where

import Data.Map (Map)
import Data.Map qualified (empty, findWithDefault, insert, lookup)
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified

type Vertex = String
type Dist = Int

dijkstra :: Vertex -> Map Vertex [(Vertex, Dist)] -> Map Vertex Dist
dijkstra start edges = go Data.Map.empty (Data.PQueue.Min.fromList [(0, start)])
  where
    go :: Map Vertex Dist -> MinQueue (Dist, Vertex) -> Map Vertex Dist
    go dists Empty = dists
    go dists ((d, v) :< work)
      | Just d' <- Data.Map.lookup v dists
      , d' <= d =
          go dists work
      | otherwise =
          let dists' = Data.Map.insert v d dists
              newWork = fmap (\(v', d') -> (d + d', v')) (Data.Map.findWithDefault [] v edges)
              work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList newWork)
          in  go dists' work'
