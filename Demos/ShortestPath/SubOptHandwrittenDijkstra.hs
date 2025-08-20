module ShortestPath.SubOptHandwrittenDijkstra where


import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import ShortestPath.HandwrittenCommon

dijkstra :: Vertex -> Map Vertex [(Vertex, Dist)] -> Map Vertex Dist
dijkstra start edges = go (M.singleton start (Dist 0)) (PQ.singleton (Dist 0, start))
  where
    go :: Map Vertex Dist -> PQ.MinQueue (Dist, Vertex) -> Map Vertex Dist
    go dists pq = 
      case PQ.minView pq of
        Nothing -> dists
        Just ((d1, v1), pq')
          | Just d' <- M.lookup v1 dists, d' <= d1 -> go dists pq'
          | otherwise ->
            let dists' = M.insert v1 d1 dists
                newWork = map (\(v2,d2) -> (add d1 d2, v2)) (M.findWithDefault [] v1 edges)
                pq'' = PQ.union pq' (PQ.fromList newWork)
            in go dists' pq''