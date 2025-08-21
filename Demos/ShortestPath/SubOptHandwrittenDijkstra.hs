module ShortestPath.SubOptHandwrittenDijkstra where


import Data.Map (Map)
import qualified Data.Map (empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.PQueue.Min (MinQueue, minView)
import qualified Data.PQueue.Min
import Prelude hiding (lookup)

import ShortestPath.HandwrittenCommon

dijkstra :: Vertex -> Map Vertex [(Vertex, Dist)] -> (Map Vertex Dist, Int)
dijkstra start edges = go 0 Data.Map.empty (Data.PQueue.Min.fromList [(0, start)])
  where
    go :: Int -> Map Vertex Dist -> MinQueue (Dist, Vertex) -> (Map Vertex Dist, Int)
    go i dists pq = case minView pq of 
      Nothing -> (dists, i)
      Just ((d_1, v_1), work) -> case Data.Map.lookup v_1 dists of
        Just d' -> if d' <= d_1 
                   then go i dists work 
                   else let dists' = Data.Map.insert v_1 d_1 dists
                            addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                            work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
                         in go (i + length addedWork) dists' work'
        Nothing -> let dists' = Data.Map.insert v_1 d_1 dists
                       addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                       work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
                    in go (i + length addedWork) dists' work'
      -- | Nothing <- minView pq = dists
      -- | Just ((d_1, v_1), work) <- minView pq
      --   = let aux
      --           | Just d' <- Data.Map.lookup v_1 dists,
      --             d' <= d_1 =
      --               go dists work
      --           | otherwise =
      --               let dists' = Data.Map.insert v_1 d_1 dists
      --                   addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
      --                   work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
      --                in go dists' work'
      --      in aux
    -- go dists Empty = dists
    -- go dists ((d_1, v_1) :< work)
    --   | Just d' <- Data.Map.lookup v_1 dists,
    --     d' <= d_1 =
    --       go dists work
    --   | otherwise =
    --       let dists' = Data.Map.insert v_1 d_1 dists
    --           addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
    --           work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
    --        in go dists' work'
