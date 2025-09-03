module ShortestPath.SubOptHandwrittenDijkstra where

import Data.Map (Map)
import qualified Data.Map (empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.PQueue.Max (MaxQueue, maxView)
import qualified Data.PQueue.Max
import Data.PQueue.Min (MinQueue, minView)
import qualified Data.PQueue.Min
import qualified Data.Sequence as Q
import Prelude hiding (lookup)

import ShortestPath.HandwrittenCommon

dijkstra :: Vertex -> Map Vertex [(Vertex, Dist)] -> (Map Vertex Dist, Int)
dijkstra start edges = go 0 Data.Map.empty (Data.PQueue.Min.fromList [(0, start)])
  where
    go :: Int -> Map Vertex Dist -> MinQueue (Dist, Vertex) -> (Map Vertex Dist, Int)
    go i dists pq = case minView pq of
        Nothing -> (dists, i)
        Just ((d_1, v_1), work) -> case Data.Map.lookup v_1 dists of
            Just d' ->
                if d' <= d_1
                    then go i dists work
                    else
                        let dists' = Data.Map.insert v_1 d_1 dists
                            addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                            work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
                         in go (i + length addedWork) dists' work'
            Nothing ->
                let dists' = Data.Map.insert v_1 d_1 dists
                    addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                    work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList addedWork)
                 in go (i + length addedWork) dists' work'

dijkstraNoPQ :: Vertex -> Map Vertex [(Vertex, Dist)] -> (Map Vertex Dist, Int)
dijkstraNoPQ start edges = go 0 Data.Map.empty (Q.singleton (0, start))
  where
    go :: Int -> Map Vertex Dist -> Q.Seq (Dist, Vertex) -> (Map Vertex Dist, Int)
    go i dists Q.Empty = (dists, i)
    go i dists ((d_1, v_1) Q.:<| work)
        | Just d' <- Data.Map.lookup v_1 dists
        , d' <= d_1 =
            go i dists work
        | otherwise =
            let dists' = Data.Map.insert v_1 d_1 dists
                addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                work' = work Q.>< Q.fromList addedWork
             in go (i + length addedWork) dists' work'

dijkstraNT :: Vertex -> Map Vertex [(Vertex, Dist)] -> (Map Vertex Dist, Int)
dijkstraNT start edges = go 0 Data.Map.empty (Data.PQueue.Max.fromList [(0, start)])
  where
    go :: Int -> Map Vertex Dist -> MaxQueue (Dist, Vertex) -> (Map Vertex Dist, Int)
    go i dists pq = case maxView pq of
        Nothing -> (dists, i)
        Just ((d_1, v_1), work) -> case Data.Map.lookup v_1 dists of
            Just d' ->
                if d' <= d_1
                    then go i dists work
                    else
                        let dists' = Data.Map.insert v_1 d_1 dists
                            addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                            work' = Data.PQueue.Max.union work (Data.PQueue.Max.fromList addedWork)
                         in go (i + length addedWork) dists' work'
            Nothing ->
                let dists' = Data.Map.insert v_1 d_1 dists
                    addedWork = fmap (\(v_2, d_2) -> (d_1 + d_2, v_2)) (fromMaybe [] (Data.Map.lookup v_1 edges))
                    work' = Data.PQueue.Max.union work (Data.PQueue.Max.fromList addedWork)
                 in go (i + length addedWork) dists' work'
