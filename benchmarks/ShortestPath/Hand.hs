module ShortestPath.Hand where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as MinQueue
import Numeric.Natural

type Vertex = String
type Dist = Natural

dijkstra :: Vertex -> HashMap Vertex [(Vertex, Dist)] -> HashMap Vertex Dist
dijkstra start edges = go HashMap.empty (MinQueue.fromList [(0, start)])
  where
    go :: HashMap Vertex Dist -> MinQueue (Dist, Vertex) -> HashMap Vertex Dist
    go dists Empty = dists
    go dists ((d, v) :< work)
      | Just d' <- HashMap.lookup v dists
      , d' <= d =
          go dists work
      | otherwise =
          let dists' = HashMap.insert v d dists
              newWork = fmap (\(v', d') -> (d + d', v')) (HashMap.findWithDefault [] v edges)
              work' = MinQueue.union (MinQueue.fromList newWork) work
           in go dists' work'

data Cont = Cont Dist Vertex

instance Eq Cont where
  c == c'
    | c < c' = False
    | c' < c = False
    | otherwise = True

instance Ord Cont where
  x <= y = not (y < x)
  Cont d _ < Cont d' _ = d < d'

dijkstraQueueOpt :: Vertex -> HashMap Vertex [(Vertex, Dist)] -> HashMap Vertex Dist
dijkstraQueueOpt start edges = go HashMap.empty (MinQueue.fromList [Cont 0 start])
  where
    go :: HashMap Vertex Dist -> MinQueue Cont -> HashMap Vertex Dist
    go dists Empty = dists
    go dists (Cont d v :< work)
      | Just d' <- HashMap.lookup v dists
      , d' <= d =
          go dists work
      | otherwise =
          let dists' = HashMap.insert v d dists
              newWork = fmap (\(v', d') -> Cont (d + d') v') (HashMap.findWithDefault [] v edges)
              work' = MinQueue.union (MinQueue.fromList newWork) work
           in go dists' work'
