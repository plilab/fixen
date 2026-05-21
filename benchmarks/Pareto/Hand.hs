module Pareto.Hand where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Numeric.Natural

type Vertex = String
type Dist = Natural
type Cost = Natural

data Cont = Cont Natural Natural Vertex

instance Eq Cont where
  c == c'
    | c < c' = False
    | c' < c = False
    | otherwise = True

instance Ord Cont where
  Cont a b _ < Cont a' b' _ = a < a' || (a == a' && b < b')
  x <= y = not (y < x)

dominates :: (Dist, Cost) -> (Dist, Cost) -> Bool
dominates (d, c) (d', c') = d <= d' && c <= c'

pareto :: Vertex -> HashMap Vertex [(Vertex, Dist, Cost)] -> HashMap Vertex (Set (Dist, Cost))
pareto start edges = go Map.empty (Data.PQueue.Min.fromList [Cont 0 0 start])
  where
    go :: HashMap Vertex (Set (Dist, Cost)) -> MinQueue Cont -> HashMap Vertex (Set (Dist, Cost))
    go dists Empty = dists
    go dists ((Cont d c v) :< work)
      | Just ls <- Map.lookup v dists
      , any (`dominates` (d, c)) ls =
          go dists work
      | otherwise =
          let curr_dists = Map.findWithDefault Set.empty v dists
              new_dists_of_v = Set.insert (d, c) $ Set.filter (not . ((d, c) `dominates`)) curr_dists
              dists' = Map.insert v new_dists_of_v dists
              newWork = fmap (\(v', d', c') -> Cont (d + d') (c + c') v') (Map.findWithDefault [] v edges)
              work' = Data.PQueue.Min.union work (Data.PQueue.Min.fromList newWork)
           in go dists' work'
