{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
module ShortestPath.ShortestPath where
import ShortestPath.Dist
import Algebra.PartialOrd
import Common.Definitions
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (Foldable(foldl'))
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.PQueue.Max as Q
import GHC.Generics (Generic)
import Numeric.Natural

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes y x = leq x y && not (leq y x)

data Start = Start String
               deriving (Eq, Show, Generic)

instance Hashable Start

instance PartialOrd Start where
        leq (Start v0) (Start v0') = (v0 `leq` v0')
mkStart v0 = StartFact (Start v0)

data DistTo = DistTo String Dist
                deriving (Eq, Show, Generic)

instance Hashable DistTo

instance PartialOrd DistTo where
        leq (DistTo v0 v1) (DistTo v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkDistTo v0 v1 = DistToFact (DistTo v0 v1)

data Edge = Edge String String Dist
              deriving (Eq, Show, Generic)

instance Hashable Edge

instance PartialOrd Edge where
        leq (Edge v0 v1 v2) (Edge v0' v1' v2')
          = (v0 `leq` v0') && (v1 `leq` v1') && (v2 `leq` v2')
mkEdge v0 v1 v2 = EdgeFact (Edge v0 v1 v2)

data Fact = StartFact Start
          | DistToFact DistTo
          | EdgeFact Edge
              deriving (Show, Eq)

instance Ord Fact where
        (<=) (StartFact x) (StartFact y) = leq x y
        (<=) (DistToFact x) (DistToFact y) = leq x y
        (<=) (EdgeFact x) (EdgeFact y) = leq x y
        (<=) _ _ = False

data DataBase = DataBase{factsStart :: S.HashSet Start,
                         factsDistTo :: S.HashSet DistTo, factsEdge :: S.HashSet Edge}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase S.empty S.empty S.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          StartFact v -> first (\ hset -> db{factsStart = hset})
                           (update (factsStart db) v)
          DistToFact v -> first (\ hset -> db{factsDistTo = hset})
                            (update (factsDistTo db) v)
          EdgeFact v -> first (\ hset -> db{factsEdge = hset})
                          (update (factsEdge db) v)

type Queue = Q.MaxQueue Fact

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_1
  = case fact of
        StartFact v_2 -> Q.unions
                           [q_1,
                            foldl'
                              (\ q_3 (Start s0) ->
                                 Q.unions [q_3, Q.singleton (DistToFact (DistTo s0 (DistNat 0)))])
                              Q.empty
                              ([v_2])]
        DistToFact v_4 -> Q.unions
                            [q_1,
                             foldl'
                               (\ q_5 (DistTo v10 d10) ->
                                  Q.unions
                                    [q_5,
                                     foldl'
                                       (\ q_7 (Edge v10_6 v20 d20) ->
                                          Q.unions
                                            [q_7,
                                             Q.singleton (DistToFact (DistTo v20 (add d10 d20)))])
                                       Q.empty
                                       (foldl'
                                          (\ rest (Edge v_8 v_9 v_10) ->
                                             (pure Edge <*> mlbs v_8 v10 <*> pure v_9 <*> pure v_10)
                                               ++ rest)
                                          []
                                          (S.toList (factsEdge db)))])
                               Q.empty
                               ([v_4])]
        EdgeFact v_11 -> Q.unions
                           [q_1,
                            foldl'
                              (\ q_12 (Edge v10 v20 d20) ->
                                 Q.unions
                                   [q_12,
                                    foldl'
                                      (\ q_14 (DistTo v10_13 d10) ->
                                         Q.unions
                                           [q_14,
                                            Q.singleton (DistToFact (DistTo v20 (add d10 d20)))])
                                      Q.empty
                                      (foldl'
                                         (\ rest (DistTo v_15 v_16) ->
                                            (pure DistTo <*> mlbs v_15 v10 <*> pure v_16) ++ rest)
                                         []
                                         (S.toList (factsDistTo db)))])
                              Q.empty
                              ([v_11])]

enumDistTo :: DataBase -> [DistTo]
enumDistTo db = S.toList (factsDistTo db)

closerThan :: Dist -> DataBase -> [DistTo]
closerThan v_20 db
  = foldl'
      (\ rest (DistTo v_21 v_22) ->
         (pure DistTo <*> pure v_21 <*> mlbs v_22 v_20) ++ rest)
      []
      (S.toList (factsDistTo db))

distTo :: String -> DataBase -> [DistTo]
distTo v_23 db
  = foldl'
      (\ rest (DistTo v_25 v_26) ->
         (pure DistTo <*> mlbs v_25 v_23 <*> pure v_26) ++ rest)
      []
      (S.toList (factsDistTo db))

reachableIn :: String -> Dist -> DataBase -> [DistTo]
reachableIn v_27 v_28 db
  = foldl'
      (\ rest (DistTo v_29 v_30) ->
         (pure DistTo <*> mlbs v_29 v_27 <*> mlbs v_30 v_28) ++ rest)
      []
      (S.toList (factsDistTo db))

compute :: [Fact] -> DataBase
compute = go emptyDB . Q.fromList
  where go :: DataBase -> Queue -> DataBase
        go db pq
          | Q.null pq = db
          | otherwise =
            let (nextFact, pq') = Q.deleteFindMax pq
                (db', changed) = insertDB nextFact db
                pq'' = if changed then step db' nextFact pq' else pq'
              in go db' pq''