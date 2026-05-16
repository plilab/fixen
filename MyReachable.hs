{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
module MyReachable where
import Algebra.PartialOrd
import Common.Definitions
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (Foldable(foldl'))
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.PQueue.Max as Q
import GHC.Generics (Generic)
import Numeric.Natural

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes y x = leq x y && not (leq y x)

data Reachable = Reachable Vertex Vertex
                   deriving (Eq, Show, Generic, Read)

instance Hashable Reachable

instance PartialOrd Reachable where
        leq (Reachable v0 v1) (Reachable v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkReachable v0 v1 = ReachableFact (Reachable v0 v1)

data Edge = Edge Vertex Vertex
              deriving (Eq, Show, Generic, Read)

instance Hashable Edge

instance PartialOrd Edge where
        leq (Edge v0 v1) (Edge v0' v1') = (v0 `leq` v0') && (v1 `leq` v1')
mkEdge v0 v1 = EdgeFact (Edge v0 v1)

data Fact = ReachableFact Reachable
          | EdgeFact Edge
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | Anonymous0Cont Vertex Vertex Vertex
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (Anonymous0Cont a0 b0 c0)
  = [ReachableFact (Reachable a0 c0)]

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsEdge :: S.HashSet (Vertex, Vertex),
                         factsReachable :: S.HashSet (Vertex, Vertex)}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase S.empty S.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          EdgeFact (Edge v0 v1) -> first (\ hset -> db{factsEdge = hset})
                                     (update (factsEdge db) (v0, v1))
          ReachableFact (Reachable v0 v1) -> first
                                               (\ hset -> db{factsReachable = hset})
                                               (update (factsReachable db) (v0, v1))

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_1
  = case fact of
        ReachableFact (Reachable v_2 v_3) -> Q.unions
                                               [q_1,
                                                foldl'
                                                  (\ q_4 (Reachable b0 c0) ->
                                                     Q.unions
                                                       [q_4,
                                                        foldl'
                                                          (\ q_6 (Edge a0 b0_5) ->
                                                             Q.unions
                                                               [q_6,
                                                                Q.singleton
                                                                  (Anonymous0Cont a0 b0_5 c0)])
                                                          Q.empty
                                                          (foldl'
                                                             (\ rest (v_7, v_8) ->
                                                                (pure Edge <*> pure v_7 <*>
                                                                   mlbs v_8 b0)
                                                                  ++ rest)
                                                             []
                                                             (factsEdge db))])
                                                  Q.empty
                                                  (S.foldl'
                                                     (\ rest (v_9, v_10) ->
                                                        Reachable v_9 v_10 : rest)
                                                     []
                                                     (S.singleton (v_2, v_3)))]
        EdgeFact (Edge v_11 v_12) -> Q.unions
                                       [q_1,
                                        foldl'
                                          (\ q_13 (Edge a0 b0) ->
                                             Q.unions
                                               [q_13,
                                                foldl'
                                                  (\ q_15 (Reachable b0_14 c0) ->
                                                     Q.unions
                                                       [q_15,
                                                        Q.singleton (Anonymous0Cont a0 b0_14 c0)])
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (v_16, v_17) ->
                                                        (pure Reachable <*> mlbs v_16 b0 <*>
                                                           pure v_17)
                                                          ++ rest)
                                                     []
                                                     (factsReachable db))])
                                          Q.empty
                                          (S.foldl' (\ rest (v_18, v_19) -> Edge v_18 v_19 : rest)
                                             []
                                             (S.singleton (v_11, v_12)))]

compute :: [Fact] -> DataBase
compute = go emptyDB . Q.fromList . map Initial
  where go :: DataBase -> Queue -> DataBase
        go db pq
          | Q.null pq = db
          | otherwise =
            let (nextFacts, pq') = first (evaluate db) $ Q.deleteFindMax pq
                (db', pq'')
                  = foldl'
                      (\ (dbOld, pqOld) f ->
                         let (dbNew, changed) = insertDB f dbOld
                             pqNew = if changed then step dbNew f pqOld else pqOld
                           in (dbNew, pqNew))
                      (db, pq')
                      nextFacts
              in go db' pq''