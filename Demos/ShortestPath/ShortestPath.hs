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
import qualified Data.HashMap.Strict as M
import qualified Data.PQueue.Max as Q
import GHC.Generics (Generic)
import Numeric.Natural
import Control.DeepSeq

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes y x = leq x y && not (leq y x)

data Start = Start String
               deriving (Eq, Show, Generic, Read)

instance Hashable Start

instance PartialOrd Start where
        leq (Start v0) (Start v0') = (v0 `leq` v0')
mkStart v0 = StartFact (Start v0)

data DistTo = DistTo String Dist
                deriving (Eq, Show, Generic, Read)

instance Hashable DistTo

instance PartialOrd DistTo where
        leq (DistTo v0 v1) (DistTo v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkDistTo v0 v1 = DistToFact (DistTo v0 v1)

data Edge = Edge String String Dist
              deriving (Eq, Show, Generic, Read)

instance Hashable Edge

instance PartialOrd Edge where
        leq (Edge v0 v1 v2) (Edge v0' v1' v2')
          = (v0 `leq` v0') && (v1 `leq` v1') && (v2 `leq` v2')
mkEdge v0 v1 v2 = EdgeFact (Edge v0 v1 v2)

data Fact = StartFact Start
          | DistToFact DistTo
          | EdgeFact Edge
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | AddDistCont String Dist String Dist
                  | InitCont String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AddDistCont v10 d10 v20 d20)
  = [DistToFact (DistTo v20 (add d10 d20))]
evaluate db (InitCont s0) = [DistToFact (DistTo s0 (DistNat 0))]

instance Ord Continuation where
        (<=) (AddDistCont _ d11 _ d21) (AddDistCont _ d12 _ d22)
          = leq (add d11 d21) (add d12 d22)
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsStart :: S.HashSet String,
                         factsEdge :: M.HashMap (String, String) (S.HashSet Dist),
                         factsDistTo :: M.HashMap String (S.HashSet Dist)}
                  deriving (Show, Eq)

instance NFData DataBase where 
  rnf (DataBase a b c) = rnf (a, b, c)

emptyDB :: DataBase
emptyDB = DataBase S.empty M.empty M.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          StartFact (Start v0) -> if S.member v0 (factsStart db) then
                                    (db, False) else
                                    (db{factsStart = S.insert v0 (factsStart db)}, True)
          EdgeFact (Edge v0 v1 v2) -> if M.member (v0, v1) (factsEdge db)
                                        then
                                        first
                                          (\ hset ->
                                             db{factsEdge = M.insert (v0, v1) hset (factsEdge db)})
                                          (update ((M.!) (factsEdge db) (v0, v1)) v2)
                                        else
                                        (db{factsEdge =
                                              M.insert (v0, v1) (S.singleton v2) (factsEdge db)},
                                         True)
          DistToFact (DistTo v0 v1) -> if M.member v0 (factsDistTo db) then
                                         first
                                           (\ hset ->
                                              db{factsDistTo = M.insert v0 hset (factsDistTo db)})
                                           (update ((M.!) (factsDistTo db) v0) v1)
                                         else
                                         (db{factsDistTo =
                                               M.insert v0 (S.singleton v1) (factsDistTo db)},
                                          True)

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_1
  = case fact of
        StartFact (Start v_2) -> Q.unions
                                   [q_1,
                                    foldl'
                                      (\ q_3 (Start s0) ->
                                         Q.unions [q_3, Q.singleton (InitCont s0)])
                                      Q.empty
                                      (S.foldl' (\ rest v_4 -> Start v_4 : rest) []
                                         (S.singleton v_2))]
        DistToFact (DistTo v_5 v_6) -> Q.unions
                                         [q_1,
                                          foldl'
                                            (\ q_7 (DistTo v10 d10) ->
                                               Q.unions
                                                 [q_7,
                                                  foldl'
                                                    (\ q_9 (Edge v10_8 v20 d20) ->
                                                       Q.unions
                                                         [q_9,
                                                          Q.singleton
                                                            (AddDistCont v10_8 d10 v20 d20)])
                                                    Q.empty
                                                    (M.foldlWithKey'
                                                       (\ rest (v_10, v_11) vals ->
                                                          concatMap
                                                            (\ v_12 ->
                                                               pure Edge <*> mlbs v_10 v10 <*>
                                                                 pure v_11
                                                                 <*> pure v_12)
                                                            vals
                                                            ++ rest)
                                                       []
                                                       (factsEdge db))])
                                            Q.empty
                                            (M.foldlWithKey'
                                               (\ rest v_13 vals ->
                                                  S.foldl' (\ acc v_14 -> DistTo v_13 v_14 : acc) []
                                                    vals
                                                    ++ rest)
                                               []
                                               (M.singleton v_5 (S.singleton v_6)))]
        EdgeFact (Edge v_15 v_16 v_17) -> Q.unions
                                            [q_1,
                                             foldl'
                                               (\ q_18 (Edge v10 v20 d20) ->
                                                  Q.unions
                                                    [q_18,
                                                     foldl'
                                                       (\ q_20 (DistTo v10_19 d10) ->
                                                          Q.unions
                                                            [q_20,
                                                             Q.singleton
                                                               (AddDistCont v10_19 d10 v20 d20)])
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_21 vals ->
                                                             concatMap
                                                               (\ v_22 ->
                                                                  pure DistTo <*> mlbs v_21 v10 <*>
                                                                    pure v_22)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsDistTo db))])
                                               Q.empty
                                               (M.foldlWithKey'
                                                  (\ rest (v_23, v_24) vals ->
                                                     S.foldl'
                                                       (\ acc v_25 -> Edge v_23 v_24 v_25 : acc)
                                                       []
                                                       vals
                                                       ++ rest)
                                                  []
                                                  (M.singleton (v_15, v_16) (S.singleton v_17)))]

enumDistTo :: DataBase -> [DistTo]
enumDistTo db
  = M.foldlWithKey'
      (\ rest v_28 vals ->
         S.foldl' (\ acc v_29 -> DistTo v_28 v_29 : acc) [] vals ++ rest)
      []
      (factsDistTo db)

closerThan :: Dist -> DataBase -> [DistTo]
closerThan v_31 db
  = M.foldlWithKey'
      (\ rest v_32 vals ->
         concatMap (\ v_33 -> pure DistTo <*> pure v_32 <*> mlbs v_33 v_31)
           vals
           ++ rest)
      []
      (factsDistTo db)

distTo :: String -> DataBase -> [DistTo]
distTo v_34 db
  = M.foldlWithKey'
      (\ rest v_36 vals ->
         concatMap (\ v_37 -> pure DistTo <*> mlbs v_36 v_34 <*> pure v_37)
           vals
           ++ rest)
      []
      (factsDistTo db)

reachableIn :: String -> Dist -> DataBase -> [DistTo]
reachableIn v_38 v_39 db
  = M.foldlWithKey'
      (\ rest v_40 vals ->
         concatMap
           (\ v_41 -> pure DistTo <*> mlbs v_40 v_38 <*> mlbs v_41 v_39)
           vals
           ++ rest)
      []
      (factsDistTo db)

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
