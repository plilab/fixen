{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
module ShortestPath.ShortestPathNoPriority where
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
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsStart :: S.HashSet String,
                         factsEdge :: M.HashMap (String, String) (S.HashSet Dist),
                         factsDistTo :: M.HashMap String (S.HashSet Dist)}
                  deriving (Show, Eq)

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
step db fact q_42
  = case fact of
        StartFact (Start v_43) -> Q.unions
                                    [q_42,
                                     foldl'
                                       (\ q_44 (Start s0) ->
                                          Q.unions [q_44, Q.singleton (InitCont s0)])
                                       Q.empty
                                       (S.foldl' (\ rest v_45 -> Start v_45 : rest) []
                                          (S.singleton v_43))]
        DistToFact (DistTo v_46 v_47) -> Q.unions
                                           [q_42,
                                            foldl'
                                              (\ q_48 (DistTo v10 d10) ->
                                                 Q.unions
                                                   [q_48,
                                                    foldl'
                                                      (\ q_50 (Edge v10_49 v20 d20) ->
                                                         Q.unions
                                                           [q_50,
                                                            Q.singleton
                                                              (AddDistCont v10_49 d10 v20 d20)])
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest (v_51, v_52) vals ->
                                                            concatMap
                                                              (\ v_53 ->
                                                                 pure Edge <*> mlbs v_51 v10 <*>
                                                                   pure v_52
                                                                   <*> pure v_53)
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (factsEdge db))])
                                              Q.empty
                                              (M.foldlWithKey'
                                                 (\ rest v_54 vals ->
                                                    S.foldl' (\ acc v_55 -> DistTo v_54 v_55 : acc)
                                                      []
                                                      vals
                                                      ++ rest)
                                                 []
                                                 (M.singleton v_46 (S.singleton v_47)))]
        EdgeFact (Edge v_56 v_57 v_58) -> Q.unions
                                            [q_42,
                                             foldl'
                                               (\ q_59 (Edge v10 v20 d20) ->
                                                  Q.unions
                                                    [q_59,
                                                     foldl'
                                                       (\ q_61 (DistTo v10_60 d10) ->
                                                          Q.unions
                                                            [q_61,
                                                             Q.singleton
                                                               (AddDistCont v10_60 d10 v20 d20)])
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_62 vals ->
                                                             concatMap
                                                               (\ v_63 ->
                                                                  pure DistTo <*> mlbs v_62 v10 <*>
                                                                    pure v_63)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsDistTo db))])
                                               Q.empty
                                               (M.foldlWithKey'
                                                  (\ rest (v_64, v_65) vals ->
                                                     S.foldl'
                                                       (\ acc v_66 -> Edge v_64 v_65 v_66 : acc)
                                                       []
                                                       vals
                                                       ++ rest)
                                                  []
                                                  (M.singleton (v_56, v_57) (S.singleton v_58)))]

enumDistTo :: DataBase -> [DistTo]
enumDistTo db
  = M.foldlWithKey'
      (\ rest v_69 vals ->
         S.foldl' (\ acc v_70 -> DistTo v_69 v_70 : acc) [] vals ++ rest)
      []
      (factsDistTo db)

closerThan :: Dist -> DataBase -> [DistTo]
closerThan v_72 db
  = M.foldlWithKey'
      (\ rest v_73 vals ->
         concatMap (\ v_74 -> pure DistTo <*> pure v_73 <*> mlbs v_74 v_72)
           vals
           ++ rest)
      []
      (factsDistTo db)

distTo :: String -> DataBase -> [DistTo]
distTo v_75 db
  = M.foldlWithKey'
      (\ rest v_77 vals ->
         concatMap (\ v_78 -> pure DistTo <*> mlbs v_77 v_75 <*> pure v_78)
           vals
           ++ rest)
      []
      (factsDistTo db)

reachableIn :: String -> Dist -> DataBase -> [DistTo]
reachableIn v_79 v_80 db
  = M.foldlWithKey'
      (\ rest v_81 vals ->
         concatMap
           (\ v_82 -> pure DistTo <*> mlbs v_81 v_79 <*> mlbs v_82 v_80)
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