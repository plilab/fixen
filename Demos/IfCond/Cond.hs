{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
module IfCond.Cond where
import IfCond.IfCond
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

data R = R Ordering
           deriving (Eq, Show, Generic)

instance Hashable R

instance PartialOrd R where
        leq (R v0) (R v0') = (v0 `leq` v0')
mkR v0 = RFact (R v0)

data Fact = RFact R
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | RulCont Ordering
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (RulCont x0) = [RFact (R GT)]

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsR :: S.HashSet Ordering}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase S.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          RFact (R v0) -> first (\ hset -> db{factsR = hset})
                            (update (factsR db) v0)

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_14
  = case fact of
        RFact (R v_15) -> Q.unions
                            [q_14,
                             foldl'
                               (\ q_16 (R x0) ->
                                  Q.unions
                                    [q_16,
                                     if eq x0 LT then Q.unions [q_16, Q.singleton (RulCont x0)] else
                                       Q.empty])
                               Q.empty
                               (S.foldl' (\ rest v_17 -> R v_17 : rest) [] (S.singleton v_15))]

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