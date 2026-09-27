module Pareto.FixenWithPriorities where

----- FIXEN IMPORTS -----
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import Control.Monad
import qualified Data.PQueue.Max as Q

----- USER IMPORTS -----
import Numeric.Natural
import Control.DeepSeq


----- USER CODE START -----
type Vertex = String

distMlbs :: Natural -> Natural -> [Natural]
distMlbs x y = [max x y]

instance NFData Database where
  rnf (Database x y) = rnf (x, y)

instance NFData Fact where
  rnf (DistTo x y z) = rnf (x, y, z)
  rnf (Edge a b c d) = rnf (a, b, c, d)
----- USER CODE END -----

----- FACTS -----
data Fact = DistTo Vertex Natural Natural
            | Edge Vertex Vertex Natural Natural deriving (Show, Eq)

factLeq :: Fact -> Fact -> Bool
factLeq (DistTo v0 v1 v2) (DistTo v'0 v'1 v'2) = ((==) v0 v'0 && ((>=) v1 v'1 && (>=) v2 v'2))
factLeq (Edge v0 v1 v2 v3) (Edge v'0 v'1 v'2 v'3) =
    ((==) v0 v'0 && ((==) v1 v'1 && ((>=) v2 v'2 && (>=) v3 v'3)))
factLeq _ _ = False

maximalContour :: [Fact] -> [Fact]
maximalContour [] = []
maximalContour [x] = [x]
maximalContour (x : xs) =
  if any (factLeq x) xs
  then maximalContour xs
  else x : maximalContour (filter (\x' -> not (factLeq x' x)) xs)

data Database = Database {_factsDistTo :: HashMap Vertex (HashSet (Natural, Natural))
                , _factsEdge :: HashMap Vertex (HashMap Vertex (HashSet ( Natural
                , Natural )))} deriving (Eq)

emptyDb :: Database
emptyDb = Database {_factsDistTo = HashMap.empty, _factsEdge = HashMap.empty}

infix 0 |=
(|=) :: Database -> Fact -> Bool
(|=) db (DistTo _v0 _v1 _v2) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsDistTo db);
      return (any (\(_t0, _t1) -> ((>=) _v1 _t0 && (>=) _v2 _t1)) step0)
    })
(|=) db (Edge _v0 _v1 _v2 _v3) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsEdge db);
      return (fromMaybe False (do {
        step1 <- HashMap.lookup _v1 step0;
        return (any (\(_t0, _t1) -> ((>=) _v2 _t0 && (>=) _v3 _t1)) step1)
      }))
    })

insertToDb :: Database -> Fact -> Database
insertToDb db (DistTo _v0 _v1 _v2) =
    db {_factsDistTo = let {
      new_fact = HashMap.singleton _v0 (HashSet.singleton (_v1, _v2))
    } in HashMap.unionWith (\s1 s2 -> HashSet.union s1 (HashSet.filter (\( _t0
    , _t1 ) -> not ((/=) _t0 _v1 && ((>=) _t0 _v1 && ((/=) _t1 _v2 && (>=) _t1 _v2)))) s2)) new_fact (_factsDistTo db)}
insertToDb db (Edge _v0 _v1 _v2 _v3) =
    db {_factsEdge = let {
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton (_v2, _v3)))
    } in HashMap.unionWith (HashMap.unionWith (\s1 s2 -> HashSet.union s1 (HashSet.filter (\( _t0
    , _t1 ) -> not ((/=) _t0 _v2 && ((>=) _t0 _v2 && ((/=) _t1 _v3 && (>=) _t1 _v3)))) s2))) new_fact (_factsEdge db)}

mergeContour :: Fact -> Database -> [Fact]
mergeContour f@(DistTo _ _ _) _ = [f]
mergeContour f@(Edge _ _ _ _) _ = [f]

data RuleInstance = Init Fact
                    | RuleAddDist Vertex Vertex Natural Natural Natural Natural deriving (Show)

evaluate :: RuleInstance -> [Fact]
evaluate (Init f) = [f]
evaluate (RuleAddDist a b c c' d d') = [DistTo b ((+) d d') ((+) c c')]

instance Eq RuleInstance where
  f == f' = not (f < f' || f' < f)

instance Ord RuleInstance where {
    (<=) i i' = not ((<) i' i);
    (<) (Init _) (Init _) = False;
    (<) _ (Init _) = True;
    (<) (RuleAddDist _ _ c1 c1' d1 d1') (RuleAddDist _ _ c2 c2' d2 d2') =
      (||) ((>) ((+) d1 d1') ((+) d2 d2')) ((&&) ((==) ((+) d1 d1') ((+) d2 d2')) ((>) ((+) c1 c1') ((+) c2 c2')));
    (<) _ _ = False
  }

type Queue = Q.MaxQueue RuleInstance



----- STEP FUNCTION -----

step :: Database -> Fact -> Queue -> Queue
step db fact q =
    case fact of {
      DistTo _t0 _t1 _t2 -> Q.union q (Q.fromList (do {
        let { _v0_0 = _t0 };
        let { _v1_0 = _t1 };
        let { _v2_0 = _t2 };
        step0 <- maybeToList (HashMap.lookup _v0_0 (_factsEdge db));
        (_v3_0, step1) <- HashMap.toList step0;
        (_v4_0, _v5_0) <- HashSet.toList step1;
        let { rule_instance = RuleAddDist _v0_0 _v3_0 _v2_0 _v5_0 _v1_0 _v4_0 };
        return rule_instance
      }));
      Edge _t0 _t1 _t2 _t3 -> Q.union q (Q.fromList (do {
        let { _v0_0 = _t0 };
        let { _v1_0 = _t1 };
        let { _v2_0 = _t2 };
        let { _v3_0 = _t3 };
        step0 <- maybeToList (HashMap.lookup _v0_0 (_factsDistTo db));
        (_v4_0, _v5_0) <- HashSet.toList step0;
        let { rule_instance = RuleAddDist _v0_0 _v1_0 _v5_0 _v3_0 _v4_0 _v2_0 };
        return rule_instance
      }))
    }

stepAll :: Database -> [Fact] -> Queue -> Queue
stepAll db xs q = foldl' (\q' f -> step db f q') q xs

----- SOLVER -----

loop :: Queue -> Database -> Database
loop q db =
    case Q.maxView q of {
      Just (p, q') -> let { fs = evaluate p } in case foldl' _fixenProcessConclusion ( q'
      , db ) fs of { (next_q, next_state) -> loop next_q next_state };
      Nothing -> db
    }
_fixenProcessConclusion :: (Queue, Database) -> Fact -> (Queue, Database)
_fixenProcessConclusion (q', db) f =
    if (|=) db f then (q', db) else let {
      c = mergeContour f db;
      new_facts = filter (\candidate -> not ((|=) db candidate)) (maximalContour c);
      new_db = foldl' insertToDb db new_facts
    } in let { next_q = stepAll new_db new_facts q' } in seq next_q (next_q, new_db)
solve :: [Fact] -> Database
solve = reSolve emptyDb

reSolve :: Database -> [Fact] -> Database
reSolve db f = let { q = Q.fromList (concat [map (\fact -> Init fact) f]) } in loop q db

----- QUERIES -----

distTo :: Vertex -> Database -> [Fact]
distTo _v0_0 db =
    do {
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsDistTo db));
      (_v1_0, _v2_0) <- HashSet.toList step0;
      return (DistTo _v0_0 _v1_0 _v2_0)
    }

reachableIn :: Natural -> Natural -> Database -> [Fact]
reachableIn _v1_0 _v2_0 db =
    do {
      (_v0_0, step0) <- HashMap.toList (_factsDistTo db);
      (_v1_1, _v2_1) <- HashSet.toList step0;
      guard ((>=) _v1_0 _v1_1);
      guard ((>=) _v2_0 _v2_1);
      return (DistTo _v0_0 _v1_1 _v2_1)
    }

distances :: Database -> [Fact]
distances db =
    do {
      (_v0_0, step0) <- HashMap.toList (_factsDistTo db);
      (_v1_0, _v2_0) <- HashSet.toList step0;
      return (DistTo _v0_0 _v1_0 _v2_0)
    }

