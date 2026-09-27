module ShortestPath.FixenNoPriorities where

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

instance NFData Database where
  rnf (Database x y) = rnf (x, y)

instance NFData Fact where
  rnf (DistTo x y) = rnf (x, y)
  rnf (Edge a b c) = rnf (a, b, c)
----- USER CODE END -----

----- FACTS -----
data Fact = DistTo Vertex Natural
            | Edge Vertex Vertex Natural deriving (Show, Eq)

factLeq :: Fact -> Fact -> Bool
factLeq (DistTo v0 v1) (DistTo v'0 v'1) = ((==) v0 v'0 && (>=) v1 v'1)
factLeq (Edge v0 v1 v2) (Edge v'0 v'1 v'2) = ((==) v0 v'0 && ((==) v1 v'1 && (>=) v2 v'2))
factLeq _ _ = False

maximalContour :: [Fact] -> [Fact]
maximalContour [] = []
maximalContour [x] = [x]
maximalContour (x : xs) =
  if any (factLeq x) xs
  then maximalContour xs
  else x : maximalContour (filter (\x' -> not (factLeq x' x)) xs)

data Database = Database {_factsDistTo :: HashMap Vertex Natural
                , _factsEdge :: HashMap Vertex (HashMap Vertex Natural)} deriving (Eq)

emptyDb :: Database
emptyDb = Database {_factsDistTo = HashMap.empty, _factsEdge = HashMap.empty}

infix 0 |=
(|=) :: Database -> Fact -> Bool
(|=) db (DistTo _v0 _v1) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsDistTo db);
      return (let { _t0 = step0 } in (>=) _v1 _t0)
    })
(|=) db (Edge _v0 _v1 _v2) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsEdge db);
      return (fromMaybe False (do {
        step1 <- HashMap.lookup _v1 step0;
        return (let { _t0 = step1 } in (>=) _v2 _t0)
      }))
    })

insertToDb :: Database -> Fact -> Database
insertToDb db (DistTo _v0 _v1) =
    db {_factsDistTo = let {
      new_fact = HashMap.singleton _v0 _v1
    } in HashMap.unionWith const new_fact (_factsDistTo db)}
insertToDb db (Edge _v0 _v1 _v2) =
    db {_factsEdge = let {
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 _v2)
    } in HashMap.unionWith (HashMap.unionWith const) new_fact (_factsEdge db)}

mergeContour :: Fact -> Database -> [Fact]
mergeContour f@(DistTo _v0 _v1) db =
    (f : (do {
      step0 <- maybeToList (HashMap.lookup _v0 (_factsDistTo db));
      let { _t0 = step0 };
      let { joined1 = min _t0 _v1 };
      return (DistTo _v0 joined1)
    }))
mergeContour f@(Edge _v0 _v1 _v2) db =
    (f : (do {
      step0 <- maybeToList (HashMap.lookup _v0 (_factsEdge db));
      step1 <- maybeToList (HashMap.lookup _v1 step0);
      let { _t0 = step1 };
      let { joined2 = min _t0 _v2 };
      return (Edge _v0 _v1 joined2)
    }))

data RuleInstance = Init Fact
                    | RuleAddDist Vertex Vertex Natural Natural deriving (Show)

evaluate :: RuleInstance -> [Fact]
evaluate (Init f) = [f]
evaluate (RuleAddDist a b d d') = [DistTo b ((+) d d')]

instance Eq RuleInstance where
  f == f' = not (f < f' || f' < f)

instance Ord RuleInstance where { (<=) _ (Init _) = True; (<=) _ _ = False }

type Queue = Q.MaxQueue RuleInstance



----- STEP FUNCTION -----

step :: Database -> Fact -> Queue -> Queue
step db fact q =
    case fact of {
      DistTo _t0 _t1 -> Q.union q (Q.fromList (do {
        let { _v0_0 = _t0 };
        let { _v1_0 = _t1 };
        step0 <- maybeToList (HashMap.lookup _v0_0 (_factsEdge db));
        (_v2_0, step1) <- HashMap.toList step0;
        let { _v3_0 = step1 };
        let { rule_instance = RuleAddDist _v0_0 _v2_0 _v1_0 _v3_0 };
        return rule_instance
      }));
      Edge _t0 _t1 _t2 -> Q.union q (Q.fromList (do {
        let { _v0_0 = _t0 };
        let { _v1_0 = _t1 };
        let { _v2_0 = _t2 };
        step0 <- maybeToList (HashMap.lookup _v0_0 (_factsDistTo db));
        let { _v3_0 = step0 };
        let { rule_instance = RuleAddDist _v0_0 _v1_0 _v3_0 _v2_0 };
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
      let { _v1_0 = step0 };
      return (DistTo _v0_0 _v1_0)
    }

reachableIn :: Natural -> Database -> [Fact]
reachableIn _v1_0 db =
    do {
      (_v0_0, step0) <- HashMap.toList (_factsDistTo db);
      let { _v1_1 = step0 };
      guard ((>=) _v1_0 _v1_1);
      return (DistTo _v0_0 _v1_1)
    }

distances :: Database -> [Fact]
distances db =
    do {
      (_v0_0, step0) <- HashMap.toList (_factsDistTo db);
      let { _v1_0 = step0 };
      return (DistTo _v0_0 _v1_0)
    }

