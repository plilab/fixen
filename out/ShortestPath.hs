module Demos.ShortestPath where

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


----- USER CODE START -----
type Vertex = String

distMlbs :: Natural -> Natural -> [Natural]
distMlbs x y = [(max x y)]

instance NFData Database where
  rnf (Database x y) = rnf (x, y)
----- USER CODE END -----

----- FACTS -----
data Fact = DistTo Vertex Natural
          | Edge Vertex Vertex Natural
  deriving (Show, Eq)

----- FACT DATABASE -----
data Database = Database
  { _factsDistTo :: HashMap Vertex (HashSet Natural)
  , _factsEdge :: HashMap Vertex (HashMap Vertex (HashSet Natural))
  } deriving Eq

emptyDb :: Database
emptyDb = Database
  { _factsDistTo = HashMap.empty
  , _factsEdge = HashMap.empty
  }

infix 0 |=

(|=) :: Database -> Fact -> Bool
db |= (DistTo _v0 _v1) =
  let db' = _factsDistTo db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ any ((>=) _v1) step1
db |= (Edge _v0 _v1 _v2) =
  let db' = _factsEdge db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v1
        return $ any ((>=) _v2) step2

insertToDb :: Database -> Fact -> Maybe Database
insertToDb db fact
  | db |= fact = Nothing
insertToDb db (DistTo _v0 _v1) =
  let mp = _factsDistTo db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              ((\s1 s2 ->
                  HashSet.union
                    s1
                    (HashSet.filter
                      (\_t -> not (_t /= _v1 && (>=) _t _v1))
                      s2)))
              new_fact
              mp
   in Just db { _factsDistTo = mp' }
insertToDb db (Edge _v0 _v1 _v2) =
  let mp = _factsEdge db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton _v2))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                ((\s1 s2 ->
                    HashSet.union
                      s1
                      (HashSet.filter
                        (\_t -> not (_t /= _v2 && (>=) _t _v2))
                        s2))))
              new_fact
              mp
   in Just db { _factsEdge = mp' }

----- RULE INSTANCES -----
data RuleInstance
       = Init Fact
       | RuleAddDist Vertex Vertex Natural Natural
  deriving Show

evaluate :: RuleInstance -> Fact
evaluate (Init f) = f
evaluate (RuleAddDist a b d d') = DistTo b (d + d')

instance Eq RuleInstance where
  f == f'
    | f < f' = False
    | f' < f = False
    | otherwise = True

instance Ord RuleInstance where
  i <= i' = not (i' < i)
  ----- PRIORITIES -----
  Init _ < Init _ = False
  _ < Init _ = True
  (RuleAddDist _ _ d1 d1') < (RuleAddDist _ _ d2 d2') = (d1 + d1') > (d2 + d2')
  _ < _ = False

type Queue = Q.MaxQueue RuleInstance


----- STEP FUNCTION -----

step :: Database -> Fact -> Queue -> Queue 
step db fact q = case fact of
    DistTo _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        step0 <- maybeToList (_factsEdge db HashMap.!? _v0_0)
        (_v2_0, step1) <- HashMap.toList step0
        _v3_0 <- HashSet.toList step1
        return $ RuleAddDist _v0_0 _v2_0 _v1_0 _v3_0
    Edge _t0 _t1 _t2 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        let _v2_0 = _t2
        step0 <- maybeToList (_factsDistTo db HashMap.!? _v0_0)
        _v3_0 <- HashSet.toList step0
        return $ RuleAddDist _v0_0 _v1_0 _v3_0 _v2_0


----- SOLVER -----

loop :: Queue -> Database -> Database
loop q db
  | Just (p, q') <- Q.maxView q =
    let f = evaluate p
     in case insertToDb db f of
          Nothing -> loop q' db
          Just db' -> loop (step db' f q') db'
  | otherwise = db

solve :: [Fact] -> Database
solve = reSolve emptyDb


reSolve :: Database -> [Fact] -> Database
reSolve db f =
  let q = Q.fromList $ concat [
            Init <$> f
          ]
   in loop q db


----- QUERIES -----

distTo :: Vertex -> Database -> [Fact]
distTo _v0_0 db = do
  step0 <- maybeToList (_factsDistTo db HashMap.!? _v0_0)
  _v1_0 <- HashSet.toList step0
  return $ DistTo _v0_0 _v1_0

reachableIn :: Natural -> Database -> [Fact]
reachableIn _v1_0 db = do
  (_v0_0, step0) <- HashMap.toList (_factsDistTo db)
  _v1_1 <- HashSet.toList step0
  guard ((>=) _v1_0 _v1_1)
  return $ DistTo _v0_0 _v1_1

distances :: Database -> [Fact]
distances db = do
  (_v0_0, step0) <- HashMap.toList (_factsDistTo db)
  _v1_0 <- HashSet.toList step0
  return $ DistTo _v0_0 _v1_0

