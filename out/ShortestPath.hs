module Demos.ShortestPath where

----- FIXEN IMPORTS-----
import Data.Maybe
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

----- USER CODE -----
type Vertex = Int

distMlbs :: Int -> Int -> [Int]
distMlbs x y = [(min x y)]

----- FACTS -----
data Fact = DistTo Vertex Int
          | Edge Vertex Vertex Int
  deriving Eq

----- FACT DATABASE -----
type DistToFacts = HashMap Vertex (HashSet Int)
type EdgeFacts = HashMap Vertex (HashMap Vertex (HashSet Int))

data Database = Database
  { _factsDistTo :: DistToFacts
  , _factsEdge :: EdgeFacts
  } deriving Eq

emptyDb :: Database
emptyDb = Database
  { _factsDistTo = HashMap.empty
  , _factsEdge = HashMap.empty
  }

type Interpretation = Database

----- ENTAILMENT -----
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

----- INSERTION -----
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
                      (\_t -> not ((>=) _t _v1))
                      s2
                    )
                )
              ) new_fact mp
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
                        (\_t -> not ((>=) _t _v2))
                        s2
                      )
                  )
                )
              ) new_fact mp
   in Just db { _factsEdge = mp' }

----- RULE INSTANCES -----
data RuleInstance
       = RuleAddDist Vertex Vertex Int Int
       | Init Fact
  deriving Eq