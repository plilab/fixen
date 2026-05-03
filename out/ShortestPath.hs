module Demos.ShortestPath where

----- FIXEN IMPORTS-----
import Data.Maybe
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

----- USER CODE -----
type Vertex = Int

distLeq :: Int -> Int -> Bool
distLeq x y = x <= y

distMlbs :: Int -> Int -> [Int]
distMlbs x y = [(min x y)]

----- FACTS -----
data Fact = DistTo Vertex Int
          | Edge Int Vertex Vertex
          | Exists
  deriving Eq

----- FACT DATABASE -----
type DistToFacts = HashMap Vertex (HashSet Int)
type EdgeFacts = HashMap Vertex (HashMap Vertex (HashSet Int))
type ExistsFacts = Bool

data Database = Database
  { _factsDistTo :: DistToFacts
  , _factsEdge :: EdgeFacts
  , _factsExists :: ExistsFacts
  } deriving Eq

type Interpretation = Database

----- ENTAILMENT -----
infix 0 |=

(|=) :: Database -> Fact -> Bool
db |= (DistTo _v0 _v1) =
  let db' = _factsDistTo db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ any (distLeq _v1) step1
db |= (Edge _v0 _v1 _v2) =
  let db' = _factsEdge db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v2
        step2 <- step1 HashMap.!? _v1
        return $ any (distLeq _v0) step2
db |= Exists = _factsExists db