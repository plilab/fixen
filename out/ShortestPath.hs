module Demos.ShortestPath where


----- USER CODE START -----
type Vertex = String

distLeq :: Dist -> Dist -> Bool
distLeq (D x) (D y) = x <= y

distMlbs :: Dist -> Dist -> [Dist]
distMlbs (D x) (D y) = [D (min x y)]
----- USER CODE END -----


----- FACTS -----
data Fact = DistTo Vertex Int
          | Edge Vertex Vertex Int
  deriving Eq

----- Database Representations -----
type DistToFacts = HashMap Vertex (HashSet Int)
type EdgeFacts = HashMap Vertex (HashMap Vertex (HashSet Int))

----- Database -----
data Database = Database
  { _factsDistTo :: DistToFacts
  , _factsEdge :: EdgeFacts
  } deriving Eq

type Interpretation = Database

----- Subsumption -----
class Subsumable a where
    subsumes :: a -> a -> Bool
    mlbs :: a -> a -> [a]

instance Subsumable Int where
  subsumes = distLeq
  mlbs = distMlbs

instance Subsumable Vertex where
  subsumes = (==)
  mlbs _a _b
    | _a == _b = [_a]
    | otherwise = []