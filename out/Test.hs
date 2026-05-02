module Test where



----- FACTS -----
data Fact = R String String
  deriving Eq

----- Database Representations -----
type RFacts = HashMap String (HashSet String)

----- Database -----
data Database = Database
  { _factsR :: RFacts
  } deriving Eq

type Interpretation = Database