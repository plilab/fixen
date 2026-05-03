module Test where

----- FIXEN IMPORTS-----
import Data.Maybe
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap



----- FACTS -----
data Fact = R String String
          | T String String String
  deriving Eq

----- FACT DATABASE -----
type RFacts = HashMap String (HashSet String)
type TFacts = HashMap String (HashMap String (HashSet String))

data Database = Database
  { _factsR :: RFacts
  , _factsT :: TFacts
  } deriving Eq

type Interpretation = Database

----- ENTAILMENT -----
infix 0 |=

(|=) :: Database -> Fact -> Bool
db |= (R _v0 _v1) =
  let db' = _factsR db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ _v1 `HashSet.member` step1
db |= (T _v0 _v1 _v2) =
  let db' = _factsT db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v1
        return $ _v2 `HashSet.member` step2

----- RULE INSTANCES -----
data RuleInstance = RuleTransitive String String String
                  | RuleReflexive String
                  | RuleReflexive' String
  deriving Eq