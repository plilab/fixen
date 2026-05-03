module StaticAnalysis.ReducedProduct where

----- FIXEN IMPORTS-----
import Data.Maybe
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

----- USER IMPORTS -----
import Demos.Program
----- USER CODE -----
joinI :: Void
joinI = undefined

type Label = Nat

joinP :: Void
joinP = undefined

-- whatever exchange and widening functions we need
reducedExchangeI :: StateI -> StateP -> StateI
reducedExchangeI =  undefined
reducedExchangeP :: StateI -> StateP -> StateP
reducedExchangeP =  undefined

----- FACTS -----
data Fact = Assign Label String Expr
          | Cond Label Expr Label Label
          | Seq Label Label
          | StateBeforeI Label ((HashMap String) (Int, Int))
          | StateBeforeP Label ((HashMap String) Parity)
          | Var Label String
  deriving Eq

----- FACT DATABASE -----
type AssignFacts = HashMap Label (HashMap String (HashSet Expr))
type CondFacts = HashMap Label (HashMap Label (HashMap Label (HashSet Expr)))
type SeqFacts = HashMap Label (HashSet Label)
type StateBeforeIFacts = HashMap Label (HashSet ((HashMap String) (Int, Int)))
type StateBeforePFacts = HashMap Label (HashSet ((HashMap String) Parity))
type VarFacts = HashMap Label (HashSet String)

data Database = Database
  { _factsAssign :: AssignFacts
  , _factsCond :: CondFacts
  , _factsSeq :: SeqFacts
  , _factsStateBeforeI :: StateBeforeIFacts
  , _factsStateBeforeP :: StateBeforePFacts
  , _factsVar :: VarFacts
  } deriving Eq

type Interpretation = (Database, Database, Database)

----- ENTAILMENT -----
infix 0 |=

(|=) :: Database -> Fact -> Bool
db |= (Assign _v0 _v1 _v2) =
  let db' = _factsAssign db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v1
        return $ _v2 `HashSet.member` step2
db |= (Cond _v0 _v1 _v2 _v3) =
  let db' = _factsCond db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v2
        step3 <- step2 HashMap.!? _v3
        return $ _v1 `HashSet.member` step3
db |= (Seq _v0 _v1) =
  let db' = _factsSeq db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ _v1 `HashSet.member` step1
db |= (StateBeforeI _v0 _v1) =
  let db' = _factsStateBeforeI db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ any (stateILeq _v1) step1
db |= (StateBeforeP _v0 _v1) =
  let db' = _factsStateBeforeP db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ any (statePLeq _v1) step1
db |= (Var _v0 _v1) =
  let db' = _factsVar db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ _v1 `HashSet.member` step1