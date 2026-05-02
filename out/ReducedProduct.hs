module StaticAnalysis.ReducedProduct where
import Demos.Program

----- USER CODE START -----
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
----- USER CODE END -----


----- FACTS -----
data Fact = Assign Label String Expr
          | Cond Label Expr Label Label
          | Seq Label Label
          | StateBeforeI Label ((HashMap String) (Int, Int))
          | StateBeforeP Label ((HashMap String) Parity)
          | Var Label String
  deriving Eq

----- Database Representations -----
type AssignFacts = HashMap Label (HashMap String (HashSet Expr))
type CondFacts = HashMap Label (HashMap Label (HashMap Label (HashSet Expr)))
type SeqFacts = HashMap Label (HashSet Label)
type StateBeforeIFacts = HashMap Label (HashSet ((HashMap String) (Int, Int)))
type StateBeforePFacts = HashMap Label (HashSet ((HashMap String) Parity))
type VarFacts = HashMap Label (HashSet String)

----- Database -----
data Database = Database
  { _factsAssign :: AssignFacts
  , _factsCond :: CondFacts
  , _factsSeq :: SeqFacts
  , _factsStateBeforeI :: StateBeforeIFacts
  , _factsStateBeforeP :: StateBeforePFacts
  , _factsVar :: VarFacts
  } deriving Eq

type Interpretation = (Database, Database, Database)

----- Subsumption -----
class Subsumable a where
    subsumes :: a -> a -> Bool
    mlbs :: a -> a -> [a]

instance Subsumable Expr where
  subsumes = (==)
  mlbs _a _b
    | _a == _b = [_a]
    | otherwise = []

instance Subsumable Label where
  subsumes = (==)
  mlbs _a _b
    | _a == _b = [_a]
    | otherwise = []

instance Subsumable ((HashMap String) (Int, Int)) where
  subsumes = stateILeq
  mlbs = stateIMlbs

instance Subsumable ((HashMap String) Parity) where
  subsumes = statePLeq
  mlbs = statePMlbs

instance Subsumable String where
  subsumes = (==)
  mlbs _a _b
    | _a == _b = [_a]
    | otherwise = []