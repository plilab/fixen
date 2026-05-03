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

emptyDb :: Database
emptyDb = Database
  { _factsAssign = HashMap.empty
  , _factsCond = HashMap.empty
  , _factsSeq = HashMap.empty
  , _factsStateBeforeI = HashMap.empty
  , _factsStateBeforeP = HashMap.empty
  , _factsVar = HashMap.empty
  }

type Interpretation = (Database, Database, Database)

data Phase = Phase1
           | Phase2
           | Phase3
 deriving (Eq, Show)

selectDb :: Interpretation -> Phase -> Database
selectDb (db, _, _) Phase1 = db
selectDb (_, db, _) Phase2 = db
selectDb (_, _, db) Phase3 = db

(||=) :: Interpretation -> Fact -> Phase -> Bool
(i ||= f) p = selectDb i p |= f

infix 1 ||=

replaceDb :: Interpretation -> Database -> Phase -> Interpretation
replaceDb (_, db2, db3) db' Phase1 = (db', db2, db3)
replaceDb (db1, _, db3) db' Phase2 = (db1, db', db3)
replaceDb (db1, db2, _) db' Phase3 = (db1, db2, db')

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

----- INSERTION -----
insertToDb :: Database -> Fact -> Maybe Database
insertToDb db fact
  | db |= fact = Nothing
insertToDb db (Assign _v0 _v1 _v2) =
  let mp = _factsAssign db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton _v2))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                (HashSet.union
                )
              ) new_fact mp
   in Just db { _factsAssign = mp' }
insertToDb db (Cond _v0 _v1 _v2 _v3) =
  let mp = _factsCond db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v2 (HashMap.singleton _v3 (HashSet.singleton _v1)))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                (HashMap.unionWith
                  (HashSet.union
                  )
                )
              ) new_fact mp
   in Just db { _factsCond = mp' }
insertToDb db (Seq _v0 _v1) =
  let mp = _factsSeq db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              (HashSet.union
              ) new_fact mp
   in Just db { _factsSeq = mp' }
insertToDb db (StateBeforeI _v0 _v1) =
  let mp = _factsStateBeforeI db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              ((\s1 s2 ->
                  HashSet.union
                    s1
                    (HashSet.filter
                      (\_t -> not (stateILeq _t _v1))
                      s2
                    )
                )
              ) new_fact mp
   in Just db { _factsStateBeforeI = mp' }
insertToDb db (StateBeforeP _v0 _v1) =
  let mp = _factsStateBeforeP db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              ((\s1 s2 ->
                  HashSet.union
                    s1
                    (HashSet.filter
                      (\_t -> not (statePLeq _t _v1))
                      s2
                    )
                )
              ) new_fact mp
   in Just db { _factsStateBeforeP = mp' }
insertToDb db (Var _v0 _v1) =
  let mp = _factsVar db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              (HashSet.union
              ) new_fact mp
   in Just db { _factsVar = mp' }

insertToInterpretation :: Interpretation -> Fact -> Phase -> Maybe Interpretation
insertToInterpretation i f p = do
  let db = selectDb i p
  db' <- insertToDb db f
  return (replaceDb i db' p)

----- RULE INSTANCES -----
data RuleInstance
       = RuleReducedExchangeP Label ((HashMap String) Parity) ((HashMap String) (Int, Int))
       | RuleReducedExchangeI Label ((HashMap String) Parity) ((HashMap String) (Int, Int))
       | RuleWidenInterval Label ((HashMap String) (Int, Int))
       | RuleWidenParity Label ((HashMap String) Parity)
       | RuleAssignInitP Label
       | RuleCondInitP Label
       | RuleVarInitP Label
       | RuleAssignStepP Expr Label Label ((HashMap String) Parity) ((HashMap String) Parity) String
       | RuleEvalCondTP Expr Label ((HashMap String) Parity) ((HashMap String) Parity) Label
       | RuleEvalCondFP Expr Label Label ((HashMap String) Parity) ((HashMap String) Parity)
       | RuleAssignInitI Label
       | RuleCondInitI Label
       | RuleVarInitI Label
       | RuleAssignStepI Expr Label Label ((HashMap String) (Int, Int)) ((HashMap String) (Int, Int)) String
       | RuleEvalCondTI Expr Label ((HashMap String) (Int, Int)) ((HashMap String) (Int, Int)) Label
       | RuleEvalCondFI Expr Label Label ((HashMap String) (Int, Int)) ((HashMap String) (Int, Int))
       | Init Fact
  deriving Eq