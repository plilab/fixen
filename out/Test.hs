module Test where

----- FIXEN IMPORTS -----
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe
import Control.Monad
import qualified Data.PQueue.Max as Q



----- FACTS -----
data Fact = R String String
          | T String String String
  deriving (Show, Eq)

----- FACT DATABASE -----
data Database = Database
  { _factsR :: HashMap String (HashSet String)
  , _factsT :: HashMap String (HashMap String (HashSet String))
  } deriving Eq

emptyDb :: Database
emptyDb = Database
  { _factsR = HashMap.empty
  , _factsT = HashMap.empty
  }

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

----- INSERTION -----
insertToDb :: Database -> Fact -> Maybe Database
insertToDb db fact
  | db |= fact = Nothing
insertToDb db (R _v0 _v1) =
  let mp = _factsR db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              (HashSet.union)
              new_fact
              mp
   in Just db { _factsR = mp' }
insertToDb db (T _v0 _v1 _v2) =
  let mp = _factsT db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton _v2))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                (HashSet.union))
              new_fact
              mp
   in Just db { _factsT = mp' }

----- RULE INSTANCES -----
data RuleInstance
       = RuleTransitive String String String
       | RuleReflexive String
       | RuleReflexive' String
       | Init Fact
  deriving Show

type Queue = Q.MaxQueue RuleInstance

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

  _ < _ = False

evaluate :: RuleInstance -> Fact
evaluate (RuleTransitive b c d) = R b d
evaluate (RuleReflexive a) = R a a
evaluate (RuleReflexive' y) = R y y
evaluate (Init f) = f

----- STEP FUNCTION -----
step :: Database -> Fact -> Queue -> Queue 
step db fact q = case fact of
    R _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        concat [
          do
            (_v2_0, step0) <- HashMap.toList (_factsR db)
            guard (_v0_0 `HashSet.member` step0)
            return $ RuleTransitive _v2_0 _v0_0 _v1_0,
          do
            step0 <- maybeToList (_factsR db HashMap.!? _v1_0)
            _v2_0 <- HashSet.toList step0
            return $ RuleTransitive _v0_0 _v1_0 _v2_0,
          do
            return $ RuleReflexive _v0_0,
          do
            return $ RuleReflexive' _v1_0
          ]
    _ -> q

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
r :: String -> String -> Database -> [Fact]
r _v0_0 _v1_0 db = do
  step0 <- maybeToList (_factsR db HashMap.!? _v0_0)
  guard (_v1_0 `HashSet.member` step0)
  return $ R _v0_0 _v1_0