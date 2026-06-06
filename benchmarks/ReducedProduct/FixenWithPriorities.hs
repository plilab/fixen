module ReducedProduct.FixenWithPriorities where

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
import Algebra.PartialOrd
import GHC.Generics
import Control.DeepSeq
import Data.Hashable


----- USER CODE START -----
stateILeq :: HashMap String Interval -> HashMap String Interval -> Bool
stateILeq = leq

meetI :: HashMap String Interval -> HashMap String Interval -> HashMap String Interval
meetI x y = HashMap.intersectionWith meetInterval x y

stateIMlbs :: HashMap String Interval -> HashMap String Interval -> [HashMap String Interval]
stateIMlbs x y = [HashMap.intersectionWith meetInterval x y]

data Interval = Pair (Natural, Natural) | ITop | IBot
    deriving (Eq, Show, Generic)

instance NFData Interval where
    rnf (Pair x) = rnf x
    rnf ITop = ()
    rnf IBot = ()

instance Hashable Interval

joinInterval :: Interval -> Interval -> Interval
joinInterval ITop _ = ITop
joinInterval _ ITop = ITop
joinInterval IBot s = s
joinInterval s IBot = s
joinInterval (Pair (a, b)) (Pair (c, d)) = Pair (min a c, max b d)

instance PartialOrd Interval where
    leq IBot _ = True
    leq _ ITop = True
    leq ITop _ = False
    leq _ IBot = False
    leq (Pair (a, b)) (Pair (c, d)) = a >= c && b <= d

meetInterval :: Interval -> Interval -> Interval
meetInterval s1 s2 = case (s1, s2) of
        (Pair (a, b), Pair (c, d)) -> if max a c <= min b d then Pair (max a c, min b d) else IBot
        (_, _) -> IBot

data BBool = BTop | BFalse | BTrue | BBot
    deriving (Eq, Show, Generic)

instance Hashable BBool

instance PartialOrd BBool where
    leq BBot _ = True
    leq _ BTop = True
    leq s1 s2 = s1 == s2

joinBBool :: BBool -> BBool -> BBool
joinBBool s1 s2 = case (s1, s2) of
        (BTrue, BTrue) -> BTrue
        (BFalse, BFalse) -> BFalse
        (BFalse, BTrue) -> BTop
        (BTrue, BFalse) -> BTop
        (BTop, _) -> BTop
        (_, BTop) -> BTop
        (rest, BBot) -> rest
        (BBot, rest) -> rest


joinI :: HashMap String Interval -> HashMap String Interval -> HashMap String Interval
joinI = HashMap.unionWith joinInterval

insertI :: String -> Interval -> HashMap String Interval -> HashMap String Interval
insertI = HashMap.insertWith joinInterval

evalI :: Expr -> HashMap String Interval -> Interval
evalI e st = case e of
    Num n -> Pair (fromIntegral n, fromIntegral n)
    Id x -> fromMaybe IBot (HashMap.lookup x st)
    InputE -> ITop
    Plus e1 e2 -> case (evalI e1 st, evalI e2 st) of
        (Pair (a, b), Pair (c, d)) -> Pair (a + c, b + d)
        (_, _) -> IBot
    -- Times e1 e2 -> case (evalI e1 st, evalI e2 st) of
    --   (Pair (a, b), Pair (c, d)) -> Pair (min ( a c) (a * d) (b * c) (b * d),  max(a * c) (a * d) (b * c) (b * d))
    --   (_, _) -> IBot
    Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
    Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional
    Eq _ _ -> error "Encountered eq" -- this should never happen, ...

evalCondI :: Expr -> HashMap String Interval -> BBool
evalCondI e st = case e of
    Leq e1 e2 -> case (evalI e1 st, evalI e2 st) of
        (Pair (a, b), Pair (c, d)) -> if b <= c then BTrue else if a > d then BFalse else BTop
        (IBot, _) -> BBot
        (_, IBot) -> BBot
        (_, ITop) -> BTop
        (ITop, _) -> BTop
    Gte e1 e2 -> case (evalI e1 st, evalI e2 st) of
        (Pair (a, b), Pair (c, d)) -> if a >= d then BTrue else if b < c then BFalse else BTop
        (IBot, _) -> BBot
        (_, IBot) -> BBot
        (_, ITop) -> BTop
        (ITop, _) -> BTop
    Eq e1 e2 -> case (evalI e1 st, evalI e2 st) of
        (Pair (a, b), Pair (c, d)) -> if a == c && b == d && a == b && c == d then BTrue else if b < c || d < a then BFalse else BTop
        (IBot, _) -> BBot
        (_, IBot) -> BBot
        (_, ITop) -> BTop
        (ITop, _) -> BTop
    _ -> error "Unexpected case"

refineTI :: Expr -> HashMap String Interval -> HashMap String Interval
refineTI e st = case e of
    Leq e1 e2 -> case (e1, evalI e2 st) of
        -- Only handle the trivial case, it's enough for our demo.
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> HashMap.fromList [(id, Pair (min a c, min b d))]
            _ -> HashMap.fromList [(id, IBot)]
        (_, _) -> st -- error "Unexpected conditional"
    Gte e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> HashMap.union (HashMap.fromList [(id, Pair (max a c, max b d))]) st
            _ -> HashMap.union (HashMap.fromList [(id, IBot)]) st
        (_, _) -> st -- error "Unexpected conditional"
    Eq e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> HashMap.union (HashMap.fromList [(id, Pair (c, d))]) st
        (_, _) -> st -- error "Unexpected conditional"
    _ -> error "Unexpected case"

refineFI :: Expr -> HashMap String Interval -> HashMap String Interval
refineFI e st = case e of
    Leq e1 e2 -> case (e1, evalI e2 st) of
        -- Only handle the trivial case, it's enough for our demo.
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> HashMap.union (HashMap.fromList [(id, Pair (d + 1, max (d + 1) b))]) st
            _ -> HashMap.union (HashMap.fromList [(id, IBot)]) st
        (_, _) -> st -- error "Unexpected conditional"
    Gte e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> HashMap.union (HashMap.fromList [(id, Pair (min (c - 1) a, c - 1))]) st
            _ -> HashMap.union (HashMap.fromList [(id, IBot)]) st
        (_, _) -> st -- error "Unexpected conditional"
    Eq e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> HashMap.union (HashMap.fromList [(id, Pair (c, d))]) st
        (_, _) -> st -- error "Unexpected conditional"
    _ -> error "Unexpected case"

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
    deriving (Eq, Show, Generic)

instance NFData Expr where
    rnf (Id x) = rnf x
    rnf (Num x) = rnf x
    rnf (Plus e e') = rnf (e, e')
    rnf (Leq e e') = rnf (e, e')
    rnf (Gte e e') = rnf (e, e')
    rnf (Eq e e') = rnf (e, e')
    rnf InputE = ()

instance Hashable Expr

type Label = Natural

instance PartialOrd Expr where
  leq = (==)


mainTest :: [Fact]
mainTest = [
  -- V = 1
  Var 0 "V", Seq 0 1,
  Assign 2 "V" (Num 1), Seq 2 4,
  -- while V <= 10
  Branch 4 (Leq (Id "V") (Num 9)) 5 6,
  Assign 5 "V" (Plus (Id "V") (Num 2)), Seq 5 4,
  -- if V == 11 then:
  Branch 6 (Eq (Id "V") (Num 11)) 7 101,
  Assign 7 "V" (Num 0), Seq 7 101,
  -- end
  Var 101 "END"
  ]

data Parity = IsEven | IsOdd | ParityTop | ParityBot
  deriving (Eq, Show, Generic)

instance NFData Parity where
  rnf IsEven = ()
  rnf IsOdd = ()
  rnf ParityTop = ()
  rnf ParityBot = ()

instance PartialOrd Parity where
  leq ParityBot _ = True
  leq _ ParityTop = True
  leq x y = x == y

instance Hashable Parity where
  hash ParityBot = 0
  hash IsOdd = 1
  hash IsEven = 2
  hash ParityTop = 3

joinSign :: Parity -> Parity -> Parity
joinSign ParityTop _ = ParityTop
joinSign _ ParityTop = ParityTop
joinSign ParityBot s = s
joinSign s ParityBot = s
joinSign s1 s2 = if s1 == s2 then s1 else ParityTop 

meetSign :: Parity -> Parity -> Parity
meetSign ParityBot _ = ParityBot
meetSign _ ParityBot = ParityBot
meetSign x ParityTop = x
meetSign ParityTop x = x
meetSign x y = if x == y then x else ParityBot

joinP :: HashMap String Parity -> HashMap String Parity -> HashMap String Parity
joinP = HashMap.unionWith joinSign

insertP :: String -> Parity -> HashMap String Parity -> HashMap String Parity
insertP = HashMap.insertWith joinSign

evalP :: Expr -> HashMap String Parity -> Parity
evalP e st = case e of
  Num n -> if even n then IsEven else IsOdd
  Id x -> fromMaybe ParityBot (HashMap.lookup x st)
  InputE -> ParityTop
  Plus e1 e2 -> case (evalP e1 st, evalP e2 st) of
    (s1, s2) | s1 == s2 -> IsEven
             | otherwise -> IsOdd 
  Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
  Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional
  Eq _ _ -> error "Encountered eq" -- this should never happen, ...


refineTP :: Expr -> HashMap String Parity -> HashMap String Parity
refineTP e st = case e of
  Leq e1 e2 -> st
  Gte e1 e2 -> st
  Eq e1 e2 -> case (e1, evalP e2 st) of
    (Id id, evenness) -> insertP id evenness st
    (_, _) -> st -- error "Unexpected conditional"    
  _ -> error "Unexpected case"

refineFP :: Expr -> HashMap String Parity -> HashMap String Parity
refineFP e st = case e of
  Leq e1 e2 -> case (e1, evalP e2 st) of
    (Id id, evenness) -> insertP id ParityBot st
    (_, _) -> st -- error "Unexpected conditional"    
  Gte e1 e2 -> case (e1, evalP e2 st) of
    (Id id, evenness) -> insertP id ParityBot st
    (_, _) -> st -- error "Unexpected conditional"    
  Eq e1 e2 -> case (e1, evalP e2 st) of
    (Id id, evenness) -> insertP id ParityBot st
    (_, _) -> st -- error "Unexpected conditional"    
  _ -> error "Unexpected case"

statePLeq :: HashMap String Parity -> HashMap String Parity -> Bool
statePLeq = leq

meetP :: HashMap String Parity -> HashMap String Parity -> HashMap String Parity
meetP x y = HashMap.intersectionWith meetSign x y

statePMlbs :: HashMap String Parity -> HashMap String Parity -> [HashMap String Parity]
statePMlbs x y = [HashMap.intersectionWith meetSign x y]

reduceInterval :: HashMap String Parity -> HashMap String Interval -> HashMap String Interval
reduceInterval st st' = HashMap.mapWithKey enrich st' where
    enrich :: String -> Interval -> Interval
    enrich k interval = case st HashMap.!? k of
      Nothing -> IBot
      Just evenness -> case (evenness, interval) of
         (IsEven, Pair (lo, hi)) -> Pair (if odd lo then lo + 1 else lo, if odd hi then hi - 1 else hi)
         (IsOdd, Pair (lo, hi)) -> Pair (if even lo then lo + 1 else lo, if even hi then hi - 1 else hi)
         (ParityBot, _) -> IBot
         (_, i) -> i

instance NFData Database where
  rnf (Database a b c d e f) = rnf (a, b, c, d, e, f)

instance NFData Fact where
  rnf (Assign a b c) = rnf (a, b, c)
  rnf (Branch a b c d) = rnf (a, b, c, d)
  rnf (Seq a b) = rnf (a, b)
  rnf (StateBeforeI l m) = rnf (l, m)
  rnf (StateBeforeP l m) = rnf (l, m)
  rnf (Var l s) = rnf (l, s)
----- USER CODE END -----

----- FACTS -----
data Fact = Assign Label String Expr
          | Branch Label Expr Label Label
          | Seq Label Label
          | StateBeforeI Label (HashMap String Interval)
          | StateBeforeP Label (HashMap String Parity)
          | Var Label String
  deriving (Show, Eq)

factLeq :: Fact -> Fact -> Bool
factLeq (Assign v0 v1 v2) (Assign v'0 v'1 v'2) = (v0 == v'0) && (v1 == v'1) && (v2 == v'2)
factLeq (Branch v0 v1 v2 v3) (Branch v'0 v'1 v'2 v'3) = (v0 == v'0) && (v1 == v'1) && (v2 == v'2) && (v3 == v'3)
factLeq (Seq v0 v1) (Seq v'0 v'1) = (v0 == v'0) && (v1 == v'1)
factLeq (StateBeforeI v0 v1) (StateBeforeI v'0 v'1) = (v0 == v'0) && (stateILeq v1 v'1)
factLeq (StateBeforeP v0 v1) (StateBeforeP v'0 v'1) = (v0 == v'0) && (statePLeq v1 v'1)
factLeq (Var v0 v1) (Var v'0 v'1) = (v0 == v'0) && (v1 == v'1)
factLeq _ _ = False

maximalContour :: [Fact] -> [Fact]
maximalContour [] = []
maximalContour [x] = [x]
maximalContour (x : xs) =
  if any (factLeq x) xs 
  then maximalContour xs
  else x : maximalContour (filter (\x' -> not (factLeq x' x)) xs)

----- FACT DATABASE -----
data Database = Database
  { _factsAssign :: HashMap Label (HashMap String (HashSet Expr))
  , _factsBranch :: HashMap Label (HashMap Expr (HashMap Label (HashSet Label)))
  , _factsSeq :: HashMap Label (HashSet Label)
  , _factsStateBeforeI :: HashMap Label (HashMap String Interval)
  , _factsStateBeforeP :: HashMap Label (HashMap String Parity)
  , _factsVar :: HashMap Label (HashSet String)
  } deriving Eq

emptyDb :: Database
emptyDb = Database
  { _factsAssign = HashMap.empty
  , _factsBranch = HashMap.empty
  , _factsSeq = HashMap.empty
  , _factsStateBeforeI = HashMap.empty
  , _factsStateBeforeP = HashMap.empty
  , _factsVar = HashMap.empty
  }

infix 0 |=

(|=) :: Database -> Fact -> Bool
db |= (Assign _v0 _v1 _v2) =
  let db' = _factsAssign db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v1
        return $ _v2 `HashSet.member` step2
db |= (Branch _v0 _v1 _v2 _v3) =
  let db' = _factsBranch db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        step2 <- step1 HashMap.!? _v1
        step3 <- step2 HashMap.!? _v2
        return $ _v3 `HashSet.member` step3
db |= (Seq _v0 _v1) =
  let db' = _factsSeq db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ _v1 `HashSet.member` step1
db |= (StateBeforeI _v0 _v1) =
  let db' = _factsStateBeforeI db
   in fromMaybe False $ do
        _t0 <- db' HashMap.!? _v0
        return $ stateILeq _v1 _t0
db |= (StateBeforeP _v0 _v1) =
  let db' = _factsStateBeforeP db
   in fromMaybe False $ do
        _t0 <- db' HashMap.!? _v0
        return $ statePLeq _v1 _t0
db |= (Var _v0 _v1) =
  let db' = _factsVar db
   in fromMaybe False $ do
        step1 <- db' HashMap.!? _v0
        return $ _v1 `HashSet.member` step1

insertToDb :: Database -> Fact -> Database
insertToDb db (Assign _v0 _v1 _v2) =
  let mp = _factsAssign db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton _v2))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                (HashSet.union))
              new_fact
              mp
   in db { _factsAssign = mp' }
insertToDb db (Branch _v0 _v1 _v2 _v3) =
  let mp = _factsBranch db
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashMap.singleton _v2 (HashSet.singleton _v3)))
      mp' = HashMap.unionWith
              (HashMap.unionWith
                (HashMap.unionWith
                  (HashSet.union)))
              new_fact
              mp
   in db { _factsBranch = mp' }
insertToDb db (Seq _v0 _v1) =
  let mp = _factsSeq db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              (HashSet.union)
              new_fact
              mp
   in db { _factsSeq = mp' }
insertToDb db (StateBeforeI _v0 _v1) =
  let mp = _factsStateBeforeI db
      new_fact = HashMap.singleton _v0 (_v1)
      mp' = HashMap.unionWith
              (const)
              new_fact
              mp
   in db { _factsStateBeforeI = mp' }
insertToDb db (StateBeforeP _v0 _v1) =
  let mp = _factsStateBeforeP db
      new_fact = HashMap.singleton _v0 (_v1)
      mp' = HashMap.unionWith
              (const)
              new_fact
              mp
   in db { _factsStateBeforeP = mp' }
insertToDb db (Var _v0 _v1) =
  let mp = _factsVar db
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
      mp' = HashMap.unionWith
              (HashSet.union)
              new_fact
              mp
   in db { _factsVar = mp' }

mergeContour :: Fact -> Database -> [Fact]
mergeContour f@(Assign _ _ _) _ = [f]
mergeContour f@(Branch _ _ _ _) _ = [f]
mergeContour f@(Seq _ _) _ = [f]
mergeContour f@(StateBeforeI v0 v1) db =
  let db' = _factsStateBeforeI db
   in f : do
        t0 <- maybeToList (db' HashMap.!? v0)
        let v'1 = joinI t0 v1
        return (StateBeforeI v0 v'1)
mergeContour f@(StateBeforeP v0 v1) db =
  let db' = _factsStateBeforeP db
   in f : do
        t0 <- maybeToList (db' HashMap.!? v0)
        let v'1 = joinP t0 v1
        return (StateBeforeP v0 v'1)
mergeContour f@(Var _ _) _ = [f]

----- RULE INSTANCES -----
data RuleInstance
       = Init Fact
       | RuleReducedExchangeP Label (HashMap String Parity)
       | RuleReducedExchangeI Label (HashMap String Parity) (HashMap String Interval)
       | RuleAssignInitP Label
       | RuleBranchInitP Label
       | RuleVarInitP Label
       | RuleAssignStepP Expr Label Label (HashMap String Parity) String
       | RuleBranchTrueP Expr Label (HashMap String Parity) Label
       | RuleBranchFalseP Expr Label Label (HashMap String Parity)
       | RuleAssignInitI Label
       | RuleBranchInitI Label
       | RuleVarInitI Label
       | RuleAssignStepI Expr Label Label (HashMap String Interval) String
       | RuleBranchTrueI Expr Label (HashMap String Interval) Label
       | RuleBranchFalseI Expr Label Label (HashMap String Interval)
  deriving Show

evaluate :: RuleInstance -> Fact
evaluate (Init f) = f
evaluate (RuleReducedExchangeP __l__ __st__) = StateBeforeP __l__ __st__
evaluate (RuleReducedExchangeI __l__ __st__ __st__') = StateBeforeI __l__ (reduceInterval __st__ __st__')
evaluate (RuleAssignInitP __l__) = StateBeforeP __l__ HashMap.empty
evaluate (RuleBranchInitP __l__) = StateBeforeP __l__ HashMap.empty
evaluate (RuleVarInitP __l__) = StateBeforeP __l__ HashMap.empty
evaluate (RuleAssignStepP __e__ __l__ __l__' __st__ __x__) = StateBeforeP __l__' (HashMap.insert __x__ (evalP __e__ __st__) __st__)
evaluate (RuleBranchTrueP __e__ __l__ __st__ __t__) = StateBeforeP __t__ (refineTP __e__ __st__)
evaluate (RuleBranchFalseP __e__ __f__ __l__ __st__) = StateBeforeP __f__ (refineFP __e__ __st__)
evaluate (RuleAssignInitI __l__) = StateBeforeI __l__ HashMap.empty
evaluate (RuleBranchInitI __l__) = StateBeforeI __l__ HashMap.empty
evaluate (RuleVarInitI __l__) = StateBeforeI __l__ HashMap.empty
evaluate (RuleAssignStepI __e__ __l__ __l__' __st__ __x__) = StateBeforeI __l__' (HashMap.insert __x__ (evalI __e__ __st__) __st__)
evaluate (RuleBranchTrueI __e__ __l__ __st__ __t__) = StateBeforeI __t__ (refineTI __e__ __st__)
evaluate (RuleBranchFalseI __e__ __f__ __l__ __st__) = StateBeforeI __f__ (refineFI __e__ __st__)

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
  (RuleAssignInitP _) < (RuleReducedExchangeP _ _) = True
  (RuleBranchInitP _) < (RuleReducedExchangeP _ _) = True
  (RuleVarInitP _) < (RuleReducedExchangeP _ _) = True
  (RuleAssignInitI _) < (RuleReducedExchangeP _ _) = True
  (RuleBranchInitI _) < (RuleReducedExchangeP _ _) = True
  (RuleVarInitI _) < (RuleReducedExchangeP _ _) = True
  (RuleAssignInitP _) < (RuleReducedExchangeI _ _ _) = True
  (RuleBranchInitP _) < (RuleReducedExchangeI _ _ _) = True
  (RuleVarInitP _) < (RuleReducedExchangeI _ _ _) = True
  (RuleAssignInitI _) < (RuleReducedExchangeI _ _ _) = True
  (RuleBranchInitI _) < (RuleReducedExchangeI _ _ _) = True
  (RuleVarInitI _) < (RuleReducedExchangeI _ _ _) = True
  (RuleAssignStepI _ _ _ _ _) < (RuleAssignInitI _) = True
  (RuleBranchTrueI _ _ _ _) < (RuleAssignInitI _) = True
  (RuleBranchFalseI _ _ _ _) < (RuleAssignInitI _) = True
  (RuleAssignStepI _ _ _ _ _) < (RuleBranchInitI _) = True
  (RuleBranchTrueI _ _ _ _) < (RuleBranchInitI _) = True
  (RuleBranchFalseI _ _ _ _) < (RuleBranchInitI _) = True
  (RuleAssignStepI _ _ _ _ _) < (RuleVarInitI _) = True
  (RuleBranchTrueI _ _ _ _) < (RuleVarInitI _) = True
  (RuleBranchFalseI _ _ _ _) < (RuleVarInitI _) = True
  (RuleAssignStepP _ _ _ _ _) < (RuleAssignInitP _) = True
  (RuleBranchTrueP _ _ _ _) < (RuleAssignInitP _) = True
  (RuleBranchFalseP _ _ _ _) < (RuleAssignInitP _) = True
  (RuleAssignStepP _ _ _ _ _) < (RuleBranchInitP _) = True
  (RuleBranchTrueP _ _ _ _) < (RuleBranchInitP _) = True
  (RuleBranchFalseP _ _ _ _) < (RuleBranchInitP _) = True
  (RuleAssignStepP _ _ _ _ _) < (RuleVarInitP _) = True
  (RuleBranchTrueP _ _ _ _) < (RuleVarInitP _) = True
  (RuleBranchFalseP _ _ _ _) < (RuleVarInitP _) = True
  (RuleAssignStepI _ __l__ _ _ _) < (RuleAssignStepI _ __l__' _ _ _) = __l__ > __l__'
  (RuleBranchTrueI _ __l__ _ _) < (RuleAssignStepI _ __l__' _ _ _) = __l__ > __l__'
  (RuleBranchFalseI _ _ __l__ _) < (RuleAssignStepI _ __l__' _ _ _) = __l__ > __l__'
  (RuleAssignStepI _ __l__ _ _ _) < (RuleBranchTrueI _ __l__' _ _) = __l__ > __l__'
  (RuleBranchTrueI _ __l__ _ _) < (RuleBranchTrueI _ __l__' _ _) = __l__ > __l__'
  (RuleBranchFalseI _ _ __l__ _) < (RuleBranchTrueI _ __l__' _ _) = __l__ > __l__'
  (RuleAssignStepI _ __l__ _ _ _) < (RuleBranchFalseI _ _ __l__' _) = __l__ > __l__'
  (RuleBranchTrueI _ __l__ _ _) < (RuleBranchFalseI _ _ __l__' _) = __l__ > __l__'
  (RuleBranchFalseI _ _ __l__ _) < (RuleBranchFalseI _ _ __l__' _) = __l__ > __l__'
  (RuleAssignStepP _ __l__ _ _ _) < (RuleAssignStepP _ __l__' _ _ _) = __l__ > __l__'
  (RuleBranchTrueP _ __l__ _ _) < (RuleAssignStepP _ __l__' _ _ _) = __l__ > __l__'
  (RuleBranchFalseP _ _ __l__ _) < (RuleAssignStepP _ __l__' _ _ _) = __l__ > __l__'
  (RuleAssignStepP _ __l__ _ _ _) < (RuleBranchTrueP _ __l__' _ _) = __l__ > __l__'
  (RuleBranchTrueP _ __l__ _ _) < (RuleBranchTrueP _ __l__' _ _) = __l__ > __l__'
  (RuleBranchFalseP _ _ __l__ _) < (RuleBranchTrueP _ __l__' _ _) = __l__ > __l__'
  (RuleAssignStepP _ __l__ _ _ _) < (RuleBranchFalseP _ _ __l__' _) = __l__ > __l__'
  (RuleBranchTrueP _ __l__ _ _) < (RuleBranchFalseP _ _ __l__' _) = __l__ > __l__'
  (RuleBranchFalseP _ _ __l__ _) < (RuleBranchFalseP _ _ __l__' _) = __l__ > __l__'
  _ < _ = False

type Queue = Q.MaxQueue (RuleInstance, Phase)


----- STEP FUNCTION -----

step :: Interpretation -> Fact -> Phase -> Queue -> Queue
step i f p q = let db = selectDb i p in case p of
  Phase1 -> case f of
    Assign _t0 _t1 _t2 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        let _v2_0 = _t2
        concat [
          do
            step0 <- maybeToList (_factsSeq db HashMap.!? _v0_0)
            _v3_0 <- HashSet.toList step0
            concat [
              do
                step1 <- maybeToList (_factsStateBeforeI db HashMap.!? _v0_0)
                let _v4_0 = step1
                return (RuleAssignStepI _v2_0 _v0_0 _v3_0 _v4_0 _v1_0, Phase1),
              do
                step1 <- maybeToList (_factsStateBeforeP db HashMap.!? _v0_0)
                let _v4_0 = step1
                return (RuleAssignStepP _v2_0 _v0_0 _v3_0 _v4_0 _v1_0, Phase1)
              ],
          do
            return (RuleAssignInitP _v0_0, Phase1),
          do
            return (RuleAssignInitI _v0_0, Phase1)
          ]
    Branch _t0 _t1 _t2 _t3 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        let _v2_0 = _t2
        let _v3_0 = _t3
        concat [
          do
            step0 <- maybeToList (_factsStateBeforeI db HashMap.!? _v0_0)
            let _v4_0 = step0
            concat [
              do
                guard (((leq BTrue) ((evalCondI _v1_0) _v4_0)))
                return (RuleBranchTrueI _v1_0 _v0_0 _v4_0 _v2_0, Phase1),
              do
                guard (((leq BFalse) ((evalCondI _v1_0) _v4_0)))
                return (RuleBranchFalseI _v1_0 _v3_0 _v0_0 _v4_0, Phase1)
              ],
          do
            step0 <- maybeToList (_factsStateBeforeP db HashMap.!? _v0_0)
            let _v4_0 = step0
            concat [
              do
                return (RuleBranchTrueP _v1_0 _v0_0 _v4_0 _v2_0, Phase1),
              do
                return (RuleBranchFalseP _v1_0 _v3_0 _v0_0 _v4_0, Phase1)
              ],
          do
            return (RuleBranchInitP _v0_0, Phase1),
          do
            return (RuleBranchInitI _v0_0, Phase1)
          ]
    Seq _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        step0 <- maybeToList (_factsAssign db HashMap.!? _v0_0)
        (_v2_0, step1) <- HashMap.toList step0
        _v3_0 <- HashSet.toList step1
        concat [
          do
            step2 <- maybeToList (_factsStateBeforeI db HashMap.!? _v0_0)
            let _v4_0 = step2
            return (RuleAssignStepI _v3_0 _v0_0 _v1_0 _v4_0 _v2_0, Phase1),
          do
            step2 <- maybeToList (_factsStateBeforeP db HashMap.!? _v0_0)
            let _v4_0 = step2
            return (RuleAssignStepP _v3_0 _v0_0 _v1_0 _v4_0 _v2_0, Phase1)
          ]
    StateBeforeI _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        concat [
          do
            step0 <- maybeToList (_factsAssign db HashMap.!? _v0_0)
            (_v2_0, step1) <- HashMap.toList step0
            _v3_0 <- HashSet.toList step1
            step2 <- maybeToList (_factsSeq db HashMap.!? _v0_0)
            _v4_0 <- HashSet.toList step2
            return (RuleAssignStepI _v3_0 _v0_0 _v4_0 _v1_0 _v2_0, Phase1),
          do
            step0 <- maybeToList (_factsBranch db HashMap.!? _v0_0)
            (_v2_0, step1) <- HashMap.toList step0
            (_v3_0, step2) <- HashMap.toList step1
            _v4_0 <- HashSet.toList step2
            concat [
              do
                guard (((leq BTrue) ((evalCondI _v2_0) _v1_0)))
                return (RuleBranchTrueI _v2_0 _v0_0 _v1_0 _v3_0, Phase1),
              do
                guard (((leq BFalse) ((evalCondI _v2_0) _v1_0)))
                return (RuleBranchFalseI _v2_0 _v4_0 _v0_0 _v1_0, Phase1)
              ]
          ]
    StateBeforeP _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        concat [
          do
            step0 <- maybeToList (_factsAssign db HashMap.!? _v0_0)
            (_v2_0, step1) <- HashMap.toList step0
            _v3_0 <- HashSet.toList step1
            step2 <- maybeToList (_factsSeq db HashMap.!? _v0_0)
            _v4_0 <- HashSet.toList step2
            return (RuleAssignStepP _v3_0 _v0_0 _v4_0 _v1_0 _v2_0, Phase1),
          do
            step0 <- maybeToList (_factsBranch db HashMap.!? _v0_0)
            (_v2_0, step1) <- HashMap.toList step0
            (_v3_0, step2) <- HashMap.toList step1
            _v4_0 <- HashSet.toList step2
            concat [
              do
                return (RuleBranchTrueP _v2_0 _v0_0 _v1_0 _v3_0, Phase1),
              do
                return (RuleBranchFalseP _v2_0 _v4_0 _v0_0 _v1_0, Phase1)
              ]
          ]
    Var _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        concat [
          do
            return (RuleVarInitP _v0_0, Phase1),
          do
            return (RuleVarInitI _v0_0, Phase1)
          ]
  Phase2 -> case f of
    StateBeforeI _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        step0 <- maybeToList (_factsStateBeforeP db HashMap.!? _v0_0)
        let _v2_0 = step0
        return (RuleReducedExchangeI _v0_0 _v2_0 _v1_0, Phase2)
    StateBeforeP _t0 _t1 -> Q.union q $ Q.fromList $ 
      do
        let _v0_0 = _t0
        let _v1_0 = _t1
        concat [
          do
            step0 <- maybeToList (_factsStateBeforeI db HashMap.!? _v0_0)
            let _v2_0 = step0
            return (RuleReducedExchangeI _v0_0 _v1_0 _v2_0, Phase2),
          do
            return (RuleReducedExchangeP _v0_0 _v1_0, Phase2)
          ]
    _ -> q

stepAll :: Interpretation -> [Fact] -> Phase -> Queue -> Queue
stepAll i xs p q = foldl' (\q' f -> step i f p q') q xs


----- SOLVER -----

loop :: Queue -> Interpretation -> Interpretation
loop q i
  | Just (p, q') <- Q.maxView q =
    let (f, phase) = evaluatePhased p
        db = selectDb i phase
     in if db |= f
        then loop q' i
        else let c = mergeContour f db
                 new_facts = filter (not . (db |=)) (maximalContour c)
                 new_db = foldl' insertToDb db new_facts
                 new_int = replaceDb i new_db phase
              in loop (stepAll new_int new_facts phase q') new_int
  | otherwise = i

solve :: [Fact] -> Interpretation
solve = reSolve emptyInterpretation


reSolve :: Interpretation -> [Fact] -> Interpretation
reSolve i f =
  let q = Q.fromList $ concat [
            (,Phase2) . Init <$> f
          ]
   in loop q i


----- QUERIES -----

stateI :: Label -> Interpretation -> Phase -> [Fact]
stateI _v0_0 i p = do
  let db = selectDb i p
  step0 <- maybeToList (_factsStateBeforeI db HashMap.!? _v0_0)
  let _v1_0 = step0
  return $ StateBeforeI _v0_0 _v1_0

stateIs :: Interpretation -> Phase -> [Fact]
stateIs i p = do
  let db = selectDb i p
  (_v0_0, step0) <- HashMap.toList (_factsStateBeforeI db)
  let _v1_0 = step0
  return $ StateBeforeI _v0_0 _v1_0

vars :: Label -> Interpretation -> Phase -> [Fact]
vars _v0_0 i p = do
  let db = selectDb i p
  step0 <- maybeToList (_factsVar db HashMap.!? _v0_0)
  _v1_0 <- HashSet.toList (step0)
  return $ Var _v0_0 _v1_0

assigns :: Label -> Interpretation -> Phase -> [Fact]
assigns _v0_0 i p = do
  let db = selectDb i p
  step0 <- maybeToList (_factsAssign db HashMap.!? _v0_0)
  (_v1_0, step1) <- HashMap.toList (step0)
  _v2_0 <- HashSet.toList (step1)
  return $ Assign _v0_0 _v1_0 _v2_0

branches :: Expr -> Interpretation -> Phase -> [Fact]
branches _v1_0 i p = do
  let db = selectDb i p
  (_v0_0, step0) <- HashMap.toList (_factsBranch db)
  step1 <- maybeToList (step0 HashMap.!? _v1_0)
  (_v2_0, step2) <- HashMap.toList (step1)
  _v3_0 <- HashSet.toList (step2)
  return $ Branch _v0_0 _v1_0 _v2_0 _v3_0

seqs :: Label -> Interpretation -> Phase -> [Fact]
seqs _v0_0 i p = do
  let db = selectDb i p
  step0 <- maybeToList (_factsSeq db HashMap.!? _v0_0)
  _v1_0 <- HashSet.toList (step0)
  return $ Seq _v0_0 _v1_0

stateP :: Label -> Interpretation -> Phase -> [Fact]
stateP _v0_0 i p = do
  let db = selectDb i p
  step0 <- maybeToList (_factsStateBeforeP db HashMap.!? _v0_0)
  let _v1_0 = step0
  return $ StateBeforeP _v0_0 _v1_0

----- MULTI-PHASE FIXEN PROGRAM DEFINITIONS -----
type Interpretation = (Database, Database)

emptyInterpretation :: Interpretation
emptyInterpretation = (emptyDb, emptyDb)

data Phase = Phase1
           | Phase2
 deriving (Eq, Show, Ord)

nextPhase :: Phase -> Phase
nextPhase Phase1 = Phase2
nextPhase Phase2 = Phase1

selectDb :: Interpretation -> Phase -> Database
selectDb (db, _) Phase1 = db
selectDb (_, db) Phase2 = db

(||=) :: Interpretation -> Fact -> Phase -> Bool
(i ||= f) p = selectDb i p |= f

infix 1 ||=

replaceDb :: Interpretation -> Database -> Phase -> Interpretation
replaceDb (_, db2) db' Phase1 = (db', db2)
replaceDb (db1, _) db' Phase2 = (db1, db')

insertToInterpretation :: Interpretation -> Fact -> Phase -> Interpretation
insertToInterpretation i f p = do
  let db = selectDb i p
      db' = insertToDb db f
   in replaceDb i db' p

evaluatePhased :: (RuleInstance, Phase) -> (Fact, Phase)
evaluatePhased (r, p) = (evaluate r, nextPhase p)