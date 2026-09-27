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
import Data.List
import ReducedProduct.Common


----- USER CODE START -----
mainTest :: [Fact]
mainTest = [
  -- V = 1
  Var 0 "V", Seq 0 2,
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

factorialTest :: [Fact]
factorialTest = [
  Var 0 "Y", Seq 0 1,
  Var 1 "Z", Seq 1 2,
  Assign 2 "Y" (Id "X"), Seq 2 3,
  Assign 3 "Z" (Num 1), Seq 3 4,
  Branch 4 (Gte (Id "Y") (Num 1)) 5 7,
  Assign 5 "Z" (Times (Id "Z") (Id "Y")), Seq 5 6,
  Assign 6 "Y" (Plus (Id "Y") (Num (-1))), Seq 6 4,
  Assign 7 "Y" (Num 0), Seq 7 100,
  Var 100 "END"
  ]

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
            | Var Label String deriving (Show, Eq)

factLeq :: Fact -> Fact -> Bool
factLeq (Assign v0 v1 v2) (Assign v'0 v'1 v'2) = ((==) v0 v'0 && ((==) v1 v'1 && (==) v2 v'2))
factLeq (Branch v0 v1 v2 v3) (Branch v'0 v'1 v'2 v'3) =
    ((==) v0 v'0 && ((==) v1 v'1 && ((==) v2 v'2 && (==) v3 v'3)))
factLeq (Seq v0 v1) (Seq v'0 v'1) = ((==) v0 v'0 && (==) v1 v'1)
factLeq (StateBeforeI v0 v1) (StateBeforeI v'0 v'1) = ((==) v0 v'0 && stateILeq v1 v'1)
factLeq (StateBeforeP v0 v1) (StateBeforeP v'0 v'1) = ((==) v0 v'0 && statePLeq v1 v'1)
factLeq (Var v0 v1) (Var v'0 v'1) = ((==) v0 v'0 && (==) v1 v'1)
factLeq _ _ = False

maximalContour :: [Fact] -> [Fact]
maximalContour [] = []
maximalContour [x] = [x]
maximalContour (x : xs) =
  if any (factLeq x) xs
  then maximalContour xs
  else x : maximalContour (filter (\x' -> not (factLeq x' x)) xs)

data Database = Database {_factsAssign :: HashMap Label (HashMap String (HashSet Expr))
                , _factsBranch :: HashMap Label (HashMap Expr (HashMap Label (HashSet Label)))
                , _factsSeq :: HashMap Label (HashSet Label)
                , _factsStateBeforeI :: HashMap Label (HashMap String Interval)
                , _factsStateBeforeP :: HashMap Label (HashMap String Parity)
                , _factsVar :: HashMap Label (HashSet String)} deriving (Eq)

emptyDb :: Database
emptyDb =
    Database {_factsAssign = HashMap.empty
    , _factsBranch = HashMap.empty
    , _factsSeq = HashMap.empty
    , _factsStateBeforeI = HashMap.empty
    , _factsStateBeforeP = HashMap.empty
    , _factsVar = HashMap.empty}

infix 0 |=
(|=) :: Database -> Fact -> Bool
(|=) db (Assign _v0 _v1 _v2) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsAssign db);
      return (fromMaybe False (do {
        step1 <- HashMap.lookup _v1 step0;
        return (HashSet.member _v2 step1)
      }))
    })
(|=) db (Branch _v0 _v1 _v2 _v3) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsBranch db);
      return (fromMaybe False (do {
        step1 <- HashMap.lookup _v1 step0;
        return (fromMaybe False (do {
          step2 <- HashMap.lookup _v2 step1;
          return (HashSet.member _v3 step2)
        }))
      }))
    })
(|=) db (Seq _v0 _v1) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsSeq db);
      return (HashSet.member _v1 step0)
    })
(|=) db (StateBeforeI _v0 _v1) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsStateBeforeI db);
      return (let { _t0 = step0 } in stateILeq _v1 _t0)
    })
(|=) db (StateBeforeP _v0 _v1) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsStateBeforeP db);
      return (let { _t0 = step0 } in statePLeq _v1 _t0)
    })
(|=) db (Var _v0 _v1) =
    fromMaybe False (do {
      step0 <- HashMap.lookup _v0 (_factsVar db);
      return (HashSet.member _v1 step0)
    })

insertToDb :: Database -> Fact -> Database
insertToDb db (Assign _v0 _v1 _v2) =
    db {_factsAssign = let {
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashSet.singleton _v2))
    } in HashMap.unionWith (HashMap.unionWith HashSet.union) new_fact (_factsAssign db)}
insertToDb db (Branch _v0 _v1 _v2 _v3) =
    db {_factsBranch = let {
      new_fact = HashMap.singleton _v0 (HashMap.singleton _v1 (HashMap.singleton _v2 (HashSet.singleton _v3)))
    } in HashMap.unionWith (HashMap.unionWith (HashMap.unionWith HashSet.union)) new_fact (_factsBranch db)}
insertToDb db (Seq _v0 _v1) =
    db {_factsSeq = let {
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
    } in HashMap.unionWith HashSet.union new_fact (_factsSeq db)}
insertToDb db (StateBeforeI _v0 _v1) =
    db {_factsStateBeforeI = let {
      new_fact = HashMap.singleton _v0 _v1
    } in HashMap.unionWith const new_fact (_factsStateBeforeI db)}
insertToDb db (StateBeforeP _v0 _v1) =
    db {_factsStateBeforeP = let {
      new_fact = HashMap.singleton _v0 _v1
    } in HashMap.unionWith const new_fact (_factsStateBeforeP db)}
insertToDb db (Var _v0 _v1) =
    db {_factsVar = let {
      new_fact = HashMap.singleton _v0 (HashSet.singleton _v1)
    } in HashMap.unionWith HashSet.union new_fact (_factsVar db)}

mergeContour :: Fact -> Database -> [Fact]
mergeContour f@(Assign _ _ _) _ = [f]
mergeContour f@(Branch _ _ _ _) _ = [f]
mergeContour f@(Seq _ _) _ = [f]
mergeContour f@(StateBeforeI _v0 _v1) db =
    (f : (do {
      step0 <- maybeToList (HashMap.lookup _v0 (_factsStateBeforeI db));
      let { _t0 = step0 };
      let { joined1 = joinI _t0 _v1 };
      return (StateBeforeI _v0 joined1)
    }))
mergeContour f@(StateBeforeP _v0 _v1) db =
    (f : (do {
      step0 <- maybeToList (HashMap.lookup _v0 (_factsStateBeforeP db));
      let { _t0 = step0 };
      let { joined1 = joinP _t0 _v1 };
      return (StateBeforeP _v0 joined1)
    }))
mergeContour f@(Var _ _) _ = [f]

data RuleInstance = Init Fact
                    | RuleReducedExchange Label (HashMap String Parity) (HashMap String Interval)
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
                    | RuleBranchFalseI Expr Label Label (HashMap String Interval) deriving (Show)

evaluate :: RuleInstance -> [Fact]
evaluate (Init f) = [f]
evaluate (RuleReducedExchange __l__ __st__ __st__') =
    [StateBeforeP __l__ __st__, StateBeforeI __l__ (reduceInterval __st__ __st__')]
evaluate (RuleAssignInitP __l__) = [StateBeforeP __l__ HashMap.empty]
evaluate (RuleBranchInitP __l__) = [StateBeforeP __l__ HashMap.empty]
evaluate (RuleVarInitP __l__) = [StateBeforeP __l__ HashMap.empty]
evaluate (RuleAssignStepP __e__ __l__ __l__' __st__ __x__) =
    [StateBeforeP __l__' (HashMap.insert __x__ (evalP __e__ __st__) __st__)]
evaluate (RuleBranchTrueP __e__ __l__ __st__ __t__) = [StateBeforeP __t__ (refineTP __e__ __st__)]
evaluate (RuleBranchFalseP __e__ __f__ __l__ __st__) = [StateBeforeP __f__ (refineFP __e__ __st__)]
evaluate (RuleAssignInitI __l__) = [StateBeforeI __l__ HashMap.empty]
evaluate (RuleBranchInitI __l__) = [StateBeforeI __l__ HashMap.empty]
evaluate (RuleVarInitI __l__) = [StateBeforeI __l__ HashMap.empty]
evaluate (RuleAssignStepI __e__ __l__ __l__' __st__ __x__) =
    [StateBeforeI __l__' (HashMap.insert __x__ (evalI __e__ __st__) __st__)]
evaluate (RuleBranchTrueI __e__ __l__ __st__ __t__) = [StateBeforeI __t__ (refineTI __e__ __st__)]
evaluate (RuleBranchFalseI __e__ __f__ __l__ __st__) = [StateBeforeI __f__ (refineFI __e__ __st__)]

instance Eq RuleInstance where
  f == f' = not (f < f' || f' < f)

instance Ord RuleInstance where {
    (<=) i i' = not ((<) i' i);
    (<) (Init _) (Init _) = False;
    (<) _ (Init _) = True;
    (<) (RuleReducedExchange __l__ _ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleReducedExchange __l__ _ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleReducedExchange __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleAssignStepI _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchTrueI _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchFalseI _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleAssignStepP _ __l__' _ _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchTrueP _ __l__' _ _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchFalseP _ _ __l__' _) = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleAssignInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleAssignInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleVarInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleVarInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchInitP __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepI _ __l__ _ _ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueI _ __l__ _ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseI _ _ __l__ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignStepP _ __l__ _ _ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchTrueP _ __l__ _ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchFalseP _ _ __l__ _) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitI __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitI __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitI __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleAssignInitP __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleBranchInitP __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) (RuleVarInitP __l__) (RuleBranchInitI __l__') = (>) __l__ __l__';
    (<) _ _ = False
  }

type Queue = Q.MaxQueue Kont
newtype Kont = Kont (RuleInstance, Phase)
instance Eq Kont where {
    (==) (Kont (x, _)) (Kont (y, _)) = (==) x y;
    (/=) (Kont (x, _)) (Kont (y, _)) = (/=) x y
  }
instance Ord Kont where {
    (<) (Kont (x, _)) (Kont (y, _)) = (<) x y;
    (<=) (Kont (x, _)) (Kont (y, _)) = (<=) x y;
    (>) (Kont (x, _)) (Kont (y, _)) = (>) x y;
    (>=) (Kont (x, _)) (Kont (y, _)) = (>=) x y;
    compare (Kont (x, _)) (Kont (y, _)) = compare x y
  }



----- STEP FUNCTION -----

step :: Interpretation -> Fact -> Phase -> Queue -> Queue
step i fact p q =
    let { db = selectDb i p } in case p of {
      Phase0 -> case fact of {
        Assign _t0 _t1 _t2 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          let { _v2_0 = _t2 };
          concat [ do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsSeq db));
            _v3_0 <- HashSet.toList step0;
            concat [ do {
              step1 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeI db));
              let { _v4_0 = step1 };
              let { rule_instance = RuleAssignStepI _v2_0 _v0_0 _v3_0 _v4_0 _v1_0 };
              return (Kont (rule_instance, Phase0))
            }
            , do {
              step1 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeP db));
              let { _v4_0 = step1 };
              let { rule_instance = RuleAssignStepP _v2_0 _v0_0 _v3_0 _v4_0 _v1_0 };
              return (Kont (rule_instance, Phase0))
            } ]
          }
          , do {
            let { rule_instance = RuleAssignInitP _v0_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            let { rule_instance = RuleAssignInitI _v0_0 };
            return (Kont (rule_instance, Phase0))
          } ]
        }));
        Branch _t0 _t1 _t2 _t3 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          let { _v2_0 = _t2 };
          let { _v3_0 = _t3 };
          concat [ do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeI db));
            let { _v4_0 = step0 };
            concat [ do {
              guard (leq BTrue (evalCondI _v1_0 _v4_0));
              let { rule_instance = RuleBranchTrueI _v1_0 _v0_0 _v4_0 _v2_0 };
              return (Kont (rule_instance, Phase0))
            }
            , do {
              guard (leq BFalse (evalCondI _v1_0 _v4_0));
              let { rule_instance = RuleBranchFalseI _v1_0 _v3_0 _v0_0 _v4_0 };
              return (Kont (rule_instance, Phase0))
            } ]
          }
          , do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeP db));
            let { _v4_0 = step0 };
            concat [ do {
              let { rule_instance = RuleBranchTrueP _v1_0 _v0_0 _v4_0 _v2_0 };
              return (Kont (rule_instance, Phase0))
            }
            , do {
              let { rule_instance = RuleBranchFalseP _v1_0 _v3_0 _v0_0 _v4_0 };
              return (Kont (rule_instance, Phase0))
            } ]
          }
          , do {
            let { rule_instance = RuleBranchInitP _v0_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            let { rule_instance = RuleBranchInitI _v0_0 };
            return (Kont (rule_instance, Phase0))
          } ]
        }));
        Seq _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          step0 <- maybeToList (HashMap.lookup _v0_0 (_factsAssign db));
          (_v2_0, step1) <- HashMap.toList step0;
          _v3_0 <- HashSet.toList step1;
          concat [ do {
            step2 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeI db));
            let { _v4_0 = step2 };
            let { rule_instance = RuleAssignStepI _v3_0 _v0_0 _v1_0 _v4_0 _v2_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            step2 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeP db));
            let { _v4_0 = step2 };
            let { rule_instance = RuleAssignStepP _v3_0 _v0_0 _v1_0 _v4_0 _v2_0 };
            return (Kont (rule_instance, Phase0))
          } ]
        }));
        StateBeforeI _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          concat [ do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsAssign db));
            (_v2_0, step1) <- HashMap.toList step0;
            _v3_0 <- HashSet.toList step1;
            step2 <- maybeToList (HashMap.lookup _v0_0 (_factsSeq db));
            _v4_0 <- HashSet.toList step2;
            let { rule_instance = RuleAssignStepI _v3_0 _v0_0 _v4_0 _v1_0 _v2_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsBranch db));
            (_v2_0, step1) <- HashMap.toList step0;
            (_v3_0, step2) <- HashMap.toList step1;
            _v4_0 <- HashSet.toList step2;
            concat [ do {
              guard (leq BTrue (evalCondI _v2_0 _v1_0));
              let { rule_instance = RuleBranchTrueI _v2_0 _v0_0 _v1_0 _v3_0 };
              return (Kont (rule_instance, Phase0))
            }
            , do {
              guard (leq BFalse (evalCondI _v2_0 _v1_0));
              let { rule_instance = RuleBranchFalseI _v2_0 _v4_0 _v0_0 _v1_0 };
              return (Kont (rule_instance, Phase0))
            } ]
          } ]
        }));
        StateBeforeP _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          concat [ do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsAssign db));
            (_v2_0, step1) <- HashMap.toList step0;
            _v3_0 <- HashSet.toList step1;
            step2 <- maybeToList (HashMap.lookup _v0_0 (_factsSeq db));
            _v4_0 <- HashSet.toList step2;
            let { rule_instance = RuleAssignStepP _v3_0 _v0_0 _v4_0 _v1_0 _v2_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            step0 <- maybeToList (HashMap.lookup _v0_0 (_factsBranch db));
            (_v2_0, step1) <- HashMap.toList step0;
            (_v3_0, step2) <- HashMap.toList step1;
            _v4_0 <- HashSet.toList step2;
            concat [ do {
              let { rule_instance = RuleBranchTrueP _v2_0 _v0_0 _v1_0 _v3_0 };
              return (Kont (rule_instance, Phase0))
            }
            , do {
              let { rule_instance = RuleBranchFalseP _v2_0 _v4_0 _v0_0 _v1_0 };
              return (Kont (rule_instance, Phase0))
            } ]
          } ]
        }));
        Var _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          concat [ do {
            let { rule_instance = RuleVarInitP _v0_0 };
            return (Kont (rule_instance, Phase0))
          }
          , do {
            let { rule_instance = RuleVarInitI _v0_0 };
            return (Kont (rule_instance, Phase0))
          } ]
        }))
      };
      Phase1 -> case fact of {
        StateBeforeI _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeP db));
          let { _v2_0 = step0 };
          let { rule_instance = RuleReducedExchange _v0_0 _v2_0 _v1_0 };
          return (Kont (rule_instance, Phase1))
        }));
        StateBeforeP _t0 _t1 -> Q.union q (Q.fromList (do {
          let { _v0_0 = _t0 };
          let { _v1_0 = _t1 };
          step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeI db));
          let { _v2_0 = step0 };
          let { rule_instance = RuleReducedExchange _v0_0 _v1_0 _v2_0 };
          return (Kont (rule_instance, Phase1))
        }));
        _ -> q
      }
    }

stepAll :: Interpretation -> [Fact] -> Phase -> Queue -> Queue
stepAll i xs p q = foldl' (\q' f -> step i f p q') q xs

----- SOLVER -----

loop :: Queue -> Interpretation -> Interpretation
loop q i =
    case Q.maxView q of {
      Just (p, q') -> let {
        (fs, target_phase) = evaluatePhased p
      } in case foldl' (_fixenProcessConclusion target_phase) (q', i) fs of {
        (next_q, next_state) -> loop next_q next_state
      };
      Nothing -> i
    }
_fixenProcessConclusion :: Phase -> (Queue, Interpretation) -> Fact -> (Queue, Interpretation)
_fixenProcessConclusion target_phase (q', i) f =
    let { db = selectDb i target_phase } in if (|=) db f then (q', i) else let {
      c = mergeContour f db;
      new_facts = filter (\candidate -> not ((|=) db candidate)) (maximalContour c);
      new_db = foldl' insertToDb db new_facts;
      new_int = replaceDb i new_db target_phase
    } in let { next_q = stepAll new_int new_facts target_phase q' } in seq next_q (next_q, new_int)
solve :: [Fact] -> Interpretation
solve = reSolve emptyInterpretation

reSolve :: Interpretation -> [Fact] -> Interpretation
reSolve i f =
    let { q = Q.fromList (concat [map (\fact -> Kont (Init fact, Phase1)) f]) } in loop q i

----- QUERIES -----

stateI :: Label -> Interpretation -> Phase -> [Fact]
stateI _v0_0 i p =
    do {
      let { db = selectDb i p };
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeI db));
      let { _v1_0 = step0 };
      return (StateBeforeI _v0_0 _v1_0)
    }

stateIs :: Interpretation -> Phase -> [Fact]
stateIs i p =
    do {
      let { db = selectDb i p };
      (_v0_0, step0) <- HashMap.toList (_factsStateBeforeI db);
      let { _v1_0 = step0 };
      return (StateBeforeI _v0_0 _v1_0)
    }

vars :: Label -> Interpretation -> Phase -> [Fact]
vars _v0_0 i p =
    do {
      let { db = selectDb i p };
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsVar db));
      _v1_0 <- HashSet.toList step0;
      return (Var _v0_0 _v1_0)
    }

assigns :: Label -> Interpretation -> Phase -> [Fact]
assigns _v0_0 i p =
    do {
      let { db = selectDb i p };
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsAssign db));
      (_v1_0, step1) <- HashMap.toList step0;
      _v2_0 <- HashSet.toList step1;
      return (Assign _v0_0 _v1_0 _v2_0)
    }

branches :: Expr -> Interpretation -> Phase -> [Fact]
branches _v1_0 i p =
    do {
      let { db = selectDb i p };
      (_v0_0, step0) <- HashMap.toList (_factsBranch db);
      step1 <- maybeToList (HashMap.lookup _v1_0 step0);
      (_v2_0, step2) <- HashMap.toList step1;
      _v3_0 <- HashSet.toList step2;
      return (Branch _v0_0 _v1_0 _v2_0 _v3_0)
    }

seqs :: Label -> Interpretation -> Phase -> [Fact]
seqs _v0_0 i p =
    do {
      let { db = selectDb i p };
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsSeq db));
      _v1_0 <- HashSet.toList step0;
      return (Seq _v0_0 _v1_0)
    }

stateP :: Label -> Interpretation -> Phase -> [Fact]
stateP _v0_0 i p =
    do {
      let { db = selectDb i p };
      step0 <- maybeToList (HashMap.lookup _v0_0 (_factsStateBeforeP db));
      let { _v1_0 = step0 };
      return (StateBeforeP _v0_0 _v1_0)
    }

type Interpretation = (Database, Database)
emptyInterpretation :: Interpretation
emptyInterpretation = (emptyDb, emptyDb)
data Phase = Phase0
             | Phase1 deriving (Eq, Show, Ord)
nextPhase :: Phase -> Phase
nextPhase Phase0 = Phase1
nextPhase Phase1 = Phase0
selectDb :: Interpretation -> Phase -> Database
selectDb (db, _) Phase0 = db
selectDb (_, db) Phase1 = db
(||=) :: Interpretation -> Fact -> Phase -> Bool
(||=) i f p = (|=) (selectDb i p) f
replaceDb :: Interpretation -> Database -> Phase -> Interpretation
replaceDb (_, db1) db' Phase0 = (db', db1)
replaceDb (db0, _) db' Phase1 = (db0, db')
insertToInterpretation :: Interpretation -> Fact -> Phase -> Interpretation
insertToInterpretation i f p = let { db = selectDb i p; db' = insertToDb db f } in replaceDb i db' p
evaluatePhased :: Kont -> ([Fact], Phase)
evaluatePhased (Kont (r, p)) = (evaluate r, nextPhase p)

infix 1 ||=
