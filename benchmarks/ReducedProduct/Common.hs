module ReducedProduct.Common where

import Algebra.PartialOrd
import Control.DeepSeq
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.Maybe
import GHC.Generics
import Numeric.Natural

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Times Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
  deriving (Eq, Generic, Show)

instance NFData Expr where
  rnf (Id x) = rnf x
  rnf (Num x) = rnf x
  rnf (Plus e e') = rnf (e, e')
  rnf (Times e e') = rnf (e, e')
  rnf (Leq e e') = rnf (e, e')
  rnf (Gte e e') = rnf (e, e')
  rnf (Eq e e') = rnf (e, e')
  rnf InputE = ()

instance Hashable Expr

type Label = Natural

instance PartialOrd Expr where
  leq = (==)

stateILeq :: HashMap String Interval -> HashMap String Interval -> Bool
stateILeq = leq

meetI :: HashMap String Interval -> HashMap String Interval -> HashMap String Interval
meetI x y = HashMap.intersectionWith meetInterval x y

stateIMlbs :: HashMap String Interval -> HashMap String Interval -> [HashMap String Interval]
stateIMlbs x y = [HashMap.intersectionWith meetInterval x y]

data Interval = Pair (Integer, Integer) | ITop | IBot
  deriving (Eq, Generic, Show)

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
  deriving (Eq, Generic, Show)

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
  Times e1 e2 -> case (evalI e1 st, evalI e2 st) of
    (Pair (a, b), Pair (c, d)) ->
      let products = [a * c, a * d, b * c, b * d]
       in Pair (minimum products, maximum products)
    (_, _) -> IBot
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
  Eq e1 e2 -> case (e1, evalI e1 st, evalI e2 st) of
    -- Only handle the trivial case, we asume e2 is just a num, and that it lies on the boundary of e1, since our intervals cannot support disjoint intervals.
    (Id id, Pair (a, b), Pair (c, d))
      | c /= d -> st
      | a == c && b == c -> HashMap.union (HashMap.fromList [(id, IBot)]) st
      | a == c -> HashMap.union (HashMap.fromList [(id, Pair (a + 1, b))]) st
      | b == c -> HashMap.union (HashMap.fromList [(id, Pair (a, b - 1))]) st
      | otherwise -> st
    (_, _, _) -> st -- error "Unexpected conditional"
  _ -> error "Unexpected case"

data Parity = IsEven | IsOdd | ParityTop | ParityBot
  deriving (Eq, Generic, Show)

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
    (s1, s2)
      | s1 == s2 -> IsEven
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
reduceInterval st st' = HashMap.mapWithKey enrich st'
  where
    enrich :: String -> Interval -> Interval
    enrich k interval = case st HashMap.!? k of
      Nothing -> IBot
      Just evenness -> case (evenness, interval) of
        (IsEven, Pair (lo, hi)) -> Pair (if odd lo then lo + 1 else lo, if odd hi then hi - 1 else hi)
        (IsOdd, Pair (lo, hi)) -> Pair (if even lo then lo + 1 else lo, if even hi then hi - 1 else hi)
        (ParityBot, _) -> IBot
        (_, i) -> i
