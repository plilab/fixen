{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ReducedProduct.Interval where

import ReducedProduct.Common

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (singleton, lookup, empty, insert, map, fromList, union)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Foldable

import Numeric.Natural

-- transfer functions sourced from Tutorial on Static Inference of Numeric Invariants by
-- Abstract Interpretation by Antoine Miné

data Interval = Pair (Natural, Natural) | ITop | IBot
  deriving (Eq, Show, Generic)

instance Hashable Interval

joinSign :: Interval -> Interval -> Interval
joinSign ITop _ = ITop
joinSign _ ITop = ITop
joinSign IBot s = s
joinSign s IBot = s
joinSign (Pair (a, b)) (Pair (c, d)) = Pair (min a c, max b d)

instance PartialOrd Interval where
  leq IBot _ = True
  leq _ ITop = True
  leq ITop _ = False
  leq _ IBot = False
  leq (Pair (a, b)) (Pair (c, d)) = a >= c && b <= d

instance MLB Interval where
  mlbs s1 s2  = case (s1, s2) of
    (Pair (a, b), Pair (c, d)) -> [if max a c <= min b d then Pair (max a c, min b d) else IBot]
    (_, _) -> [IBot]

joinState :: (Foldable db) => (IState -> f) -> IState -> db IState -> [f]
joinState k s db = map (k . joinState) (s : toList db)
  where
    joinState = joinI s
    --- if isOdd: tighten interval [x, y] (if x isEven, x + 1, if y isEven y - 1)
    --- if isEven: tighten interval [x, y] (if x iOdd, x + 1)


widenState :: (Foldable db) => (IState -> f) -> IState -> db IState -> [f]
widenState k s db = map (k . widen) (s : toList db)
  where
    widen = M.map widenInterval
    widenInterval p@(Pair (m, n)) = if n - m > 30 then ITop else p
    widenInterval other = other


data BBool = BTop | BFalse | BTrue | BBot
  deriving (Eq, Show, Generic)

instance Hashable BBool

instance PartialOrd BBool where
  leq BBot _ = True
  leq _ BTop = True
  leq s1 s2 = s1 == s2

instance MLB BBool where
  mlbs s1 s2  = case (s1, s2) of
    (BTrue, BTrue) -> [BTrue]
    (BFalse, BFalse) -> [BFalse]
    (BFalse, BTrue) -> [BTop]
    (BTrue, BFalse) -> [BTop]
    (BTop, _) -> [BTop]
    (_, BTop) -> [BTop]
    (rest, BBot) -> [rest]
    (BBot, rest) -> [rest]

type IState = Map String Interval

joinI :: IState -> IState -> IState
joinI = unionWith joinSign

insertI :: String -> Interval -> Map String Interval -> Map String Interval
insertI = M.insert

singleton :: k -> a -> Map k a
singleton = M.singleton

emptyI :: Int -> Map k a
emptyI = const M.empty

evalI :: Expr -> IState -> Interval
evalI e st = case e of
  Num n -> Pair (fromIntegral n, fromIntegral n)
  Id x -> fromMaybe IBot (M.lookup x st)
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

evaluateConditional :: Expr -> IState -> BBool
evaluateConditional e st = case e of
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


narrowConditionalI :: Expr -> IState -> IState
narrowConditionalI e st = case e of
  Leq e1 e2 -> case (e1, evalI e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.fromList [(id, Pair (min a c, min b d))]
      _ -> M.fromList [(id, IBot)]
    (_, _) -> error "Unexpected conditional"
  Gte e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (max a c,  max b d))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> error "Unexpected conditional"
  Eq e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
    (_, _) -> error "Unexpected conditional"    
  _ -> error "Unexpected case"

narrowConditionalIFalse :: Expr -> IState -> IState
narrowConditionalIFalse e st = case e of
  Leq e1 e2 -> case (e1, evalI e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (d+1, max (d+1) b))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> error "Unexpected conditional"
  Gte e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (min (c-1) a,  c-1))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> error "Unexpected conditional"
  Eq e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
    (_, _) -> error "Unexpected conditional"        
  _ -> error "Unexpected case"
