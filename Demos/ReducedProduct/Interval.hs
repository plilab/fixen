{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ReducedProduct.Interval where

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (singleton, lookup, empty, insertWith, map, fromList, union)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Foldable

import Numeric.Natural

-- transfer functions sourced from Tutorial on Static Inference of Numeric Invariants by
-- Abstract Interpretation by Antoine Miné

data Interval = Pair (Natural, Natural) | Top | Bot
  deriving (Eq, Show, Generic)

instance Hashable Interval

joinSign :: Interval -> Interval -> Interval
joinSign Top _ = Top
joinSign _ Top = Top
joinSign Bot s = s
joinSign s Bot = s
joinSign (Pair (a, b)) (Pair (c, d)) = Pair (min a c, max b d)

instance PartialOrd Interval where
  leq Bot _ = True
  leq _ Top = True
  leq Top _ = False
  leq _ Bot = False
  leq (Pair (a, b)) (Pair (c, d)) = a >= c && b <= d

instance MLB Interval where
  mlbs s1 s2  = case (s1, s2) of
    (Pair (a, b), Pair (c, d)) -> [if max a c <= min b d then Pair (max a c, min b d) else Bot]
    (_, _) -> [Bot]

joinState :: (Foldable db) => (State -> f) -> State -> db State -> [f]
joinState k s db = map (k . joinState) (s : toList db)
  where
    joinState = join s


widenState :: (Foldable db) => (State -> f) -> State -> db State -> [f]
widenState k s db = map (k . widen) (s : toList db)
  where
    widen = M.map widenInterval
    widenInterval p@(Pair (m, n)) = if n - m > 30 then Top else p
    widenInterval other = other

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr
  deriving (Eq, Show, Generic)

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)

type State = Map String Interval

join :: State -> State -> State
join = unionWith joinSign

insert :: String -> Interval -> Map String Interval -> Map String Interval
insert = M.insertWith joinSign

singleton :: k -> a -> Map k a
singleton = M.singleton

empty :: Int -> Map k a
empty = const M.empty

eval :: Expr -> State -> Interval
eval e st = case e of
  Num n -> Pair (fromIntegral n, fromIntegral n)
  Id x -> fromMaybe Bot (M.lookup x st)
  InputE -> Top
  Plus e1 e2 -> case (eval e1 st, eval e2 st) of
    (Pair (a, b), Pair (c, d)) -> Pair (a + c, b + d)
    (_, _) -> Bot
  -- Times e1 e2 -> case (eval e1 st, eval e2 st) of
  --   (Pair (a, b), Pair (c, d)) -> Pair (min ( a c) (a * d) (b * c) (b * d),  max(a * c) (a * d) (b * c) (b * d))
  --   (_, _) -> Bot
  Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
  Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional

evaluateConditional :: Expr -> State -> Bool
evaluateConditional e st = case e of
  Leq e1 e2 -> case (eval e1 st, eval e2 st) of
    (Pair (a, b), Pair (c, d)) -> a <= c && b <= d
    (_, Top) -> True
    (Bot, _) -> True
    (_, _) -> error "Unexpected conditional"
  Gte e1 e2 -> case (eval e1 st, eval e2 st) of
    (Pair (a, b), Pair (c, d)) -> a >= c && b >= d
    (Top, _) -> True
    (_, Bot) -> True
    (Bot, _) -> False -- Technically not true, but it doesn't matter, as bottom will be overrided later.
    (_, _) -> error "Unexpected conditional"
  _ -> error "Unexpected case"


-- narrowConditional :: Expr -> State -> State
-- narrowConditional e st = case e of
--   Leq e1 e2 -> case (e1, eval e2 st) of
--     -- Only handle the trivial case, it's enough for our demo.
--     (Id id, Pair (c, d)) -> case eval e1 st of
--       Pair (a, b) -> M.fromList [(id, Pair (min a c, min b d))]
--       _ -> M.fromList [(id, Bot)]
--     (_, _) -> error "Unexpected conditional"
--   Gte e1 e2 -> case (e1, eval e2 st) of
--     (Id id, Pair (c, d)) -> case eval e1 st of
--       Pair (a, b) -> M.union (M.fromList [(id, Pair (max a c,  max b d))]) st
--       _ -> M.union (M.fromList [(id, Bot)]) st
--     (_, _) -> error "Unexpected conditional"    
--   _ -> error "Unexpected case"

-- narrowConditionalFalse :: Expr -> State -> State
-- narrowConditionalFalse e st = case e of
--   Leq e1 e2 -> case (e1, eval e2 st) of
--     -- Only handle the trivial case, it's enough for our demo.
--     (Id id, Pair (c, d)) -> case eval e1 st of
--       Pair (a, b) -> M.union (M.fromList [(id, Pair (d+1, max (d+1) b))]) st
--       _ -> M.union (M.fromList [(id, Bot)]) st
--     (_, _) -> error "Unexpected conditional"
--   Gte e1 e2 -> case (e1, eval e2 st) of
--     (Id id, Pair (c, d)) -> case eval e1 st of
--       Pair (a, b) -> M.union (M.fromList [(id, Pair (min (c-1) a,  c-1))]) st
--       _ -> M.union (M.fromList [(id, Bot)]) st
--     (_, _) -> error "Unexpected conditional"
--   _ -> error "Unexpected case"

eq :: Bool -> Bool -> Bool
eq = (==)