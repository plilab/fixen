{-# LANGUAGE DeriveGeneric #-}
module ReducedProduct.Interval where

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (insert, singleton, lookup, empty, insertWith)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

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
joinSign s1 s2 = case (s1, s2) of
  (Pair (a, b), Pair (c, d)) -> Pair (min a c, max b d)


instance PartialOrd Interval where
  leq Bot _ = True
  leq _ Top = True
  leq s1 s2 = s1 == s2

instance MLB Interval where
  mlbs s1 s2  = case (s1, s2) of
    (Pair (a, b), Pair (c, d)) -> [if max a c <= min c d then Pair (max a c, min c d) else Bot]
    (_, _) -> [Bot]

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr deriving (Eq, Show, Generic)

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

-- the spurious argument is a hack to make sure the parser understands that 
-- `empty` is not a variable but a function, we should come up with a better solution
-- (at least add support for a unit type and use it as an argument instead of a number)
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
  Leq e1 e2 -> case (eval e1 st, eval e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Pair (_, b), Pair (c, d)) -> Pair (c, min b d)
    (_, _) -> Bot

evaluateConditional :: Expr -> State -> Bool
evaluateConditional e st = case e of
  Leq e1 e2 -> case (eval e1 st, eval e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Pair (a, b), Pair (c, d)) -> a <= c && b <= d
    (_, _) -> False
  _ -> False