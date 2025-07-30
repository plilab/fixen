{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ReducedProduct.Congruence where

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (singleton, lookup, empty, insertWith, map, fromList, union)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Foldable
import Data.List as L (head)

import Numeric.Natural

-- transfer functions sourced from Tutorial on Static Inference of Numeric Invariants by
-- Abstract Interpretation by Antoine Miné

data Congruence = Pair (Natural, Natural) | Top | Bot
  deriving (Eq, Show, Generic)

instance Hashable Congruence

joinSign :: Congruence -> Congruence -> Congruence
joinSign Top _ = Top
joinSign _ Top = Top
joinSign Bot s = s
joinSign s Bot = s
joinSign (Pair (a, b)) (Pair (c, d)) = Pair (gcd (gcd a c) (abs (if b >= d then b-d else d-b)), b) 


divides :: Natural -> Natural -> Bool
divides y y' = if y /= 0 then (rem y' y) == 0 else False 

congruence :: Natural -> Natural -> Natural -> Bool
congruence x x' y = divides y (abs (x - x'))

instance PartialOrd Congruence where
  leq Bot _ = True
  leq _ Top = True
  leq Top _ = False
  leq _ Bot = False
  leq (Pair (a, b)) (Pair (a', b')) = divides a' a && congruence b b' a'


instance MLB Congruence where
  mlbs s1 s2  = case (s1, s2) of
    (Pair (a, b), Pair (a', b')) -> [Pair (lcm a a', fromInteger (L.head ((besout (toInteger b') (toInteger (gcd a a'))))))]
    (_, _) -> [Bot]

joinState :: (Foldable db) => (State -> f) -> State -> db State -> [f]
joinState k s db = map (k . joinState) (s : toList db)
  where
    joinState = join s


data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
  deriving (Eq, Show, Generic)

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)

type State = Map String Congruence

join :: State -> State -> State
join = unionWith joinSign

insert :: String -> Congruence -> Map String Congruence -> Map String Congruence
insert = M.insertWith joinSign

singleton :: k -> a -> Map k a
singleton = M.singleton

empty :: Int -> Map k a
empty = const M.empty

eval :: Expr -> State -> Congruence
eval e st = case e of
  Num n -> Pair (fromIntegral n, fromIntegral n)
  Id x -> fromMaybe Bot (M.lookup x st)
  InputE -> Top
  Plus e1 e2 -> case (eval e1 st, eval e2 st) of
    (Pair (a, b), Pair (c, d)) -> if c /= 0 then Pair (gcd a c, b + d) else Pair (1, 0) 
    (_, _) -> Bot
  -- Times e1 e2 -> case (eval e1 st, eval e2 st) of
  --   (Pair (a, b), Pair (c, d)) -> Pair (min ( a c) (a * d) (b * c) (b * d),  max(a * c) (a * d) (b * c) (b * d))
  --   (_, _) -> Bot
  Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
  Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional
  Eq _ _ -> error "Encountered eq" -- this should never happen, ...

narrowConditional :: Expr -> State -> State
narrowConditional e st = case e of
  Leq e1 e2 -> case (e1, eval e2 st) of
    -- Only handle the trivial case or X < Z (where Z is integer), it's enough for our demo.
    (Id id, Pair (c, d)) -> case eval e1 st of
      Pair (a, b) -> M.fromList [(id, Pair (gcd a c, rem b (gcd a c)))]
      _ -> M.fromList [(id, Bot)]
    (_, _) -> error "Unexpected conditional"
  Gte e1 e2 -> case (e1, eval e2 st) of
    (Id id, Pair (c, d)) -> case eval e1 st of
      Pair (a, b) -> M.fromList [(id, Pair (gcd a c, rem b (gcd a c)))]
      _ -> M.fromList [(id, Bot)]
    (_, _) -> error "Unexpected conditional"
  Eq e1 e2 -> case (e1, eval e2 st) of
    (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
    (_, _) -> error "Unexpected conditional"    
  _ -> error "Unexpected case"

narrowConditionalFalse :: Expr -> State -> State
narrowConditionalFalse e st = case e of
  Leq e1 e2 -> case (e1, eval e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Id id, Pair (c, d)) -> case eval e1 st of
      Pair (a, b) -> M.fromList [(id, Pair (1, 0))]
      _ -> M.union (M.fromList [(id, Bot)]) st
    (_, _) -> error "Unexpected conditional"
  Gte e1 e2 -> case (e1, eval e2 st) of
    (Id id, Pair (c, d)) -> case eval e1 st of
      Pair (a, b) -> M.fromList [(id, Pair (1, 0))]
      _ -> M.union (M.fromList [(id, Bot)]) st
    (_, _) -> error "Unexpected conditional"
  Eq e1 e2 -> case (e1, eval e2 st) of
    (Id id, Pair (c, d)) -> M.fromList [(id, Pair (1, 0))]
    (_, _) -> error "Unexpected conditional"        
  _ -> error "Unexpected case"

eq :: Bool -> Bool -> Bool
eq = (==)