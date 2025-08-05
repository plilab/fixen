{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module ReducedProduct.IsEven where

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

data Evenness = IsEven | IsOdd | Top | Bot
  deriving (Eq, Show, Generic)

instance Hashable Evenness

joinSign :: Evenness -> Evenness -> Evenness
joinSign Top _ = Top
joinSign _ Top = Top
joinSign Bot s = s
joinSign s Bot = s
joinSign s1 s2 = if s1 == s2 then s1 else Top 


instance PartialOrd Evenness where
  leq :: Evenness -> Evenness -> Bool
  leq Bot _ = True
  leq _ Top = True
  leq Top _ = False
  leq _ Bot = False
  leq s1 s2 = s1 == s2


instance MLB Evenness where
  mlbs s1 s2  = case (s1, s2) of
    (Top, s) -> [s]
    (Bot, _) -> [Bot]
    (st1, st2)
      | st1 == st2 -> [st1]
    (_, _) -> []

joinState :: (Foldable db) => (State -> f) -> State -> db State -> [f]
joinState k s db = map (k . joinState) (s : toList db)
  where
    joinState = join s


data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
  deriving (Eq, Show, Generic)

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)

type State = Map String Evenness

join :: State -> State -> State
join = unionWith joinSign

insert :: String -> Evenness -> Map String Evenness -> Map String Evenness
insert = M.insertWith joinSign

singleton :: k -> a -> Map k a
singleton = M.singleton

empty :: Int -> Map k a
empty = const M.empty

eval :: Expr -> State -> Evenness
eval e st = case e of
  Num n -> if even n then IsEven else IsOdd
  Id x -> fromMaybe Bot (M.lookup x st)
  InputE -> Top
  Plus e1 e2 -> case (eval e1 st, eval e2 st) of
    (s1, s2) | s1 == s2 -> IsEven
             | otherwise -> IsOdd 
  -- Times e1 e2 -> case (eval e1 st, eval e2 st) of
  --   (Pair (a, b), Pair (c, d)) -> Pair (min ( a c) (a * d) (b * c) (b * d),  max(a * c) (a * d) (b * c) (b * d))
  --   (_, _) -> Bot
  Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
  Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional
  Eq _ _ -> error "Encountered eq" -- this should never happen, ...

narrowConditional :: Expr -> State -> State
narrowConditional e st = case e of
  Leq e1 e2 -> st
  Gte e1 e2 -> st
  Eq e1 e2 -> case (e1, eval e2 st) of
    (Id id, evenness) -> insert id evenness st
    (_, _) -> error "Unexpected conditional"    
  _ -> error "Unexpected case"

narrowConditionalFalse :: Expr -> State -> State
narrowConditionalFalse e st = case e of
  Leq e1 e2 -> case (e1, eval e2 st) of
    (Id id, evenness) -> insert id Bot st
    (_, _) -> error "Unexpected conditional"    
  Gte e1 e2 -> case (e1, eval e2 st) of
    (Id id, evenness) -> insert id Bot st
    (_, _) -> error "Unexpected conditional"    
  Eq e1 e2 -> case (e1, eval e2 st) of
    (Id id, evenness) -> insert id Bot st
    (_, _) -> error "Unexpected conditional"    
  _ -> error "Unexpected case"
