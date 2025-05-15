{-# LANGUAGE DeriveGeneric #-}
module Dataflow.Sign where

import Algebra.PartialOrd
import Data.Map ( Map, unionWith ) 
import qualified Data.Map as M (insert, singleton, lookup, empty)
import Common.Definitions
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)

data Sign = Pos | Neg | Zero | Top | Bot
  deriving (Eq, Show, Generic)

instance Hashable Sign

joinSign :: Sign -> Sign -> Sign
joinSign Top _ = Top
joinSign _ Top = Top
joinSign Bot s = s
joinSign s Bot = s
joinSign s1 s2 
  | s1 == s2  = s1
  | otherwise = Top

instance PartialOrd Sign where
  leq Bot _ = True
  leq _ Top = True
  leq s1 s2 = s1 == s2

instance MLB Sign where
  mlbs s1 s2 | leq s1 s2 = [s1]
             | leq s2 s1 = [s2]
             | otherwise = [Bot]

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Times Expr Expr | Leq Expr Expr deriving (Eq, Show, Generic)

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)

type State = Map String Sign

join :: State -> State -> State
join = unionWith joinSign

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert = M.insert

singleton :: k -> a -> Map k a
singleton = M.singleton

empty :: Int -> Map k a
empty = const M.empty

eval :: Expr -> State -> Sign
eval e st = case e of
  Num n | n > 0     -> Pos
        | n == 0    -> Zero
        | otherwise -> Neg
  Id x -> fromMaybe Bot (M.lookup x st)
  InputE -> Top
  Plus e1 e2 -> case (eval e1 st, eval e2 st) of
    (Bot, _) -> Bot
    (_, Bot) -> Bot
    (Top, _) -> Top
    (_, Top) -> Top
    (Pos, Pos) -> Pos
    (Pos, Zero) -> Pos
    (Pos, Neg) -> Top
    (Zero, Pos) -> Pos
    (Zero, Zero) -> Zero
    (Neg, Neg) -> Neg
    (Zero, Neg) -> Neg
    (Neg, Zero) -> Neg
    (Neg, Pos) -> Top
  Times e1 e2 -> case (eval e1 st, eval e2 st) of
    (Bot, _)    -> Bot
    (_, Bot)    -> Bot
    (Top, _) -> Top
    (_, Top) -> Top
    (Pos, Pos) -> Pos
    (Pos, Zero) -> Zero
    (Pos, Neg) -> Neg
    (Zero, Pos) -> Zero
    (Zero, Zero) -> Zero
    (Zero, Neg) -> Zero
    (Neg, Pos)  -> Neg
    (Neg, Zero) -> Zero
    (Neg, Neg)  -> Pos
  Leq e1 e2 -> case (eval e1 st, eval e2 st) of
    (Bot, _) -> Bot
    (_, Bot) -> Bot
    (Top, _) -> Top
    (_, Top) -> Top
    (Pos, Pos) -> Top
    (Pos, Zero) -> Zero
    (Pos, Neg) -> Zero
    (Neg, Pos) -> Pos
    (Neg, Zero) -> Pos
    (Neg, Neg) -> Top
    (Zero, Pos) -> Pos
    (Zero, Zero) -> Pos
    (Zero, Neg) -> Zero