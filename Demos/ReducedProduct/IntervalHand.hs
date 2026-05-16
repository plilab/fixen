{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ReducedProduct.IntervalHand where

import Control.DeepSeq
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import ReducedProduct.Common

type ProgramPoint = Int
data Interval = Pair (Integer, Integer) | ITop | IBot
    deriving (Eq, Show)
instance NFData Interval where
    rnf (Pair x) = rnf x
    rnf ITop = ()
    rnf IBot = ()

data BBool = BTop | BFalse | BTrue | BBot
    deriving (Eq, Show)

isTrue :: BBool -> Bool
isTrue BTrue = True
isTrue BTop = True
isTrue _ = False

isFalse :: BBool -> Bool
isFalse BFalse = True
isFalse BTop = True
isFalse _ = False

subsumes :: Interval -> Interval -> Bool
subsumes ITop _ = True
subsumes _ IBot = True
subsumes IBot _ = False
subsumes _ ITop = False
subsumes (Pair (x, y)) (Pair (x', y')) = x <= x' && y >= y'

widen :: Interval -> Interval
widen (Pair (a, b))
    | b - a >= 30 = ITop
widen x = x

type Program = IM.IntMap Stmt

type Seqs = IM.IntMap IS.IntSet

data Stmt
    = Assign !String !Expr
    | Cond !Expr !ProgramPoint !ProgramPoint
    | VarDecl !String
    deriving (Show, Eq)

data Q a = Q [a] [a]

(<-:) :: Q a -> a -> Q a
Q [] [] <-: x = Q [x] []
Q xs ys <-: y = Q xs (y : ys)

uncons :: Q a -> Maybe (a, Q a)
uncons (Q [] []) = Nothing
uncons (Q [] ys) = uncons (Q (reverse ys) [])
uncons (Q (x : xs) ys) = Just (x, Q xs ys)

isEmpty :: Q a -> Bool
isEmpty (Q [] []) = True
isEmpty _ = False

pattern Empty :: Q a
pattern Empty <- (uncons -> Nothing)
    where
        Empty = Q [] []

pattern (:||) :: a -> Q a -> Q a
pattern x :|| xs <- (uncons -> Just (x, xs))
    where
        x :|| (Q xs ys) = Q (x : xs) ys
{-# COMPLETE Empty, (:||) #-}

pushAll :: S.Set a -> Q a -> Q a
pushAll s (Q [] []) = Q (S.toList s) []
pushAll s (Q xs ys) = Q xs (S.toList s ++ ys)

type IState = M.Map String Interval

type StateMap = IM.IntMap IState

joinI :: M.Map String Interval -> M.Map String Interval -> M.Map String Interval
joinI = M.unionWith joinSign

joinSign :: Interval -> Interval -> Interval
joinSign ITop _ = ITop
joinSign _ ITop = ITop
joinSign IBot s = s
joinSign s IBot = s
joinSign (Pair (a, b)) (Pair (c, d)) = Pair (min a c, max b d)
insertI :: String -> Interval -> M.Map String Interval -> M.Map String Interval
insertI = M.insert

singleton :: k -> a -> M.Map k a
singleton = M.singleton

evalI :: Expr -> M.Map String Interval -> Interval
evalI e st = case e of
    Num n -> Pair (fromIntegral n, fromIntegral n)
    Id x -> fromMaybe IBot (M.lookup x st)
    InputE -> ITop
    Plus e1 e2 -> case (evalI e1 st, evalI e2 st) of
        (Pair (a, b), Pair (c, d)) -> Pair (a + c, b + d)
        (_, _) -> IBot
    Leq _ _ -> error "Encounted leq" -- this should never happen, and be always handled by evaluateConditional
    Gte _ _ -> error "Encounted gte" -- this should never happen, and be always handled by evaluateConditional
    Eq _ _ -> error "Encountered eq" -- this should never happen, ...

evaluateConditional :: Expr -> M.Map String Interval -> BBool
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

-- TODO: IS THIS DIFFERENT??
narrowConditionalI :: Expr -> M.Map String Interval -> M.Map String Interval
narrowConditionalI e st = case e of
    Leq e1 e2 -> case (e1, evalI e2 st) of
        -- Only handle the trivial case, it's enough for our demo.
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> M.insert id (Pair (min a c, min b d)) st
            _ -> M.insert id IBot st
        (_, _) -> error "Unexpected conditional"
    Gte e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> M.union (M.fromList [(id, Pair (max a c, max b d))]) st
            _ -> M.union (M.fromList [(id, IBot)]) st
        (_, _) -> error "Unexpected conditional"
    Eq e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
        (_, _) -> error "Unexpected conditional"
    _ -> error "Unexpected case"

narrowConditionalIFalse :: Expr -> M.Map String Interval -> M.Map String Interval
narrowConditionalIFalse e st = case e of
    Leq e1 e2 -> case (e1, evalI e2 st) of
        -- Only handle the trivial case, it's enough for our demo.
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> M.union (M.fromList [(id, Pair (d + 1, max (d + 1) b))]) st
            _ -> M.union (M.fromList [(id, IBot)]) st
        (_, _) -> error "Unexpected conditional"
    Gte e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> case evalI e1 st of
            Pair (a, b) -> M.union (M.fromList [(id, Pair (min (c - 1) a, c - 1))]) st
            _ -> M.union (M.fromList [(id, IBot)]) st
        (_, _) -> error "Unexpected conditional"
    Eq e1 e2 -> case (e1, evalI e2 st) of
        (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
        (_, _) -> error "Unexpected conditional"
    _ -> error "Unexpected case"
data PS = P (ProgramPoint, Stmt) | S (ProgramPoint, ProgramPoint)

mkVar :: ProgramPoint -> String -> PS
mkVar a b = P (a, VarDecl b)

mkAssign :: ProgramPoint -> String -> Expr -> PS
mkAssign a b c = P (a, Assign b c)

mkCond :: ProgramPoint -> Expr -> ProgramPoint -> ProgramPoint -> PS
mkCond a b c d = P (a, Cond b c d)

mkSeq :: ProgramPoint -> ProgramPoint -> PS
mkSeq a b = S (a, b)

solveHand :: [PS] -> StateMap
solveHand ls =
    -- make the program
    let (pgm, seqs) = foldl' partitionPS (IM.empty, IM.empty) ls
        pgm_points = IM.keys pgm
        initial_q = pgm_points
     in loop pgm seqs initial_q

loop :: Program -> Seqs -> [ProgramPoint] -> StateMap
loop pgm seqs q = go q IM.empty
  where
    go :: [ProgramPoint] -> StateMap -> StateMap
    go [] mp = mp
    go (p : ps) mp
        | Nothing <- pgm IM.!? p = go ps mp
        | Just (VarDecl _) <- pgm IM.!? p = go ps mp
        | Just (Assign x e) <- pgm IM.!? p =
            let
                -- find the current state before the assignment statement
                st_at_assignment = IM.findWithDefault M.empty p mp
                -- evaluate the RHS, widen when the interval is too wide
                eval_result = widen $ evalI e st_at_assignment
                curr_interval = M.findWithDefault IBot x st_at_assignment
             in
                if curr_interval `subsumes` eval_result
                    then go ps mp -- nothing to do
                    else
                        let
                            -- get all the program points that come after this assignment
                            -- statement
                            afters = IM.findWithDefault IS.empty p seqs
                            new_interval = joinSign eval_result curr_interval
                            -- propagate the new state to everything that comes after.
                            -- the result has new work (where the states have been
                            -- updated), and the new map with the propagation done.
                            (new_work, new_map) = IS.foldl (fold_new_states (M.insert x new_interval st_at_assignment)) (IS.empty, mp) afters
                         in
                            go (IS.toList new_work ++ ps) new_map
        | Just (Cond e t f) <- pgm IM.!? p =
            let
                -- get the current state at the condition
                st_at_cond = IM.findWithDefault M.empty p mp
                -- evaluate the condition
                eval_result = evaluateConditional e st_at_cond
                -- get the current state at the true branch to potentially update
                curr_st_at_true = IM.findWithDefault M.empty t mp
                -- update the true branch based on the condition
                (new_work1, mp1) =
                    if isTrue eval_result
                        then
                            let
                                -- narrow the conditional given the expression
                                narrowed_state = narrowConditionalI e st_at_cond
                                -- join the narrowed state at the condition with
                                -- whatever that already exists
                                new_st_at_true = joinI narrowed_state curr_st_at_true
                             in
                                if new_st_at_true == curr_st_at_true
                                    then ([], mp) -- nothing to update, already equal
                                    else ([t], IM.insert t new_st_at_true mp)
                        else ([], mp) -- true branch is not visited, dont propagate anything
                curr_st_at_false = IM.findWithDefault M.empty f mp1
                -- update the false branch based on the condition
                (new_work2, mp2) =
                    if isFalse eval_result
                        then
                            let
                                -- narrow the conditional given the expression
                                narrowed_state = narrowConditionalIFalse e st_at_cond
                                -- join the narrowed state at the condition with
                                -- whatever that already exists in the false br
                                new_st_at_false = joinI narrowed_state curr_st_at_false
                             in
                                if new_st_at_false == curr_st_at_false
                                    then (new_work1, mp1) -- nothing to update, already equal
                                    else (f : new_work1, IM.insert f new_st_at_false mp1)
                        else (new_work1, mp1) -- false branch not visited, dont propagate anything
             in
                go (new_work2 ++ ps) mp2
    fold_new_states :: M.Map String Interval -> (IS.IntSet, StateMap) -> ProgramPoint -> (IS.IntSet, StateMap)
    fold_new_states int (pps, mp) p =
        let current_state = IM.findWithDefault M.empty p mp
            new_state = joinI current_state int
         in if new_state == current_state
                then (pps, mp) -- nothing to do
                else (IS.insert p pps, IM.insert p new_state mp)

partitionPS :: (Program, Seqs) -> PS -> (Program, Seqs)
partitionPS (pgm, seqs) (P (pp, stmt)) = (IM.insert pp stmt pgm, seqs)
partitionPS (pgm, seqs) (S (a, b)) =
    let curr = IM.findWithDefault IS.empty a seqs
        new = IS.insert b curr
     in (pgm, IM.insert a new seqs)

testHand :: [PS]
testHand =
    [ -- V = 1
      mkVar 0 "V"
    , mkSeq 0 1
    , mkAssign 2 "V" (Num 1)
    , mkSeq 2 4
    , -- while V <= 10
      mkCond 4 (Leq (Id "V") (Num 9)) 5 6
    , mkAssign 5 "V" (Plus (Id "V") (Num 2))
    , mkSeq 5 4
    , -- if V == 11 then:
      mkCond 6 (Eq (Id "V") (Num 11)) 7 101
    , mkAssign 7 "V" (Num 0)
    , mkSeq 7 101
    , -- end
      mkVar 101 "END"
    ]
