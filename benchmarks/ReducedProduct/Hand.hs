module ReducedProduct.Hand where

import Algebra.PartialOrd
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as MinQueue
import ReducedProduct.Common

type ProgramPoint = Int

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
  | Branch !Expr !ProgramPoint !ProgramPoint
  | VarDecl !String
  deriving (Eq, Show)

type IState = HashMap String Interval
type PState = HashMap String Parity

type StateMap = IM.IntMap (PState, IState)

data PS = P (ProgramPoint, Stmt) | S (ProgramPoint, ProgramPoint)

mkVar :: ProgramPoint -> String -> PS
mkVar a b = P (a, VarDecl b)

mkAssign :: ProgramPoint -> String -> Expr -> PS
mkAssign a b c = P (a, Assign b c)

mkBranch :: ProgramPoint -> Expr -> ProgramPoint -> ProgramPoint -> PS
mkBranch a b c d = P (a, Branch b c d)

mkSeq :: ProgramPoint -> ProgramPoint -> PS
mkSeq a b = S (a, b)

solveHand :: [PS] -> (Int, StateMap)
solveHand ls =
  -- make the program
  let (pgm, seqs) = foldl' partitionPS (IM.empty, IM.empty) ls
      pgm_points = IM.keys pgm
      initial_q = MinQueue.fromList pgm_points
   in loop pgm seqs initial_q

loop :: Program -> Seqs -> MinQueue ProgramPoint -> (Int, StateMap)
loop pgm seqs q = go q IM.empty 0
  where
    go :: MinQueue ProgramPoint -> StateMap -> Int -> (Int, StateMap)
    go Empty mp n = (n, mp)
    go (p :< ps) mp n
      | Nothing <- pgm IM.!? p = go ps mp n
      | Just (VarDecl _) <- pgm IM.!? p = go ps mp n
      | Just (Assign x e) <- pgm IM.!? p =
          let -- find the current state before the assignment statement
              (p_st_bef_ass, i_st_bef_ass) = IM.findWithDefault (HashMap.empty, HashMap.empty) p mp
              -- evaluate the RHS, widen when the interval is too wide
              eval_p_result = HashMap.insert x (evalP e p_st_bef_ass) p_st_bef_ass
              eval_i_result = HashMap.insert x (evalI e i_st_bef_ass) i_st_bef_ass
              exchanged_i = reduceInterval eval_p_result eval_i_result
              afters = IM.findWithDefault IS.empty p seqs
              (new_work, new_map) = IS.foldl (fold_new_states (eval_p_result, exchanged_i)) (IS.empty, mp) afters
           in go (MinQueue.union (MinQueue.fromList $ IS.toList new_work) ps) new_map (n + IS.size new_work)
      | Just (Branch e t f) <- pgm IM.!? p =
          let -- get the current state at the condition
              (p_st_bef_cond, i_st_bef_cond) = IM.findWithDefault (HashMap.empty, HashMap.empty) p mp
              reduced_i_st_bef_cond = reduceInterval p_st_bef_cond i_st_bef_cond
              -- evaluate the condition
              eval_result = evalCondI e reduced_i_st_bef_cond
              -- get the current state at the true branch to potentially update
              (p_st_bef_true, i_st_bef_true) = IM.findWithDefault (HashMap.empty, HashMap.empty) t mp
              -- update the true branch based on the condition
              (new_work1, mp1) =
                if isTrue eval_result
                  then
                    let -- narrow the interval and parity given the expression
                        narrowed_state_i = refineTI e reduced_i_st_bef_cond
                        narrowed_state_p = refineTP e p_st_bef_cond
                        -- reduce the narrowed_state_i
                        reduced_narrowed_state_i = reduceInterval narrowed_state_p narrowed_state_i
                     in -- join the narrowed state at the condition with
                        -- whatever that already exists
                        -- new_st_at_true = joinI narrowed_state curr_st_at_true
                        if (narrowed_state_p, reduced_narrowed_state_i) `leq` (p_st_bef_true, i_st_bef_true) -- if new_st_at_true == curr_st_at_true
                          then ([], mp) -- nothing to update, already equal
                          else
                            let new_p_st_bef_true = joinP p_st_bef_true narrowed_state_p
                                new_i_st_bef_true = joinI i_st_bef_true reduced_narrowed_state_i
                                reduced_new_i_st_bef_true = reduceInterval new_p_st_bef_true new_i_st_bef_true
                             in ([t], IM.insert t (new_p_st_bef_true, reduced_new_i_st_bef_true) mp)
                  else ([], mp) -- true branch is not visited, dont propagate anything
              (p_st_bef_false, i_st_bef_false) = IM.findWithDefault (HashMap.empty, HashMap.empty) f mp1
              -- update the false branch based on the condition
              (new_work2, mp2) =
                if isFalse eval_result
                  then
                    let -- narrow the conditional given the expression
                        narrowed_state_i = refineFI e reduced_i_st_bef_cond
                        narrowed_state_p = refineFP e p_st_bef_cond
                        -- reduce the narrowed_state_i
                        reduced_narrowed_state_i = reduceInterval narrowed_state_p narrowed_state_i
                     in -- join the narrowed state at the condition with
                        -- whatever that already exists in the false br
                        -- new_st_at_false = joinI narrowed_state curr_st_at_false
                        if (narrowed_state_p, reduced_narrowed_state_i) `leq` (p_st_bef_false, i_st_bef_false) -- if new_st_at_false == curr_st_at_false
                          then (new_work1, mp1) -- nothing to update, already equal
                          else
                            let new_p_st_bef_false = joinP p_st_bef_false narrowed_state_p
                                new_i_st_bef_false = joinI i_st_bef_false reduced_narrowed_state_i
                                reduced_new_i_st_bef_false = reduceInterval new_p_st_bef_false new_i_st_bef_false
                             in (f : new_work1, IM.insert f (new_p_st_bef_false, reduced_new_i_st_bef_false) mp1)
                  else (new_work1, mp1) -- false branch not visited, dont propagate anything
           in go (MinQueue.union (MinQueue.fromList new_work2) ps) mp2 (n + length new_work2)
    fold_new_states :: (PState, IState) -> (IS.IntSet, StateMap) -> ProgramPoint -> (IS.IntSet, StateMap)
    fold_new_states int (pps, mp) p =
      let current_state = IM.findWithDefault (HashMap.empty, HashMap.empty) p mp
       in -- new_state = joinI current_state int
          if int `leq` current_state
            then -- if new_state == current_state
              (pps, mp) -- nothing to do
            else
              let (new_p, new_i) = int
                  (old_p, old_i) = current_state
                  new_state = (joinP new_p old_p, joinI new_i old_i)
               in (IS.insert p pps, IM.insert p new_state mp)

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
    mkBranch 4 (Leq (Id "V") (Num 9)) 5 6
  , mkAssign 5 "V" (Plus (Id "V") (Num 2))
  , mkSeq 5 4
  , -- if V == 11 then:
    mkBranch 6 (Eq (Id "V") (Num 11)) 7 101
  , mkAssign 7 "V" (Num 0)
  , mkSeq 7 101
  , -- end
    mkVar 101 "END"
  ]
