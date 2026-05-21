module ReducedProduct.Hand where

import Algebra.PartialOrd
import Control.DeepSeq
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as MinQueue
import GHC.Generics (Generic)

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
  deriving (Eq, Show, Generic)

instance NFData Expr where
  rnf (Id x) = rnf x
  rnf (Num x) = rnf x
  rnf (Plus e e') = rnf (e, e')
  rnf (Leq e e') = rnf (e, e')
  rnf (Gte e e') = rnf (e, e')
  rnf (Eq e e') = rnf (e, e')
  rnf InputE = ()

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)
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

type IState = M.Map String Interval
type PState = HashMap String Parity

data Parity = IsEven | IsOdd | ParityTop | ParityBot
  deriving (Eq, Show, Generic)

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
  Leq _ _ -> st
  Gte _ _ -> st
  Eq e1 e2 -> case (e1, evalP e2 st) of
    (Id id', evenness) -> insertP id' evenness st
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

statePMlbs :: HashMap String Parity -> HashMap String Parity -> [HashMap String Parity]
statePMlbs x y = [HashMap.intersectionWith meetSign x y]

type StateMap = IM.IntMap (PState, IState)

stateILeq :: HashMap String Interval -> HashMap String Interval -> Bool
stateILeq = leq

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
joinI :: M.Map String Interval -> M.Map String Interval -> M.Map String Interval
joinI = M.unionWith joinIntervals

joinIntervals :: Interval -> Interval -> Interval
joinIntervals ITop _ = ITop
joinIntervals _ ITop = ITop
joinIntervals IBot s = s
joinIntervals s IBot = s
joinIntervals (Pair (a, b)) (Pair (c, d)) = Pair (min a c, max b d)
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
    (_, _) -> st -- error "Unexpected conditional"
  Gte e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (max a c, max b d))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> st -- error "Unexpected conditional"
  Eq e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
    (_, _) -> st -- error "Unexpected conditional"
  _ -> error "Unexpected case"

narrowConditionalIFalse :: Expr -> M.Map String Interval -> M.Map String Interval
narrowConditionalIFalse e st = case e of
  Leq e1 e2 -> case (e1, evalI e2 st) of
    -- Only handle the trivial case, it's enough for our demo.
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (d + 1, max (d + 1) b))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> st -- error "Unexpected conditional"
  Gte e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> case evalI e1 st of
      Pair (a, b) -> M.union (M.fromList [(id, Pair (min (c - 1) a, c - 1))]) st
      _ -> M.union (M.fromList [(id, IBot)]) st
    (_, _) -> st -- error "Unexpected conditional"
  Eq e1 e2 -> case (e1, evalI e2 st) of
    (Id id, Pair (c, d)) -> M.union (M.fromList [(id, Pair (c, d))]) st
    (_, _) -> st -- error "Unexpected conditional"
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

reduceInterval :: HashMap String Parity -> IState -> IState
reduceInterval st st' = M.mapWithKey enrich st'
  where
    enrich :: String -> Interval -> Interval
    enrich k interval = case st HashMap.!? k of
      Nothing -> IBot
      Just evenness -> case (evenness, interval) of
        (IsEven, Pair (lo, hi)) -> Pair (if odd lo then lo + 1 else lo, if odd hi then hi - 1 else hi)
        (IsOdd, Pair (lo, hi)) -> Pair (if even lo then lo + 1 else lo, if even hi then hi - 1 else hi)
        (ParityBot, _) -> IBot
        (_, i) -> i

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
              (p_st_bef_ass, i_st_bef_ass) = IM.findWithDefault (HashMap.empty, M.empty) p mp
              -- evaluate the RHS, widen when the interval is too wide
              eval_p_result = HashMap.insert x (evalP e p_st_bef_ass) p_st_bef_ass
              eval_i_result = M.insert x (widen $ evalI e i_st_bef_ass) i_st_bef_ass
              exchanged_i = reduceInterval eval_p_result eval_i_result
              afters = IM.findWithDefault IS.empty p seqs
              (new_work, new_map) = IS.foldl (fold_new_states (eval_p_result, exchanged_i)) (IS.empty, mp) afters
           in go (MinQueue.union (MinQueue.fromList $ IS.toList new_work) ps) new_map (n + IS.size new_work)
      | Just (Cond e t f) <- pgm IM.!? p =
          let -- get the current state at the condition
              (p_st_bef_cond, i_st_bef_cond) = IM.findWithDefault (HashMap.empty, M.empty) p mp
              reduced_i_st_bef_cond = reduceInterval p_st_bef_cond i_st_bef_cond
              -- evaluate the condition
              eval_result = evaluateConditional e reduced_i_st_bef_cond
              -- get the current state at the true branch to potentially update
              (p_st_bef_true, i_st_bef_true) = IM.findWithDefault (HashMap.empty, M.empty) t mp
              -- update the true branch based on the condition
              (new_work1, mp1) =
                if isTrue eval_result
                  then
                    let -- narrow the interval and parity given the expression
                        narrowed_state_i = narrowConditionalI e reduced_i_st_bef_cond
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
              (p_st_bef_false, i_st_bef_false) = IM.findWithDefault (HashMap.empty, M.empty) f mp1
              -- update the false branch based on the condition
              (new_work2, mp2) =
                if isFalse eval_result
                  then
                    let -- narrow the conditional given the expression
                        narrowed_state_i = narrowConditionalIFalse e reduced_i_st_bef_cond
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
      let current_state = IM.findWithDefault (HashMap.empty, M.empty) p mp
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
