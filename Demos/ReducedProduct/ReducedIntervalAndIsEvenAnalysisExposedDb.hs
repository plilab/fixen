{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.ReducedIntervalAndIsEvenAnalysisExposedDb where
import ReducedProduct.IsEven
import ReducedProduct.Interval
import ReducedProduct.Common
import Debug.Trace
import GHC.IO (unsafePerformIO)
import Prettyprinter (Pretty(pretty), vsep)
import Algebra.PartialOrd
import Common.Definitions
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (Foldable(foldl'))
import Data.Hashable
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.PQueue.Max as Q
import GHC.Generics (Generic)
import Numeric.Natural
--- HANDWRITTEN CODE START
import qualified Data.Map as MP
--- HANDWRITTEN CODE END

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes y x = leq x y && not (leq y x)

data StateBeforeIsEven = StateBeforeIsEven Natural State
                           deriving (Eq, Show, Generic)

instance Hashable StateBeforeIsEven

instance PartialOrd StateBeforeIsEven where
        leq (StateBeforeIsEven v0 v1) (StateBeforeIsEven v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkStateBeforeIsEven v0 v1
  = StateBeforeIsEvenFact (StateBeforeIsEven v0 v1)

data Seq = Seq Natural Natural
             deriving (Eq, Show, Generic)

instance Hashable Seq

instance PartialOrd Seq where
        leq (Seq v0 v1) (Seq v0' v1') = (v0 `leq` v0') && (v1 `leq` v1')
mkSeq v0 v1 = SeqFact (Seq v0 v1)

data Cond = Cond Natural Expr Natural Natural
              deriving (Eq, Show, Generic)

instance Hashable Cond

instance PartialOrd Cond where
        leq (Cond v0 v1 v2 v3) (Cond v0' v1' v2' v3')
          = (v0 `leq` v0') &&
              (v1 `leq` v1') && (v2 `leq` v2') && (v3 `leq` v3')
mkCond v0 v1 v2 v3 = CondFact (Cond v0 v1 v2 v3)

data Assign = Assign Natural String Expr
                deriving (Eq, Show, Generic)

instance Hashable Assign

instance PartialOrd Assign where
        leq (Assign v0 v1 v2) (Assign v0' v1' v2')
          = (v0 `leq` v0') && (v1 `leq` v1') && (v2 `leq` v2')
mkAssign v0 v1 v2 = AssignFact (Assign v0 v1 v2)

data Var = Var Natural String
             deriving (Eq, Show, Generic)

instance Hashable Var

instance PartialOrd Var where
        leq (Var v0 v1) (Var v0' v1') = (v0 `leq` v0') && (v1 `leq` v1')
mkVar v0 v1 = VarFact (Var v0 v1)

data StateBeforeInterval = StateBeforeInterval Natural IState
                             deriving (Eq, Show, Generic)

instance Hashable StateBeforeInterval

instance PartialOrd StateBeforeInterval where
        leq (StateBeforeInterval v0 v1) (StateBeforeInterval v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkStateBeforeInterval v0 v1
  = StateBeforeIntervalFact (StateBeforeInterval v0 v1)

data Fact = StateBeforeIsEvenFact StateBeforeIsEven
          | SeqFact Seq
          | CondFact Cond
          | AssignFact Assign
          | VarFact Var
          | StateBeforeIntervalFact StateBeforeInterval
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | EvalCondTrueIntervalCont Natural IState Expr Natural Natural
                                             IState
                  | EvalCondFalseIsEvenCont Natural State Expr Natural Natural State
                  | AssignStepIsEvenCont Natural String Expr State Natural State
                  | CondInitIsEvenCont Natural Expr Natural Natural
                  | VarInitIsEvenCont Natural String
                  | CondInitIntervalCont Natural Expr Natural Natural
                  | AssignInitIsEvenCont Natural String Expr
                  | AssignInitIntervalCont Natural String Expr
                  | VarInitIntervalCont Natural String
                  | EvalCondFalseIntervalCont Natural IState Expr Natural Natural
                                              IState
                  | AssignStepIntervalCont Natural String Expr IState Natural IState
                  | EvalCondTrueIsEvenCont Natural State Expr Natural Natural State
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db
  (EvalCondTrueIntervalCont lcond3 stc3 e6 jlt4 jlf4 stt1)
  = reducedExchange
      jlt4
      (StateBeforeIntervalFact . StateBeforeInterval jlt4)
      (joinI (narrowConditionalI e6 stc3) stt1)
      db
evaluate db (EvalCondFalseIsEvenCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeIsEvenFact
       (StateBeforeIsEven jlf0
          (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db
  (AssignStepIsEvenCont lAssign0 x0 e2 stAssign0 lAfter0 stAfter0)
  = [StateBeforeIsEvenFact
       (StateBeforeIsEven lAfter0
          (join (insert x0 (eval e2 stAssign0) stAssign0) stAfter0))]
evaluate db (CondInitIsEvenCont l1 e3 jlt2 jlf2)
  = [StateBeforeIsEvenFact (StateBeforeIsEven l1 (empty 0))]
evaluate db (VarInitIsEvenCont l0 x1)
  = [StateBeforeIsEvenFact (StateBeforeIsEven l0 (empty 0))]
evaluate db (CondInitIntervalCont l4 e8 jlt5 jlf5)
  = reducedExchange
      l4
      (StateBeforeIntervalFact . StateBeforeInterval l4)
      (emptyI 0)
      db
evaluate db (AssignInitIsEvenCont l2 x2 e4)
  = [StateBeforeIsEvenFact (StateBeforeIsEven l2 (empty 0))]
evaluate db (AssignInitIntervalCont l5 x5 e9)
  = reducedExchange
      l5
      (StateBeforeIntervalFact . StateBeforeInterval l5)
      (emptyI 0)
      db
evaluate db (VarInitIntervalCont l3 x4)
  = reducedExchange
      l3
      (StateBeforeIntervalFact . StateBeforeInterval l3)
      (emptyI 0)
      db
evaluate db
  (EvalCondFalseIntervalCont lcond2 stc2 e5 jlt3 jlf3 stf1)
  = reducedExchange
      jlf3
      (StateBeforeIntervalFact . StateBeforeInterval jlf3)
      (joinI (narrowConditionalIFalse e5 stc2) stf1)
      db
evaluate db
  (AssignStepIntervalCont lAssign1 x3 e7 stAssign1 lAfter1 stAfter1)
  = reducedExchange
      lAfter1
      (StateBeforeIntervalFact . StateBeforeInterval lAfter1)
      (joinI (insertI x3 (evalI e7 stAssign1) stAssign1) stAfter1)
      db
evaluate db (EvalCondTrueIsEvenCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeIsEvenFact
       (StateBeforeIsEven jlt1 (join (narrowConditional e1 stc1) stt0))]

instance Ord Continuation where
      --   (<=) _ (Initial _) = True
        (<=) _ _ = False


data DataBase = DataBase{factsSeq :: S.HashSet (Natural, Natural),
                         factsVar :: S.HashSet (Natural, String),
                         factsCond ::
                         M.HashMap Natural (S.HashSet (Expr, Natural, Natural)),
                         factsStateBeforeIsEven :: M.HashMap Natural (S.HashSet State),
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr),
                         factsStateBeforeInterval :: M.HashMap Natural (S.HashSet IState)}
                  deriving (Show, Eq)


emptyDB :: DataBase
emptyDB = DataBase S.empty S.empty M.empty M.empty M.empty M.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          SeqFact (Seq v0 v1) -> if S.member (v0, v1) (factsSeq db) then
                                   (db, False) else
                                   (db{factsSeq = S.insert (v0, v1) (factsSeq db)}, True)
          VarFact (Var v0 v1) -> if S.member (v0, v1) (factsVar db) then
                                   (db, False) else
                                   (db{factsVar = S.insert (v0, v1) (factsVar db)}, True)
          CondFact (Cond v0 v1 v2 v3) -> if M.member v0 (factsCond db) then
                                           first
                                             (\ hset ->
                                                db{factsCond = M.insert v0 hset (factsCond db)})
                                             (update ((M.!) (factsCond db) v0) (v1, v2, v3))
                                           else
                                           (db{factsCond =
                                                 M.insert v0 (S.singleton (v1, v2, v3))
                                                   (factsCond db)},
                                            True)
          StateBeforeIsEvenFact (StateBeforeIsEven v0 v1) -> if
                                                               M.member v0
                                                                 (factsStateBeforeIsEven db)
                                                               then
                                                               first
                                                                 (\ hset ->
                                                                    db{factsStateBeforeIsEven =
                                                                         M.insert v0 hset
                                                                           (factsStateBeforeIsEven
                                                                              db)})
                                                                 (update
                                                                    ((M.!)
                                                                       (factsStateBeforeIsEven db)
                                                                       v0)
                                                                    v1)
                                                               else
                                                               (db{factsStateBeforeIsEven =
                                                                     M.insert v0 (S.singleton v1)
                                                                       (factsStateBeforeIsEven db)},
                                                                True)
          AssignFact (Assign v0 v1 v2) -> if
                                            M.member (v0, v1) (factsAssign db) then
                                            first
                                              (\ hset ->
                                                 db{factsAssign =
                                                      M.insert (v0, v1) hset (factsAssign db)})
                                              (update ((M.!) (factsAssign db) (v0, v1)) v2)
                                            else
                                            (db{factsAssign =
                                                  M.insert (v0, v1) (S.singleton v2)
                                                    (factsAssign db)},
                                             True)
          StateBeforeIntervalFact (StateBeforeInterval v0 v1) -> if
                                                                   M.member v0
                                                                     (factsStateBeforeInterval db)
                                                                   then
                                                                   first
                                                                     (\ hset ->
                                                                        db{factsStateBeforeInterval
                                                                             =
                                                                             M.insert v0 hset
                                                                               (factsStateBeforeInterval
                                                                                  db)})
                                                                     (update
                                                                        ((M.!)
                                                                           (factsStateBeforeInterval
                                                                              db)
                                                                           v0)
                                                                        v1)
                                                                   else
                                                                   (db{factsStateBeforeInterval =
                                                                         M.insert v0
                                                                           (S.singleton v1)
                                                                           (factsStateBeforeInterval
                                                                              db)},
                                                                    True)

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_2588
  = case fact of
        StateBeforeIsEvenFact
          f@(StateBeforeIsEven v_2589 v_2590) -> Q.unions
                                                   [q_2588,
                                                    foldl'
                                                      (\ q_2591 f@(StateBeforeIsEven lcond0 stc0) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_2591,
                                                               foldl'
                                                                 (\ q_2593
                                                                    f@(Cond lcond0_2592 e0 jlt0
                                                                         jlf0)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2593,
                                                                          foldl'
                                                                            (\ q_2595
                                                                               f@(StateBeforeIsEven
                                                                                    jlf0_2594 stf0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2595,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondFalseIsEvenCont
                                                                                             lcond0_2592
                                                                                             stc0
                                                                                             e0
                                                                                             jlt0
                                                                                             jlf0_2594
                                                                                             stf0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_2596 vals
                                                                                  ->
                                                                                  concatMap
                                                                                    (\ v_2597 ->
                                                                                       pure
                                                                                         StateBeforeIsEven
                                                                                         <*>
                                                                                         mlbs v_2596
                                                                                           jlf0
                                                                                         <*>
                                                                                         pure
                                                                                           v_2597)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBeforeIsEven
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2598 vals ->
                                                                       concatMap
                                                                         (\ (v_2599, v_2600, v_2601)
                                                                            ->
                                                                            pure Cond <*>
                                                                              mlbs v_2598 lcond0
                                                                              <*> pure v_2599
                                                                              <*> pure v_2600
                                                                              <*> pure v_2601)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsCond db)),
                                                               foldl'
                                                                 (\ q_2603
                                                                    f@(Cond jlf0 e0 jlt0
                                                                         lcond0_2602)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2603,
                                                                          foldl'
                                                                            (\ q_2605
                                                                               f@(StateBeforeIsEven
                                                                                    jlf0_2604 stf0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2605,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondFalseIsEvenCont
                                                                                             jlf0_2604
                                                                                             stf0
                                                                                             e0
                                                                                             jlt0
                                                                                             lcond0_2602
                                                                                             stc0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_2606 vals
                                                                                  ->
                                                                                  concatMap
                                                                                    (\ v_2607 ->
                                                                                       pure
                                                                                         StateBeforeIsEven
                                                                                         <*>
                                                                                         mlbs v_2606
                                                                                           jlf0
                                                                                         <*>
                                                                                         pure
                                                                                           v_2607)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBeforeIsEven
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2608 vals ->
                                                                       concatMap
                                                                         (\ (v_2609, v_2610, v_2611)
                                                                            ->
                                                                            pure Cond <*>
                                                                              pure v_2608
                                                                              <*> pure v_2609
                                                                              <*> pure v_2610
                                                                              <*>
                                                                              mlbs v_2611 lcond0)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsCond db)),
                                                               foldl'
                                                                 (\ q_2613
                                                                    f@(Cond lcond0_2612 e1 jlt1
                                                                         jlf1)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2613,
                                                                          foldl'
                                                                            (\ q_2615
                                                                               f@(StateBeforeIsEven
                                                                                    jlt1_2614 stt0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2615,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondTrueIsEvenCont
                                                                                             lcond0_2612
                                                                                             stc0
                                                                                             e1
                                                                                             jlt1_2614
                                                                                             jlf1
                                                                                             stt0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_2616 vals
                                                                                  ->
                                                                                  concatMap
                                                                                    (\ v_2617 ->
                                                                                       pure
                                                                                         StateBeforeIsEven
                                                                                         <*>
                                                                                         mlbs v_2616
                                                                                           jlt1
                                                                                         <*>
                                                                                         pure
                                                                                           v_2617)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBeforeIsEven
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2618 vals ->
                                                                       concatMap
                                                                         (\ (v_2619, v_2620, v_2621)
                                                                            ->
                                                                            pure Cond <*>
                                                                              mlbs v_2618 lcond0
                                                                              <*> pure v_2619
                                                                              <*> pure v_2620
                                                                              <*> pure v_2621)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsCond db)),
                                                               foldl'
                                                                 (\ q_2623
                                                                    f@(Cond lcond1 e1 lcond0_2622
                                                                         jlf1)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2623,
                                                                          foldl'
                                                                            (\ q_2625
                                                                               f@(StateBeforeIsEven
                                                                                    lcond1_2624
                                                                                    stc1)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2625,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondTrueIsEvenCont
                                                                                             lcond1_2624
                                                                                             stc1
                                                                                             e1
                                                                                             lcond0_2622
                                                                                             jlf1
                                                                                             stc0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_2626 vals
                                                                                  ->
                                                                                  concatMap
                                                                                    (\ v_2627 ->
                                                                                       pure
                                                                                         StateBeforeIsEven
                                                                                         <*>
                                                                                         mlbs v_2626
                                                                                           lcond1
                                                                                         <*>
                                                                                         pure
                                                                                           v_2627)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBeforeIsEven
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2628 vals ->
                                                                       concatMap
                                                                         (\ (v_2629, v_2630, v_2631)
                                                                            ->
                                                                            pure Cond <*>
                                                                              pure v_2628
                                                                              <*> pure v_2629
                                                                              <*> mlbs v_2630 lcond0
                                                                              <*> pure v_2631)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsCond db)),
                                                               foldl'
                                                                 (\ q_2632
                                                                    f@(StateBeforeIsEven lAfter0
                                                                         stAfter0)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2632,
                                                                          foldl'
                                                                            (\ q_2635
                                                                               f@(Seq lcond0_2633
                                                                                    lAfter0_2634)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2635,
                                                                                     foldl'
                                                                                       (\ q_2637
                                                                                          f@(Assign
                                                                                               lcond0_2633_2636
                                                                                               x0
                                                                                               e2)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_2637,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (AssignStepIsEvenCont
                                                                                                        lcond0_2633_2636
                                                                                                        x0
                                                                                                        e2
                                                                                                        stc0
                                                                                                        lAfter0_2634
                                                                                                        stAfter0))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             (v_2638,
                                                                                              v_2639)
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ v_2640
                                                                                                  ->
                                                                                                  pure
                                                                                                    Assign
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_2638
                                                                                                      lcond0_2633
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_2639
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_2640)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsAssign
                                                                                             db))]))
                                                                            Q.empty
                                                                            (foldl'
                                                                               (\ rest
                                                                                  (v_2641, v_2642)
                                                                                  ->
                                                                                  (pure Seq <*>
                                                                                     mlbs v_2641
                                                                                       lcond0
                                                                                     <*>
                                                                                     mlbs v_2642
                                                                                       lAfter0)
                                                                                    ++ rest)
                                                                               []
                                                                               (factsSeq db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2643 vals ->
                                                                       S.foldl'
                                                                         (\ acc v_2644 ->
                                                                            StateBeforeIsEven v_2643
                                                                              v_2644
                                                                              : acc)
                                                                         []
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateBeforeIsEven db)),
                                                               foldl'
                                                                 (\ q_2645
                                                                    f@(StateBeforeIsEven lAssign0
                                                                         stAssign0)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_2645,
                                                                          foldl'
                                                                            (\ q_2648
                                                                               f@(Seq lAssign0_2646
                                                                                    lcond0_2647)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_2648,
                                                                                     foldl'
                                                                                       (\ q_2650
                                                                                          f@(Assign
                                                                                               lAssign0_2646_2649
                                                                                               x0
                                                                                               e2)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_2650,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (AssignStepIsEvenCont
                                                                                                        lAssign0_2646_2649
                                                                                                        x0
                                                                                                        e2
                                                                                                        stAssign0
                                                                                                        lcond0_2647
                                                                                                        stc0))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             (v_2651,
                                                                                              v_2652)
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ v_2653
                                                                                                  ->
                                                                                                  pure
                                                                                                    Assign
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_2651
                                                                                                      lAssign0_2646
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_2652
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_2653)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsAssign
                                                                                             db))]))
                                                                            Q.empty
                                                                            (foldl'
                                                                               (\ rest
                                                                                  (v_2654, v_2655)
                                                                                  ->
                                                                                  (pure Seq <*>
                                                                                     mlbs v_2654
                                                                                       lAssign0
                                                                                     <*>
                                                                                     mlbs v_2655
                                                                                       lcond0)
                                                                                    ++ rest)
                                                                               []
                                                                               (factsSeq db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_2656 vals ->
                                                                       S.foldl'
                                                                         (\ acc v_2657 ->
                                                                            StateBeforeIsEven v_2656
                                                                              v_2657
                                                                              : acc)
                                                                         []
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateBeforeIsEven db))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_2658 vals ->
                                                            S.foldl'
                                                              (\ acc v_2659 ->
                                                                 StateBeforeIsEven v_2658 v_2659 :
                                                                   acc)
                                                              []
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_2589 (S.singleton v_2590)))]
        SeqFact f@(Seq v_2660 v_2661) -> Q.unions
                                           [q_2588,
                                            foldl'
                                              (\ q_2662 f@(Seq lAssign0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_2662,
                                                       foldl'
                                                         (\ q_2664
                                                            f@(StateBeforeIsEven lAssign0_2663
                                                                 stAssign0)
                                                            ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_2664,
                                                                  foldl'
                                                                    (\ q_2666
                                                                       f@(Assign lAssign0_2663_2665
                                                                            x0 e2)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_2666,
                                                                             foldl'
                                                                               (\ q_2668
                                                                                  f@(StateBeforeIsEven
                                                                                       lAfter0_2667
                                                                                       stAfter0)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_2668,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepIsEvenCont
                                                                                                lAssign0_2663_2665
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_2667
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest v_2669
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_2670 ->
                                                                                          pure
                                                                                            StateBeforeIsEven
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_2669
                                                                                              lAfter0
                                                                                            <*>
                                                                                            pure
                                                                                              v_2670)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsStateBeforeIsEven
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest (v_2671, v_2672) vals
                                                                          ->
                                                                          concatMap
                                                                            (\ v_2673 ->
                                                                               pure Assign <*>
                                                                                 mlbs v_2671
                                                                                   lAssign0_2663
                                                                                 <*> pure v_2672
                                                                                 <*> pure v_2673)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsAssign db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_2674 vals ->
                                                               concatMap
                                                                 (\ v_2675 ->
                                                                    pure StateBeforeIsEven <*>
                                                                      mlbs v_2674 lAssign0
                                                                      <*> pure v_2675)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateBeforeIsEven db)),
                                                       foldl'
                                                         (\ q_2677 f@(Assign lAssign0_2676 x3 e7) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_2677,
                                                                  foldl'
                                                                    (\ q_2679
                                                                       f@(StateBeforeInterval
                                                                            lAfter0_2678 stAfter1)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_2679,
                                                                             foldl'
                                                                               (\ q_2681
                                                                                  f@(StateBeforeInterval
                                                                                       lAssign0_2676_2680
                                                                                       stAssign1)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_2681,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepIntervalCont
                                                                                                lAssign0_2676_2680
                                                                                                x3
                                                                                                e7
                                                                                                stAssign1
                                                                                                lAfter0_2678
                                                                                                stAfter1))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest v_2682
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_2683 ->
                                                                                          pure
                                                                                            StateBeforeInterval
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_2682
                                                                                              lAssign0_2676
                                                                                            <*>
                                                                                            pure
                                                                                              v_2683)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsStateBeforeInterval
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_2684 vals ->
                                                                          concatMap
                                                                            (\ v_2685 ->
                                                                               pure
                                                                                 StateBeforeInterval
                                                                                 <*>
                                                                                 mlbs v_2684 lAfter0
                                                                                 <*> pure v_2685)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBeforeInterval
                                                                          db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest (v_2686, v_2687) vals ->
                                                               concatMap
                                                                 (\ v_2688 ->
                                                                    pure Assign <*>
                                                                      mlbs v_2686 lAssign0
                                                                      <*> pure v_2687
                                                                      <*> pure v_2688)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsAssign db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_2689, v_2690) ->
                                                    Seq v_2689 v_2690 : rest)
                                                 []
                                                 (S.singleton (v_2660, v_2661)))]
        CondFact f@(Cond v_2691 v_2692 v_2693 v_2694) -> Q.unions
                                                           [q_2588,
                                                            foldl'
                                                              (\ q_2695 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_2695,
                                                                       foldl'
                                                                         (\ q_2697
                                                                            f@(StateBeforeIsEven
                                                                                 lcond0_2696 stc0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_2697,
                                                                                  foldl'
                                                                                    (\ q_2699
                                                                                       f@(StateBeforeIsEven
                                                                                            jlf0_2698
                                                                                            stf0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_2699,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseIsEvenCont
                                                                                                     lcond0_2696
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_2698
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_2700
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_2701
                                                                                               ->
                                                                                               pure
                                                                                                 StateBeforeIsEven
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_2700
                                                                                                   jlf0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_2701)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBeforeIsEven
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_2702 vals ->
                                                                               concatMap
                                                                                 (\ v_2703 ->
                                                                                    pure
                                                                                      StateBeforeIsEven
                                                                                      <*>
                                                                                      mlbs v_2702
                                                                                        lcond0
                                                                                      <*>
                                                                                      pure v_2703)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBeforeIsEven
                                                                               db)),
                                                                       foldl'
                                                                         (\ q_2705
                                                                            f@(StateBeforeIsEven
                                                                                 lcond0_2704 stc1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_2705,
                                                                                  foldl'
                                                                                    (\ q_2707
                                                                                       f@(StateBeforeIsEven
                                                                                            jlt0_2706
                                                                                            stt0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_2707,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueIsEvenCont
                                                                                                     lcond0_2704
                                                                                                     stc1
                                                                                                     e0
                                                                                                     jlt0_2706
                                                                                                     jlf0
                                                                                                     stt0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_2708
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_2709
                                                                                               ->
                                                                                               pure
                                                                                                 StateBeforeIsEven
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_2708
                                                                                                   jlt0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_2709)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBeforeIsEven
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_2710 vals ->
                                                                               concatMap
                                                                                 (\ v_2711 ->
                                                                                    pure
                                                                                      StateBeforeIsEven
                                                                                      <*>
                                                                                      mlbs v_2710
                                                                                        lcond0
                                                                                      <*>
                                                                                      pure v_2711)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBeforeIsEven
                                                                               db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (CondInitIsEvenCont
                                                                               lcond0
                                                                               e0
                                                                               jlt0
                                                                               jlf0)),
                                                                       foldl'
                                                                         (\ q_2713
                                                                            f@(StateBeforeInterval
                                                                                 jlf0_2712 stf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_2713,
                                                                                  foldl'
                                                                                    (\ q_2715
                                                                                       f@(StateBeforeInterval
                                                                                            lcond0_2714
                                                                                            stc2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_2715,
                                                                                             if
                                                                                               leq
                                                                                                 BFalse
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stc2)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_2715,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseIntervalCont
                                                                                                             lcond0_2714
                                                                                                             stc2
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_2712
                                                                                                             stf1))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_2716
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_2717
                                                                                               ->
                                                                                               pure
                                                                                                 StateBeforeInterval
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_2716
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_2717)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBeforeInterval
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_2718 vals ->
                                                                               concatMap
                                                                                 (\ v_2719 ->
                                                                                    pure
                                                                                      StateBeforeInterval
                                                                                      <*>
                                                                                      mlbs v_2718
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_2719)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBeforeInterval
                                                                               db)),
                                                                       foldl'
                                                                         (\ q_2721
                                                                            f@(StateBeforeInterval
                                                                                 jlt0_2720 stt1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_2721,
                                                                                  foldl'
                                                                                    (\ q_2723
                                                                                       f@(StateBeforeInterval
                                                                                            lcond0_2722
                                                                                            stc3)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_2723,
                                                                                             if
                                                                                               leq
                                                                                                 BTrue
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stc3)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_2723,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueIntervalCont
                                                                                                             lcond0_2722
                                                                                                             stc3
                                                                                                             e0
                                                                                                             jlt0_2720
                                                                                                             jlf0
                                                                                                             stt1))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_2724
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_2725
                                                                                               ->
                                                                                               pure
                                                                                                 StateBeforeInterval
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_2724
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_2725)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBeforeInterval
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_2726 vals ->
                                                                               concatMap
                                                                                 (\ v_2727 ->
                                                                                    pure
                                                                                      StateBeforeInterval
                                                                                      <*>
                                                                                      mlbs v_2726
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_2727)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBeforeInterval
                                                                               db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (CondInitIntervalCont
                                                                               lcond0
                                                                               e0
                                                                               jlt0
                                                                               jlf0))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_2728 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_2729, v_2730, v_2731) ->
                                                                         Cond v_2728 v_2729 v_2730
                                                                           v_2731
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_2691
                                                                    (S.singleton
                                                                       (v_2692, v_2693, v_2694))))]
        AssignFact f@(Assign v_2732 v_2733 v_2734) -> Q.unions
                                                        [q_2588,
                                                         foldl'
                                                           (\ q_2735 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_2735,
                                                                    foldl'
                                                                      (\ q_2737
                                                                         f@(StateBeforeIsEven
                                                                              lAssign0_2736
                                                                              stAssign0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_2737,
                                                                               foldl'
                                                                                 (\ q_2738
                                                                                    f@(StateBeforeIsEven
                                                                                         lAfter0
                                                                                         stAfter0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_2738,
                                                                                          foldl'
                                                                                            (\ q_2741
                                                                                               f@(Seq
                                                                                                    lAssign0_2736_2739
                                                                                                    lAfter0_2740)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_2741,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepIsEvenCont
                                                                                                             lAssign0_2736_2739
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter0_2740
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_2742,
                                                                                                   v_2743)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_2742
                                                                                                       lAssign0_2736
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_2743
                                                                                                       lAfter0)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_2744
                                                                                       vals ->
                                                                                       S.foldl'
                                                                                         (\ acc
                                                                                            v_2745
                                                                                            ->
                                                                                            StateBeforeIsEven
                                                                                              v_2744
                                                                                              v_2745
                                                                                              : acc)
                                                                                         []
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBeforeIsEven
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_2746 vals ->
                                                                            concatMap
                                                                              (\ v_2747 ->
                                                                                 pure
                                                                                   StateBeforeIsEven
                                                                                   <*>
                                                                                   mlbs v_2746
                                                                                     lAssign0
                                                                                   <*> pure v_2747)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBeforeIsEven
                                                                            db)),
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (AssignInitIsEvenCont
                                                                            lAssign0
                                                                            x0
                                                                            e2)),
                                                                    foldl'
                                                                      (\ q_2748
                                                                         f@(StateBeforeInterval
                                                                              lAfter1 stAfter1)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_2748,
                                                                               foldl'
                                                                                 (\ q_2751
                                                                                    f@(Seq
                                                                                         lAssign0_2749
                                                                                         lAfter1_2750)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_2751,
                                                                                          foldl'
                                                                                            (\ q_2753
                                                                                               f@(StateBeforeInterval
                                                                                                    lAssign0_2749_2752
                                                                                                    stAssign1)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_2753,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepIntervalCont
                                                                                                             lAssign0_2749_2752
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign1
                                                                                                             lAfter1_2750
                                                                                                             stAfter1))]))
                                                                                            Q.empty
                                                                                            (M.foldlWithKey'
                                                                                               (\ rest
                                                                                                  v_2754
                                                                                                  vals
                                                                                                  ->
                                                                                                  concatMap
                                                                                                    (\ v_2755
                                                                                                       ->
                                                                                                       pure
                                                                                                         StateBeforeInterval
                                                                                                         <*>
                                                                                                         mlbs
                                                                                                           v_2754
                                                                                                           lAssign0_2749
                                                                                                         <*>
                                                                                                         pure
                                                                                                           v_2755)
                                                                                                    vals
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsStateBeforeInterval
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (foldl'
                                                                                    (\ rest
                                                                                       (v_2756,
                                                                                        v_2757)
                                                                                       ->
                                                                                       (pure Seq <*>
                                                                                          mlbs
                                                                                            v_2756
                                                                                            lAssign0
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_2757
                                                                                            lAfter1)
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsSeq
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_2758 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_2759 ->
                                                                                 StateBeforeInterval
                                                                                   v_2758
                                                                                   v_2759
                                                                                   : acc)
                                                                              []
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBeforeInterval
                                                                            db)),
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (AssignInitIntervalCont
                                                                            lAssign0
                                                                            x0
                                                                            e2))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest (v_2760, v_2761) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_2762 ->
                                                                      Assign v_2760 v_2761 v_2762 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_2732, v_2733)
                                                                 (S.singleton v_2734)))]
        VarFact f@(Var v_2763 v_2764) -> Q.unions
                                           [q_2588,
                                            foldl'
                                              (\ q_2765 f@(Var l0 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_2765,
                                                       Q.singleton
                                                         (traceConclusion
                                                            (VarInitIsEvenCont l0 x1)),
                                                       Q.singleton
                                                         (traceConclusion
                                                            (VarInitIntervalCont l0 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_2766, v_2767) ->
                                                    Var v_2766 v_2767 : rest)
                                                 []
                                                 (S.singleton (v_2763, v_2764)))]
        StateBeforeIntervalFact
          f@(StateBeforeInterval v_2768 v_2769) -> Q.unions
                                                     [q_2588,
                                                      foldl'
                                                        (\ q_2770 f@(StateBeforeInterval jlf3 stf1)
                                                           ->
                                                           trace ("got: " ++ show f)
                                                             (Q.unions
                                                                [q_2770,
                                                                 foldl'
                                                                   (\ q_2772
                                                                      f@(Cond lcond2 e5 jlt3
                                                                           jlf3_2771)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2772,
                                                                            foldl'
                                                                              (\ q_2774
                                                                                 f@(StateBeforeInterval
                                                                                      lcond2_2773
                                                                                      stc2)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2774,
                                                                                       if
                                                                                         leq BFalse
                                                                                           (evaluateConditional
                                                                                              e5
                                                                                              stc2)
                                                                                         then
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_2774,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondFalseIntervalCont
                                                                                                       lcond2_2773
                                                                                                       stc2
                                                                                                       e5
                                                                                                       jlt3
                                                                                                       jlf3_2771
                                                                                                       stf1))])
                                                                                         else
                                                                                         Q.empty]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2775 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ v_2776 ->
                                                                                         pure
                                                                                           StateBeforeInterval
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2775
                                                                                             lcond2
                                                                                           <*>
                                                                                           pure
                                                                                             v_2776)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBeforeInterval
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_2777 vals ->
                                                                         concatMap
                                                                           (\ (v_2778, v_2779,
                                                                               v_2780)
                                                                              ->
                                                                              pure Cond <*>
                                                                                pure v_2777
                                                                                <*> pure v_2778
                                                                                <*> pure v_2779
                                                                                <*>
                                                                                mlbs v_2780 jlf3)
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsCond db)),
                                                                 foldl'
                                                                   (\ q_2782
                                                                      f@(Cond jlf3_2781 e5 jlt3
                                                                           lcond2)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2782,
                                                                            foldl'
                                                                              (\ q_2784
                                                                                 f@(StateBeforeInterval
                                                                                      lcond2_2783
                                                                                      stc2)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2784,
                                                                                       if
                                                                                         leq BFalse
                                                                                           (evaluateConditional
                                                                                              e5
                                                                                              stf1)
                                                                                         then
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_2784,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondFalseIntervalCont
                                                                                                       jlf3_2781
                                                                                                       stf1
                                                                                                       e5
                                                                                                       jlt3
                                                                                                       lcond2_2783
                                                                                                       stc2))])
                                                                                         else
                                                                                         Q.empty]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2785 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ v_2786 ->
                                                                                         pure
                                                                                           StateBeforeInterval
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2785
                                                                                             lcond2
                                                                                           <*>
                                                                                           pure
                                                                                             v_2786)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBeforeInterval
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_2787 vals ->
                                                                         concatMap
                                                                           (\ (v_2788, v_2789,
                                                                               v_2790)
                                                                              ->
                                                                              pure Cond <*>
                                                                                mlbs v_2787 jlf3
                                                                                <*> pure v_2788
                                                                                <*> pure v_2789
                                                                                <*> pure v_2790)
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsCond db)),
                                                                 foldl'
                                                                   (\ q_2791
                                                                      f@(StateBeforeInterval lcond3
                                                                           stc3)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2791,
                                                                            foldl'
                                                                              (\ q_2794
                                                                                 f@(Cond lcond3_2792
                                                                                      e6 jlf3_2793
                                                                                      jlf4)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2794,
                                                                                       if
                                                                                         leq BTrue
                                                                                           (evaluateConditional
                                                                                              e6
                                                                                              stc3)
                                                                                         then
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_2794,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondTrueIntervalCont
                                                                                                       lcond3_2792
                                                                                                       stc3
                                                                                                       e6
                                                                                                       jlf3_2793
                                                                                                       jlf4
                                                                                                       stf1))])
                                                                                         else
                                                                                         Q.empty]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2795 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ (v_2796,
                                                                                          v_2797,
                                                                                          v_2798)
                                                                                         ->
                                                                                         pure Cond
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2795
                                                                                             lcond3
                                                                                           <*>
                                                                                           pure
                                                                                             v_2796
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2797
                                                                                             jlf3
                                                                                           <*>
                                                                                           pure
                                                                                             v_2798)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsCond db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_2799 vals ->
                                                                         S.foldl'
                                                                           (\ acc v_2800 ->
                                                                              StateBeforeInterval
                                                                                v_2799
                                                                                v_2800
                                                                                : acc)
                                                                           []
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsStateBeforeInterval
                                                                         db)),
                                                                 foldl'
                                                                   (\ q_2801
                                                                      f@(StateBeforeInterval jlt4
                                                                           stt1)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2801,
                                                                            foldl'
                                                                              (\ q_2804
                                                                                 f@(Cond jlf3_2802
                                                                                      e6 jlt4_2803
                                                                                      jlf4)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2804,
                                                                                       if
                                                                                         leq BTrue
                                                                                           (evaluateConditional
                                                                                              e6
                                                                                              stf1)
                                                                                         then
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_2804,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondTrueIntervalCont
                                                                                                       jlf3_2802
                                                                                                       stf1
                                                                                                       e6
                                                                                                       jlt4_2803
                                                                                                       jlf4
                                                                                                       stt1))])
                                                                                         else
                                                                                         Q.empty]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2805 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ (v_2806,
                                                                                          v_2807,
                                                                                          v_2808)
                                                                                         ->
                                                                                         pure Cond
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2805
                                                                                             jlf3
                                                                                           <*>
                                                                                           pure
                                                                                             v_2806
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2807
                                                                                             jlt4
                                                                                           <*>
                                                                                           pure
                                                                                             v_2808)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsCond db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_2809 vals ->
                                                                         S.foldl'
                                                                           (\ acc v_2810 ->
                                                                              StateBeforeInterval
                                                                                v_2809
                                                                                v_2810
                                                                                : acc)
                                                                           []
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsStateBeforeInterval
                                                                         db)),
                                                                 foldl'
                                                                   (\ q_2812
                                                                      f@(Assign jlf3_2811 x3 e7) ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2812,
                                                                            foldl'
                                                                              (\ q_2813
                                                                                 f@(StateBeforeInterval
                                                                                      lAfter1
                                                                                      stAfter1)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2813,
                                                                                       foldl'
                                                                                         (\ q_2816
                                                                                            f@(Seq
                                                                                                 jlf3_2811_2814
                                                                                                 lAfter1_2815)
                                                                                            ->
                                                                                            trace
                                                                                              ("got: "
                                                                                                 ++
                                                                                                 show
                                                                                                   f)
                                                                                              (Q.unions
                                                                                                 [q_2816,
                                                                                                  Q.singleton
                                                                                                    (traceConclusion
                                                                                                       (AssignStepIntervalCont
                                                                                                          jlf3_2811_2814
                                                                                                          x3
                                                                                                          e7
                                                                                                          stf1
                                                                                                          lAfter1_2815
                                                                                                          stAfter1))]))
                                                                                         Q.empty
                                                                                         (foldl'
                                                                                            (\ rest
                                                                                               (v_2817,
                                                                                                v_2818)
                                                                                               ->
                                                                                               (pure
                                                                                                  Seq
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_2817
                                                                                                    jlf3_2811
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_2818
                                                                                                    lAfter1)
                                                                                                 ++
                                                                                                 rest)
                                                                                            []
                                                                                            (factsSeq
                                                                                               db))]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2819 vals
                                                                                    ->
                                                                                    S.foldl'
                                                                                      (\ acc v_2820
                                                                                         ->
                                                                                         StateBeforeInterval
                                                                                           v_2819
                                                                                           v_2820
                                                                                           : acc)
                                                                                      []
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBeforeInterval
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest (v_2821, v_2822) vals
                                                                         ->
                                                                         concatMap
                                                                           (\ v_2823 ->
                                                                              pure Assign <*>
                                                                                mlbs v_2821 jlf3
                                                                                <*> pure v_2822
                                                                                <*> pure v_2823)
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsAssign db)),
                                                                 foldl'
                                                                   (\ q_2824
                                                                      f@(Assign lAssign1 x3 e7) ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_2824,
                                                                            foldl'
                                                                              (\ q_2826
                                                                                 f@(StateBeforeInterval
                                                                                      lAssign1_2825
                                                                                      stAssign1)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_2826,
                                                                                       foldl'
                                                                                         (\ q_2829
                                                                                            f@(Seq
                                                                                                 lAssign1_2825_2827
                                                                                                 jlf3_2828)
                                                                                            ->
                                                                                            trace
                                                                                              ("got: "
                                                                                                 ++
                                                                                                 show
                                                                                                   f)
                                                                                              (Q.unions
                                                                                                 [q_2829,
                                                                                                  Q.singleton
                                                                                                    (traceConclusion
                                                                                                       (AssignStepIntervalCont
                                                                                                          lAssign1_2825_2827
                                                                                                          x3
                                                                                                          e7
                                                                                                          stAssign1
                                                                                                          jlf3_2828
                                                                                                          stf1))]))
                                                                                         Q.empty
                                                                                         (foldl'
                                                                                            (\ rest
                                                                                               (v_2830,
                                                                                                v_2831)
                                                                                               ->
                                                                                               (pure
                                                                                                  Seq
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_2830
                                                                                                    lAssign1_2825
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_2831
                                                                                                    jlf3)
                                                                                                 ++
                                                                                                 rest)
                                                                                            []
                                                                                            (factsSeq
                                                                                               db))]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_2832 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ v_2833 ->
                                                                                         pure
                                                                                           StateBeforeInterval
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_2832
                                                                                             lAssign1
                                                                                           <*>
                                                                                           pure
                                                                                             v_2833)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBeforeInterval
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest (v_2834, v_2835) vals
                                                                         ->
                                                                         S.foldl'
                                                                           (\ acc v_2836 ->
                                                                              Assign v_2834 v_2835
                                                                                v_2836
                                                                                : acc)
                                                                           []
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsAssign db))]))
                                                        Q.empty
                                                        (M.foldlWithKey'
                                                           (\ rest v_2837 vals ->
                                                              S.foldl'
                                                                (\ acc v_2838 ->
                                                                   StateBeforeInterval v_2837 v_2838
                                                                     : acc)
                                                                []
                                                                vals
                                                                ++ rest)
                                                           []
                                                           (M.singleton v_2768
                                                              (S.singleton v_2769)))]

stateBeforeIsEven :: Natural -> DataBase -> [StateBeforeIsEven]
stateBeforeIsEven v_2839 db
  = M.foldlWithKey'
      (\ rest v_2841 vals ->
         concatMap
           (\ v_2842 ->
              pure StateBeforeIsEven <*> mlbs v_2841 v_2839 <*> pure v_2842)
           vals
           ++ rest)
      []
      (factsStateBeforeIsEven db)

stateBeforeInterval :: DataBase -> [StateBeforeInterval]
stateBeforeInterval db
  = M.foldlWithKey'
      (\ rest v_2844 vals ->
         S.foldl' (\ acc v_2845 -> StateBeforeInterval v_2844 v_2845 : acc)
           []
           vals
           ++ rest)
      []
      (factsStateBeforeInterval db)

traceConclusion :: Show a => a -> a
traceConclusion c = trace ("concluded: " ++ show c) c

tracePopped :: Show a => a -> a
tracePopped a = trace ("popped: " ++ show a) a

compute :: [Fact] -> DataBase
compute = go emptyDB . Q.fromList . map Initial
  where {-# NOINLINE go #-}
        
        go :: DataBase -> Queue -> DataBase
        go db pq
          | Q.null pq = db
          | otherwise =
            let (nextFacts, pq')
                  = first (evaluate db . tracePopped) $ Q.deleteFindMax pq
                (db', pq'')
                  = foldl'
                      (\ (dbOld, pqOld) f ->
                         let (dbNew, changed) = insertDB f dbOld
                             pqNew
                               = if changed then step dbNew f pqOld else
                                   trace ("dropped: " ++ show f) pqOld
                           in (dbNew, pqNew))
                      (db, pq')
                      nextFacts
                respondCmds
                  = do ln <- getLine
                       case ln of
                           "db" -> print db'
                           "pq" -> print $ vsep (pretty . show <$> Q.toDescList pq'')
                           "" -> return ()
                           _ -> putStrLn "unknown command"
                       if null ln then return $ go db' pq'' else respondCmds
              in unsafePerformIO respondCmds



----------------------- HANDWRITTEN CODE----------------------------------


reducedExchange :: Natural -> (IState -> f) -> IState -> DataBase -> [f]
reducedExchange programPoint k s db = [k (MP.fromList (map enrich (MP.toList s)))]
  where
    isEvenForThisPointSet = M.lookupDefault S.empty programPoint (factsStateBeforeIsEven db)
    enrich :: (String, Interval) -> (String, Interval)
    enrich (name, interval) =
      case evenness of
         IsEven -> case interval of
            Pair (lo, hi) -> (name, Pair (if odd lo then lo + 1 else lo, if odd hi then hi - 1 else hi))
            _ -> (name, interval)
         IsOdd -> case interval of
            Pair (lo, hi) -> (name, Pair (if even lo then lo + 1 else lo, if even hi then hi - 1 else hi))
            _ -> (name, interval)
         _ -> (name, interval)
      where
         evennessState = head (((S.toList isEvenForThisPointSet) ++ [MP.fromList [(name, EvenBot)]]))
         evenness = case MP.lookup name evennessState of
            Nothing -> EvenBot
            Just otherwise -> otherwise

     
