{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.IsEvenAnalysis where
import ReducedProduct.IsEven
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

subsumes :: (PartialOrd a) => a -> a -> Bool
subsumes = flip leq

strictlySubsumes :: (PartialOrd a) => a -> a -> Bool
strictlySubsumes y x = leq x y && not (leq y x)

data Seq = Seq Natural Natural
             deriving (Eq, Show, Generic)

instance Hashable Seq

instance PartialOrd Seq where
        leq (Seq v0 v1) (Seq v0' v1') = (v0 `leq` v0') && (v1 `leq` v1')
mkSeq v0 v1 = SeqFact (Seq v0 v1)

data Phi = Phi Natural
             deriving (Eq, Show, Generic)

instance Hashable Phi

instance PartialOrd Phi where
        leq (Phi v0) (Phi v0') = (v0 `leq` v0')
mkPhi v0 = PhiFact (Phi v0)

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

data StateBefore = StateBefore Natural State
                     deriving (Eq, Show, Generic)

instance Hashable StateBefore

instance PartialOrd StateBefore where
        leq (StateBefore v0 v1) (StateBefore v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkStateBefore v0 v1 = StateBeforeFact (StateBefore v0 v1)

data Fact = SeqFact Seq
          | PhiFact Phi
          | CondFact Cond
          | AssignFact Assign
          | VarFact Var
          | StateBeforeFact StateBefore
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | AssignInitCont Natural String Expr
                  | AssignStepCont Natural String Expr State Natural State
                  | EvalCondFalseCont Natural State Expr Natural Natural State
                  | PhiStepCont Natural State Natural
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | PhiInitCont Natural
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l4 x2 e4)
  = [StateBeforeFact (StateBefore l4 (empty 0))]
evaluate db
  (AssignStepCont lAssign0 x0 e2 stAssign0 lAfter1 stAfter0)
  = [StateBeforeFact
       (StateBefore lAfter1
          (join (insert x0 (eval e2 stAssign0) stAssign0) stAfter0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db (PhiStepCont l0 st0 lAfter0)
  = [StateBeforeFact (StateBefore lAfter0 st0)]
evaluate db (CondInitCont l3 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l3 (empty 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (join (narrowConditional e1 stc1) stt0))]
evaluate db (PhiInitCont l2)
  = [StateBeforeFact (StateBefore l2 (empty 0))]
evaluate db (VarInitCont l1 x1)
  = [StateBeforeFact (StateBefore l1 (empty 0))]

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsStateBefore ::
                         M.HashMap Natural (S.HashSet State),
                         factsSeq :: S.HashSet (Natural, Natural),
                         factsVar :: S.HashSet (Natural, String),
                         factsCond ::
                         M.HashMap Natural (S.HashSet (Expr, Natural, Natural)),
                         factsPhi :: S.HashSet Natural,
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr)}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase M.empty S.empty S.empty M.empty S.empty M.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          StateBeforeFact (StateBefore v0 v1) -> if
                                                   M.member v0 (factsStateBefore db) then
                                                   first
                                                     (\ hset ->
                                                        db{factsStateBefore =
                                                             M.insert v0 hset
                                                               (factsStateBefore db)})
                                                     (update ((M.!) (factsStateBefore db) v0) v1)
                                                   else
                                                   (db{factsStateBefore =
                                                         M.insert v0 (S.singleton v1)
                                                           (factsStateBefore db)},
                                                    True)
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
          PhiFact (Phi v0) -> if S.member v0 (factsPhi db) then (db, False)
                                else (db{factsPhi = S.insert v0 (factsPhi db)}, True)
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

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_502
  = case fact of
        SeqFact f@(Seq v_503 v_504) -> Q.unions
                                         [q_502,
                                          foldl'
                                            (\ q_505 f@(Seq l0 lAfter0) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_505,
                                                     foldl'
                                                       (\ q_507 f@(Phi l0_506) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_507,
                                                                foldl'
                                                                  (\ q_509
                                                                     f@(StateBefore l0_506_508 st0)
                                                                     ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_509,
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (PhiStepCont
                                                                                   l0_506_508
                                                                                   st0
                                                                                   lAfter0))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_510 vals ->
                                                                        concatMap
                                                                          (\ v_511 ->
                                                                             pure StateBefore <*>
                                                                               mlbs v_510 l0_506
                                                                               <*> pure v_511)
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (factsStateBefore db))]))
                                                       Q.empty
                                                       (foldl'
                                                          (\ rest v_512 ->
                                                             (pure Phi <*> mlbs v_512 l0) ++ rest)
                                                          []
                                                          (factsPhi db)),
                                                     foldl'
                                                       (\ q_514 f@(Assign l0_513 x0 e2) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_514,
                                                                foldl'
                                                                  (\ q_516
                                                                     f@(StateBefore lAfter0_515
                                                                          stAfter0)
                                                                     ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_516,
                                                                           foldl'
                                                                             (\ q_518
                                                                                f@(StateBefore
                                                                                     l0_513_517
                                                                                     stAssign0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_518,
                                                                                      Q.singleton
                                                                                        (traceConclusion
                                                                                           (AssignStepCont
                                                                                              l0_513_517
                                                                                              x0
                                                                                              e2
                                                                                              stAssign0
                                                                                              lAfter0_515
                                                                                              stAfter0))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_519 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_520 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs v_519
                                                                                            l0_513
                                                                                          <*>
                                                                                          pure
                                                                                            v_520)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_521 vals ->
                                                                        concatMap
                                                                          (\ v_522 ->
                                                                             pure StateBefore <*>
                                                                               mlbs v_521 lAfter0
                                                                               <*> pure v_522)
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (factsStateBefore db))]))
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest (v_523, v_524) vals ->
                                                             concatMap
                                                               (\ v_525 ->
                                                                  pure Assign <*> mlbs v_523 l0 <*>
                                                                    pure v_524
                                                                    <*> pure v_525)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsAssign db))]))
                                            Q.empty
                                            (S.foldl'
                                               (\ rest (v_526, v_527) -> Seq v_526 v_527 : rest)
                                               []
                                               (S.singleton (v_503, v_504)))]
        PhiFact f@(Phi v_528) -> Q.unions
                                   [q_502,
                                    foldl'
                                      (\ q_529 f@(Phi l0) ->
                                         trace ("got: " ++ show f)
                                           (Q.unions
                                              [q_529,
                                               foldl'
                                                 (\ q_531 f@(Seq l0_530 lAfter0) ->
                                                    trace ("got: " ++ show f)
                                                      (Q.unions
                                                         [q_531,
                                                          foldl'
                                                            (\ q_533 f@(StateBefore l0_530_532 st0)
                                                               ->
                                                               trace ("got: " ++ show f)
                                                                 (Q.unions
                                                                    [q_533,
                                                                     Q.singleton
                                                                       (traceConclusion
                                                                          (PhiStepCont l0_530_532
                                                                             st0
                                                                             lAfter0))]))
                                                            Q.empty
                                                            (M.foldlWithKey'
                                                               (\ rest v_534 vals ->
                                                                  concatMap
                                                                    (\ v_535 ->
                                                                       pure StateBefore <*>
                                                                         mlbs v_534 l0_530
                                                                         <*> pure v_535)
                                                                    vals
                                                                    ++ rest)
                                                               []
                                                               (factsStateBefore db))]))
                                                 Q.empty
                                                 (foldl'
                                                    (\ rest (v_536, v_537) ->
                                                       (pure Seq <*> mlbs v_536 l0 <*> pure v_537)
                                                         ++ rest)
                                                    []
                                                    (factsSeq db)),
                                               Q.singleton (traceConclusion (PhiInitCont l0))]))
                                      Q.empty
                                      (S.foldl' (\ rest v_538 -> Phi v_538 : rest) []
                                         (S.singleton v_528))]
        CondFact f@(Cond v_539 v_540 v_541 v_542) -> Q.unions
                                                       [q_502,
                                                        foldl'
                                                          (\ q_543 f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_543,
                                                                   foldl'
                                                                     (\ q_545
                                                                        f@(StateBefore jlf0_544
                                                                             stf0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_545,
                                                                              foldl'
                                                                                (\ q_547
                                                                                   f@(StateBefore
                                                                                        lcond0_546
                                                                                        stc0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_547,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondFalseCont
                                                                                                 lcond0_546
                                                                                                 stc0
                                                                                                 e0
                                                                                                 jlt0
                                                                                                 jlf0_544
                                                                                                 stf0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_548
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_549 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_548
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_549)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_550 vals ->
                                                                           concatMap
                                                                             (\ v_551 ->
                                                                                pure StateBefore <*>
                                                                                  mlbs v_550 jlf0
                                                                                  <*> pure v_551)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_553
                                                                        f@(StateBefore jlt0_552
                                                                             stt0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_553,
                                                                              foldl'
                                                                                (\ q_555
                                                                                   f@(StateBefore
                                                                                        lcond0_554
                                                                                        stc1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_555,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondTrueCont
                                                                                                 lcond0_554
                                                                                                 stc1
                                                                                                 e0
                                                                                                 jlt0_552
                                                                                                 jlf0
                                                                                                 stt0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_556
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_557 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_556
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_557)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_558 vals ->
                                                                           concatMap
                                                                             (\ v_559 ->
                                                                                pure StateBefore <*>
                                                                                  mlbs v_558 jlt0
                                                                                  <*> pure v_559)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   Q.singleton
                                                                     (traceConclusion
                                                                        (CondInitCont lcond0 e0 jlt0
                                                                           jlf0))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_560 vals ->
                                                                S.foldl'
                                                                  (\ acc (v_561, v_562, v_563) ->
                                                                     Cond v_560 v_561 v_562 v_563 :
                                                                       acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (M.singleton v_539
                                                                (S.singleton
                                                                   (v_540, v_541, v_542))))]
        AssignFact f@(Assign v_564 v_565 v_566) -> Q.unions
                                                     [q_502,
                                                      foldl'
                                                        (\ q_567 f@(Assign lAssign0 x0 e2) ->
                                                           trace ("got: " ++ show f)
                                                             (Q.unions
                                                                [q_567,
                                                                 foldl'
                                                                   (\ q_568
                                                                      f@(StateBefore lAfter1
                                                                           stAfter0)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_568,
                                                                            foldl'
                                                                              (\ q_570
                                                                                 f@(StateBefore
                                                                                      lAssign0_569
                                                                                      stAssign0)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_570,
                                                                                       foldl'
                                                                                         (\ q_573
                                                                                            f@(Seq
                                                                                                 lAssign0_569_571
                                                                                                 lAfter1_572)
                                                                                            ->
                                                                                            trace
                                                                                              ("got: "
                                                                                                 ++
                                                                                                 show
                                                                                                   f)
                                                                                              (Q.unions
                                                                                                 [q_573,
                                                                                                  Q.singleton
                                                                                                    (traceConclusion
                                                                                                       (AssignStepCont
                                                                                                          lAssign0_569_571
                                                                                                          x0
                                                                                                          e2
                                                                                                          stAssign0
                                                                                                          lAfter1_572
                                                                                                          stAfter0))]))
                                                                                         Q.empty
                                                                                         (foldl'
                                                                                            (\ rest
                                                                                               (v_574,
                                                                                                v_575)
                                                                                               ->
                                                                                               (pure
                                                                                                  Seq
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_574
                                                                                                    lAssign0_569
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_575
                                                                                                    lAfter1)
                                                                                                 ++
                                                                                                 rest)
                                                                                            []
                                                                                            (factsSeq
                                                                                               db))]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_576 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ v_577 ->
                                                                                         pure
                                                                                           StateBefore
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_576
                                                                                             lAssign0
                                                                                           <*>
                                                                                           pure
                                                                                             v_577)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBefore
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_578 vals ->
                                                                         S.foldl'
                                                                           (\ acc v_579 ->
                                                                              StateBefore v_578
                                                                                v_579
                                                                                : acc)
                                                                           []
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsStateBefore db)),
                                                                 Q.singleton
                                                                   (traceConclusion
                                                                      (AssignInitCont lAssign0 x0
                                                                         e2))]))
                                                        Q.empty
                                                        (M.foldlWithKey'
                                                           (\ rest (v_580, v_581) vals ->
                                                              S.foldl'
                                                                (\ acc v_582 ->
                                                                   Assign v_580 v_581 v_582 : acc)
                                                                []
                                                                vals
                                                                ++ rest)
                                                           []
                                                           (M.singleton (v_564, v_565)
                                                              (S.singleton v_566)))]
        VarFact f@(Var v_583 v_584) -> Q.unions
                                         [q_502,
                                          foldl'
                                            (\ q_585 f@(Var l1 x1) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_585,
                                                     Q.singleton
                                                       (traceConclusion (VarInitCont l1 x1))]))
                                            Q.empty
                                            (S.foldl'
                                               (\ rest (v_586, v_587) -> Var v_586 v_587 : rest)
                                               []
                                               (S.singleton (v_583, v_584)))]
        StateBeforeFact f@(StateBefore v_588 v_589) -> Q.unions
                                                         [q_502,
                                                          foldl'
                                                            (\ q_590 f@(StateBefore l0 st0) ->
                                                               trace ("got: " ++ show f)
                                                                 (Q.unions
                                                                    [q_590,
                                                                     foldl'
                                                                       (\ q_592
                                                                          f@(Seq l0_591 lAfter0) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_592,
                                                                                foldl'
                                                                                  (\ q_594
                                                                                     f@(Phi
                                                                                          l0_591_593)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_594,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (PhiStepCont
                                                                                                   l0_591_593
                                                                                                   st0
                                                                                                   lAfter0))]))
                                                                                  Q.empty
                                                                                  (foldl'
                                                                                     (\ rest v_595
                                                                                        ->
                                                                                        (pure Phi
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_595
                                                                                             l0_591)
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsPhi
                                                                                        db))]))
                                                                       Q.empty
                                                                       (foldl'
                                                                          (\ rest (v_596, v_597) ->
                                                                             (pure Seq <*>
                                                                                mlbs v_596 l0
                                                                                <*> pure v_597)
                                                                               ++ rest)
                                                                          []
                                                                          (factsSeq db)),
                                                                     foldl'
                                                                       (\ q_599
                                                                          f@(Cond lcond0 e0 jlt0
                                                                               l0_598)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_599,
                                                                                foldl'
                                                                                  (\ q_601
                                                                                     f@(StateBefore
                                                                                          lcond0_600
                                                                                          stc0)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_601,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondFalseCont
                                                                                                   lcond0_600
                                                                                                   stc0
                                                                                                   e0
                                                                                                   jlt0
                                                                                                   l0_598
                                                                                                   st0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_602
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_603
                                                                                             ->
                                                                                             pure
                                                                                               StateBefore
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_602
                                                                                                 lcond0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_603)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateBefore
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_604 vals ->
                                                                             concatMap
                                                                               (\ (v_605, v_606,
                                                                                   v_607)
                                                                                  ->
                                                                                  pure Cond <*>
                                                                                    pure v_604
                                                                                    <*> pure v_605
                                                                                    <*> pure v_606
                                                                                    <*>
                                                                                    mlbs v_607 l0)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_608
                                                                          f@(StateBefore jlf0 stf0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_608,
                                                                                foldl'
                                                                                  (\ q_611
                                                                                     f@(Cond l0_609
                                                                                          e0 jlt0
                                                                                          jlf0_610)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_611,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondFalseCont
                                                                                                   l0_609
                                                                                                   st0
                                                                                                   e0
                                                                                                   jlt0
                                                                                                   jlf0_610
                                                                                                   stf0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_612
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_613,
                                                                                              v_614,
                                                                                              v_615)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_612
                                                                                                 l0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_613
                                                                                               <*>
                                                                                               pure
                                                                                                 v_614
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_615
                                                                                                 jlf0)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_616 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_617 ->
                                                                                  StateBefore v_616
                                                                                    v_617
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db)),
                                                                     foldl'
                                                                       (\ q_619
                                                                          f@(Cond lcond1 e1 l0_618
                                                                               jlf1)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_619,
                                                                                foldl'
                                                                                  (\ q_621
                                                                                     f@(StateBefore
                                                                                          lcond1_620
                                                                                          stc1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_621,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondTrueCont
                                                                                                   lcond1_620
                                                                                                   stc1
                                                                                                   e1
                                                                                                   l0_618
                                                                                                   jlf1
                                                                                                   st0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_622
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_623
                                                                                             ->
                                                                                             pure
                                                                                               StateBefore
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_622
                                                                                                 lcond1
                                                                                               <*>
                                                                                               pure
                                                                                                 v_623)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateBefore
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_624 vals ->
                                                                             concatMap
                                                                               (\ (v_625, v_626,
                                                                                   v_627)
                                                                                  ->
                                                                                  pure Cond <*>
                                                                                    pure v_624
                                                                                    <*> pure v_625
                                                                                    <*>
                                                                                    mlbs v_626 l0
                                                                                    <*> pure v_627)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_628
                                                                          f@(StateBefore jlt1 stt0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_628,
                                                                                foldl'
                                                                                  (\ q_631
                                                                                     f@(Cond l0_629
                                                                                          e1
                                                                                          jlt1_630
                                                                                          jlf1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_631,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondTrueCont
                                                                                                   l0_629
                                                                                                   st0
                                                                                                   e1
                                                                                                   jlt1_630
                                                                                                   jlf1
                                                                                                   stt0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_632
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_633,
                                                                                              v_634,
                                                                                              v_635)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_632
                                                                                                 l0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_633
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_634
                                                                                                 jlt1
                                                                                               <*>
                                                                                               pure
                                                                                                 v_635)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_636 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_637 ->
                                                                                  StateBefore v_636
                                                                                    v_637
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db)),
                                                                     foldl'
                                                                       (\ q_638
                                                                          f@(StateBefore lAssign0
                                                                               stAssign0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_638,
                                                                                foldl'
                                                                                  (\ q_640
                                                                                     f@(Assign
                                                                                          lAssign0_639
                                                                                          x0 e2)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_640,
                                                                                           foldl'
                                                                                             (\ q_643
                                                                                                f@(Seq
                                                                                                     lAssign0_639_641
                                                                                                     l0_642)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_643,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (AssignStepCont
                                                                                                              lAssign0_639_641
                                                                                                              x0
                                                                                                              e2
                                                                                                              stAssign0
                                                                                                              l0_642
                                                                                                              st0))]))
                                                                                             Q.empty
                                                                                             (foldl'
                                                                                                (\ rest
                                                                                                   (v_644,
                                                                                                    v_645)
                                                                                                   ->
                                                                                                   (pure
                                                                                                      Seq
                                                                                                      <*>
                                                                                                      mlbs
                                                                                                        v_644
                                                                                                        lAssign0_639
                                                                                                      <*>
                                                                                                      mlbs
                                                                                                        v_645
                                                                                                        l0)
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsSeq
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest
                                                                                        (v_646,
                                                                                         v_647)
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_648
                                                                                             ->
                                                                                             pure
                                                                                               Assign
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_646
                                                                                                 lAssign0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_647
                                                                                               <*>
                                                                                               pure
                                                                                                 v_648)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsAssign
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_649 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_650 ->
                                                                                  StateBefore v_649
                                                                                    v_650
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db)),
                                                                     foldl'
                                                                       (\ q_652
                                                                          f@(Assign l0_651 x0 e2) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_652,
                                                                                foldl'
                                                                                  (\ q_654
                                                                                     f@(Seq
                                                                                          l0_651_653
                                                                                          lAfter1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_654,
                                                                                           foldl'
                                                                                             (\ q_656
                                                                                                f@(StateBefore
                                                                                                     lAfter1_655
                                                                                                     stAfter0)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_656,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (AssignStepCont
                                                                                                              l0_651_653
                                                                                                              x0
                                                                                                              e2
                                                                                                              st0
                                                                                                              lAfter1_655
                                                                                                              stAfter0))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   v_657
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ v_658
                                                                                                        ->
                                                                                                        pure
                                                                                                          StateBefore
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_657
                                                                                                            lAfter1
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_658)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsStateBefore
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (foldl'
                                                                                     (\ rest
                                                                                        (v_659,
                                                                                         v_660)
                                                                                        ->
                                                                                        (pure Seq
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_659
                                                                                             l0_651
                                                                                           <*>
                                                                                           pure
                                                                                             v_660)
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsSeq
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest (v_661, v_662)
                                                                             vals ->
                                                                             concatMap
                                                                               (\ v_663 ->
                                                                                  pure Assign <*>
                                                                                    mlbs v_661 l0
                                                                                    <*> pure v_662
                                                                                    <*> pure v_663)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsAssign db))]))
                                                            Q.empty
                                                            (M.foldlWithKey'
                                                               (\ rest v_664 vals ->
                                                                  S.foldl'
                                                                    (\ acc v_665 ->
                                                                       StateBefore v_664 v_665 :
                                                                         acc)
                                                                    []
                                                                    vals
                                                                    ++ rest)
                                                               []
                                                               (M.singleton v_588
                                                                  (S.singleton v_589)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_667 vals ->
         S.foldl' (\ acc v_668 -> StateBefore v_667 v_668 : acc) [] vals ++
           rest)
      []
      (factsStateBefore db)

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