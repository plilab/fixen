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
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l2 x2 e4)
  = [StateBeforeFact (StateBefore l2 (empty 0))]
evaluate db
  (AssignStepCont lAssign0 x0 e2 stAssign0 lAfter0 stAfter0)
  = [StateBeforeFact
       (StateBefore lAfter0
          (join (insert x0 (eval e2 stAssign0) stAssign0) stAfter0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db (CondInitCont l1 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l1 (empty 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (join (narrowConditional e1 stc1) stt0))]
evaluate db (VarInitCont l0 x1)
  = [StateBeforeFact (StateBefore l0 (empty 0))]

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
step db fact q_1504
  = case fact of
        SeqFact f@(Seq v_1505 v_1506) -> Q.unions
                                           [q_1504,
                                            foldl'
                                              (\ q_1507 f@(Seq lAssign0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1507,
                                                       foldl'
                                                         (\ q_1509
                                                            f@(StateBefore lAfter0_1508 stAfter0) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1509,
                                                                  foldl'
                                                                    (\ q_1511
                                                                       f@(StateBefore lAssign0_1510
                                                                            stAssign0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1511,
                                                                             foldl'
                                                                               (\ q_1513
                                                                                  f@(Assign
                                                                                       lAssign0_1510_1512
                                                                                       x0 e2)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_1513,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                lAssign0_1510_1512
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_1508
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest
                                                                                     (v_1514,
                                                                                      v_1515)
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_1516 ->
                                                                                          pure
                                                                                            Assign
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_1514
                                                                                              lAssign0_1510
                                                                                            <*>
                                                                                            pure
                                                                                              v_1515
                                                                                            <*>
                                                                                            pure
                                                                                              v_1516)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsAssign
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1517 vals ->
                                                                          concatMap
                                                                            (\ v_1518 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1517
                                                                                   lAssign0
                                                                                 <*> pure v_1518)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_1519 vals ->
                                                               concatMap
                                                                 (\ v_1520 ->
                                                                    pure StateBefore <*>
                                                                      mlbs v_1519 lAfter0
                                                                      <*> pure v_1520)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateBefore db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1521, v_1522) ->
                                                    Seq v_1521 v_1522 : rest)
                                                 []
                                                 (S.singleton (v_1505, v_1506)))]
        CondFact f@(Cond v_1523 v_1524 v_1525 v_1526) -> Q.unions
                                                           [q_1504,
                                                            foldl'
                                                              (\ q_1527 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1527,
                                                                       foldl'
                                                                         (\ q_1529
                                                                            f@(StateBefore jlf0_1528
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1529,
                                                                                  foldl'
                                                                                    (\ q_1531
                                                                                       f@(StateBefore
                                                                                            lcond0_1530
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1531,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     lcond0_1530
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_1528
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1532
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1533
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1532
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1533)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1534 vals ->
                                                                               concatMap
                                                                                 (\ v_1535 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1534
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1535)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1537
                                                                            f@(StateBefore jlt0_1536
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1537,
                                                                                  foldl'
                                                                                    (\ q_1539
                                                                                       f@(StateBefore
                                                                                            lcond0_1538
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1539,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     lcond0_1538
                                                                                                     stc1
                                                                                                     e0
                                                                                                     jlt0_1536
                                                                                                     jlf0
                                                                                                     stt0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1540
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1541
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1540
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1541)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1542 vals ->
                                                                               concatMap
                                                                                 (\ v_1543 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1542
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_1543)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (CondInitCont lcond0 e0
                                                                               jlt0
                                                                               jlf0))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1544 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1545, v_1546, v_1547) ->
                                                                         Cond v_1544 v_1545 v_1546
                                                                           v_1547
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1523
                                                                    (S.singleton
                                                                       (v_1524, v_1525, v_1526))))]
        AssignFact f@(Assign v_1548 v_1549 v_1550) -> Q.unions
                                                        [q_1504,
                                                         foldl'
                                                           (\ q_1551 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1551,
                                                                    foldl'
                                                                      (\ q_1552
                                                                         f@(StateBefore lAfter0
                                                                              stAfter0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1552,
                                                                               foldl'
                                                                                 (\ q_1554
                                                                                    f@(StateBefore
                                                                                         lAssign0_1553
                                                                                         stAssign0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_1554,
                                                                                          foldl'
                                                                                            (\ q_1557
                                                                                               f@(Seq
                                                                                                    lAssign0_1553_1555
                                                                                                    lAfter0_1556)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1557,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_1553_1555
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter0_1556
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_1558,
                                                                                                   v_1559)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1558
                                                                                                       lAssign0_1553
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1559
                                                                                                       lAfter0)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_1560
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_1561
                                                                                            ->
                                                                                            pure
                                                                                              StateBefore
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_1560
                                                                                                lAssign0
                                                                                              <*>
                                                                                              pure
                                                                                                v_1561)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1562 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_1563 ->
                                                                                 StateBefore v_1562
                                                                                   v_1563
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
                                                              (\ rest (v_1564, v_1565) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1566 ->
                                                                      Assign v_1564 v_1565 v_1566 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1548, v_1549)
                                                                 (S.singleton v_1550)))]
        VarFact f@(Var v_1567 v_1568) -> Q.unions
                                           [q_1504,
                                            foldl'
                                              (\ q_1569 f@(Var l0 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1569,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l0 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1570, v_1571) ->
                                                    Var v_1570 v_1571 : rest)
                                                 []
                                                 (S.singleton (v_1567, v_1568)))]
        StateBeforeFact f@(StateBefore v_1572 v_1573) -> Q.unions
                                                           [q_1504,
                                                            foldl'
                                                              (\ q_1574 f@(StateBefore jlf0 stf0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1574,
                                                                       foldl'
                                                                         (\ q_1576
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 jlf0_1575)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1576,
                                                                                  foldl'
                                                                                    (\ q_1578
                                                                                       f@(StateBefore
                                                                                            lcond0_1577
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1578,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     lcond0_1577
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_1575
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1579
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1580
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1579
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1580)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1581 vals ->
                                                                               concatMap
                                                                                 (\ (v_1582, v_1583,
                                                                                     v_1584)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1581
                                                                                      <*>
                                                                                      pure v_1582
                                                                                      <*>
                                                                                      pure v_1583
                                                                                      <*>
                                                                                      mlbs v_1584
                                                                                        jlf0)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1586
                                                                            f@(Cond jlf0_1585 e0
                                                                                 jlt0 lcond0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1586,
                                                                                  foldl'
                                                                                    (\ q_1588
                                                                                       f@(StateBefore
                                                                                            lcond0_1587
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1588,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     jlf0_1585
                                                                                                     stf0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     lcond0_1587
                                                                                                     stc0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1589
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1590
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1589
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1590)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1591 vals ->
                                                                               concatMap
                                                                                 (\ (v_1592, v_1593,
                                                                                     v_1594)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      mlbs v_1591
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1592
                                                                                      <*>
                                                                                      pure v_1593
                                                                                      <*>
                                                                                      pure v_1594)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1596
                                                                            f@(Cond lcond1 e1
                                                                                 jlf0_1595 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1596,
                                                                                  foldl'
                                                                                    (\ q_1598
                                                                                       f@(StateBefore
                                                                                            lcond1_1597
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1598,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     lcond1_1597
                                                                                                     stc1
                                                                                                     e1
                                                                                                     jlf0_1595
                                                                                                     jlf1
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1599
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1600
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1599
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1600)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1601 vals ->
                                                                               concatMap
                                                                                 (\ (v_1602, v_1603,
                                                                                     v_1604)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1601
                                                                                      <*>
                                                                                      pure v_1602
                                                                                      <*>
                                                                                      mlbs v_1603
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1604)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1605
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1605,
                                                                                  foldl'
                                                                                    (\ q_1608
                                                                                       f@(Cond
                                                                                            jlf0_1606
                                                                                            e1
                                                                                            jlt1_1607
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1608,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     jlf0_1606
                                                                                                     stf0
                                                                                                     e1
                                                                                                     jlt1_1607
                                                                                                     jlf1
                                                                                                     stt0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1609
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1610,
                                                                                                v_1611,
                                                                                                v_1612)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1609
                                                                                                   jlf0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1610
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1611
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1612)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1613 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1614 ->
                                                                                    StateBefore
                                                                                      v_1613
                                                                                      v_1614
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1615
                                                                            f@(StateBefore lAssign0
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1615,
                                                                                  foldl'
                                                                                    (\ q_1617
                                                                                       f@(Assign
                                                                                            lAssign0_1616
                                                                                            x0 e2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1617,
                                                                                             foldl'
                                                                                               (\ q_1620
                                                                                                  f@(Seq
                                                                                                       lAssign0_1616_1618
                                                                                                       jlf0_1619)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1620,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_1616_1618
                                                                                                                x0
                                                                                                                e2
                                                                                                                stAssign0
                                                                                                                jlf0_1619
                                                                                                                stf0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_1621,
                                                                                                      v_1622)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1621
                                                                                                          lAssign0_1616
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1622
                                                                                                          jlf0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          (v_1623,
                                                                                           v_1624)
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1625
                                                                                               ->
                                                                                               pure
                                                                                                 Assign
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1623
                                                                                                   lAssign0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1624
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1625)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsAssign
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1626 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1627 ->
                                                                                    StateBefore
                                                                                      v_1626
                                                                                      v_1627
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1628
                                                                            f@(StateBefore lAfter0
                                                                                 stAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1628,
                                                                                  foldl'
                                                                                    (\ q_1631
                                                                                       f@(Seq
                                                                                            jlf0_1629
                                                                                            lAfter0_1630)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1631,
                                                                                             foldl'
                                                                                               (\ q_1633
                                                                                                  f@(Assign
                                                                                                       jlf0_1629_1632
                                                                                                       x0
                                                                                                       e2)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1633,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                jlf0_1629_1632
                                                                                                                x0
                                                                                                                e2
                                                                                                                stf0
                                                                                                                lAfter0_1630
                                                                                                                stAfter0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     (v_1634,
                                                                                                      v_1635)
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1636
                                                                                                          ->
                                                                                                          pure
                                                                                                            Assign
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1634
                                                                                                              jlf0_1629
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1635
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1636)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsAssign
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_1637,
                                                                                           v_1638)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1637
                                                                                               jlf0
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1638
                                                                                               lAfter0)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1639 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1640 ->
                                                                                    StateBefore
                                                                                      v_1639
                                                                                      v_1640
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore
                                                                               db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1641 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1642 ->
                                                                         StateBefore v_1641 v_1642 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1572
                                                                    (S.singleton v_1573)))]
        _ -> q_1504

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_1644 vals ->
         S.foldl' (\ acc v_1645 -> StateBefore v_1644 v_1645 : acc) [] vals
           ++ rest)
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