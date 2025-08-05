{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.IntervalAnalysis where
import ReducedProduct.Interval
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

data StateBefore = StateBefore Natural IState
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
                  | AssignStepCont Natural String Expr IState Natural IState
                  | EvalCondFalseCont Natural IState Expr Natural Natural IState
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondTrueCont Natural IState Expr Natural Natural IState
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l2 x2 e4)
  = [StateBeforeFact (StateBefore l2 (emptyI 0))]
evaluate db
  (AssignStepCont lAssign0 x0 e2 stAssign0 lAfter0 stAfter0)
  = [StateBeforeFact
       (StateBefore lAfter0
          (joinI (insertI x0 (evalI e2 stAssign0) stAssign0) stAfter0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (joinI (narrowConditionalIFalse e0 stc0) stf0))]
evaluate db (CondInitCont l1 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l1 (emptyI 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (joinI (narrowConditionalI e1 stc1) stt0))]
evaluate db (VarInitCont l0 x1)
  = [StateBeforeFact (StateBefore l0 (emptyI 0))]

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsStateBefore ::
                         M.HashMap Natural (S.HashSet IState),
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
step db fact q_1646
  = case fact of
        SeqFact f@(Seq v_1647 v_1648) -> Q.unions
                                           [q_1646,
                                            foldl'
                                              (\ q_1649 f@(Seq lAssign0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1649,
                                                       foldl'
                                                         (\ q_1651
                                                            f@(StateBefore lAfter0_1650 stAfter0) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1651,
                                                                  foldl'
                                                                    (\ q_1653
                                                                       f@(StateBefore lAssign0_1652
                                                                            stAssign0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1653,
                                                                             foldl'
                                                                               (\ q_1655
                                                                                  f@(Assign
                                                                                       lAssign0_1652_1654
                                                                                       x0 e2)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_1655,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                lAssign0_1652_1654
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_1650
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest
                                                                                     (v_1656,
                                                                                      v_1657)
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_1658 ->
                                                                                          pure
                                                                                            Assign
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_1656
                                                                                              lAssign0_1652
                                                                                            <*>
                                                                                            pure
                                                                                              v_1657
                                                                                            <*>
                                                                                            pure
                                                                                              v_1658)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsAssign
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1659 vals ->
                                                                          concatMap
                                                                            (\ v_1660 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1659
                                                                                   lAssign0
                                                                                 <*> pure v_1660)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_1661 vals ->
                                                               concatMap
                                                                 (\ v_1662 ->
                                                                    pure StateBefore <*>
                                                                      mlbs v_1661 lAfter0
                                                                      <*> pure v_1662)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateBefore db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1663, v_1664) ->
                                                    Seq v_1663 v_1664 : rest)
                                                 []
                                                 (S.singleton (v_1647, v_1648)))]
        CondFact f@(Cond v_1665 v_1666 v_1667 v_1668) -> Q.unions
                                                           [q_1646,
                                                            foldl'
                                                              (\ q_1669 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1669,
                                                                       foldl'
                                                                         (\ q_1671
                                                                            f@(StateBefore jlf0_1670
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1671,
                                                                                  foldl'
                                                                                    (\ q_1673
                                                                                       f@(StateBefore
                                                                                            lcond0_1672
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1673,
                                                                                             if
                                                                                               leq
                                                                                                 BFalse
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stc0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1673,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1672
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1670
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1674
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1675
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1674
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1675)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1676 vals ->
                                                                               concatMap
                                                                                 (\ v_1677 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1676
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1677)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1679
                                                                            f@(StateBefore jlt0_1678
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1679,
                                                                                  foldl'
                                                                                    (\ q_1681
                                                                                       f@(StateBefore
                                                                                            lcond0_1680
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1681,
                                                                                             if
                                                                                               leq
                                                                                                 BTrue
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stc1)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1681,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond0_1680
                                                                                                             stc1
                                                                                                             e0
                                                                                                             jlt0_1678
                                                                                                             jlf0
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1682
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1683
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1682
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1683)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1684 vals ->
                                                                               concatMap
                                                                                 (\ v_1685 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1684
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_1685)
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
                                                                 (\ rest v_1686 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1687, v_1688, v_1689) ->
                                                                         Cond v_1686 v_1687 v_1688
                                                                           v_1689
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1665
                                                                    (S.singleton
                                                                       (v_1666, v_1667, v_1668))))]
        AssignFact f@(Assign v_1690 v_1691 v_1692) -> Q.unions
                                                        [q_1646,
                                                         foldl'
                                                           (\ q_1693 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1693,
                                                                    foldl'
                                                                      (\ q_1694
                                                                         f@(StateBefore lAfter0
                                                                              stAfter0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1694,
                                                                               foldl'
                                                                                 (\ q_1696
                                                                                    f@(StateBefore
                                                                                         lAssign0_1695
                                                                                         stAssign0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_1696,
                                                                                          foldl'
                                                                                            (\ q_1699
                                                                                               f@(Seq
                                                                                                    lAssign0_1695_1697
                                                                                                    lAfter0_1698)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1699,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_1695_1697
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter0_1698
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_1700,
                                                                                                   v_1701)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1700
                                                                                                       lAssign0_1695
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1701
                                                                                                       lAfter0)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_1702
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_1703
                                                                                            ->
                                                                                            pure
                                                                                              StateBefore
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_1702
                                                                                                lAssign0
                                                                                              <*>
                                                                                              pure
                                                                                                v_1703)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1704 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_1705 ->
                                                                                 StateBefore v_1704
                                                                                   v_1705
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
                                                              (\ rest (v_1706, v_1707) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1708 ->
                                                                      Assign v_1706 v_1707 v_1708 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1690, v_1691)
                                                                 (S.singleton v_1692)))]
        VarFact f@(Var v_1709 v_1710) -> Q.unions
                                           [q_1646,
                                            foldl'
                                              (\ q_1711 f@(Var l0 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1711,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l0 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1712, v_1713) ->
                                                    Var v_1712 v_1713 : rest)
                                                 []
                                                 (S.singleton (v_1709, v_1710)))]
        StateBeforeFact f@(StateBefore v_1714 v_1715) -> Q.unions
                                                           [q_1646,
                                                            foldl'
                                                              (\ q_1716 f@(StateBefore jlf0 stf0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1716,
                                                                       foldl'
                                                                         (\ q_1718
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 jlf0_1717)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1718,
                                                                                  foldl'
                                                                                    (\ q_1720
                                                                                       f@(StateBefore
                                                                                            lcond0_1719
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1720,
                                                                                             if
                                                                                               leq
                                                                                                 BFalse
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stc0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1720,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1719
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1717
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1721
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1722
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1721
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1722)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1723 vals ->
                                                                               concatMap
                                                                                 (\ (v_1724, v_1725,
                                                                                     v_1726)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1723
                                                                                      <*>
                                                                                      pure v_1724
                                                                                      <*>
                                                                                      pure v_1725
                                                                                      <*>
                                                                                      mlbs v_1726
                                                                                        jlf0)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1728
                                                                            f@(Cond jlf0_1727 e0
                                                                                 jlt0 lcond0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1728,
                                                                                  foldl'
                                                                                    (\ q_1730
                                                                                       f@(StateBefore
                                                                                            lcond0_1729
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1730,
                                                                                             if
                                                                                               leq
                                                                                                 BFalse
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    stf0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1730,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             jlf0_1727
                                                                                                             stf0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             lcond0_1729
                                                                                                             stc0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1731
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1732
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1731
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1732)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1733 vals ->
                                                                               concatMap
                                                                                 (\ (v_1734, v_1735,
                                                                                     v_1736)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      mlbs v_1733
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1734
                                                                                      <*>
                                                                                      pure v_1735
                                                                                      <*>
                                                                                      pure v_1736)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1738
                                                                            f@(Cond lcond1 e1
                                                                                 jlf0_1737 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1738,
                                                                                  foldl'
                                                                                    (\ q_1740
                                                                                       f@(StateBefore
                                                                                            lcond1_1739
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1740,
                                                                                             if
                                                                                               leq
                                                                                                 BTrue
                                                                                                 (evaluateConditional
                                                                                                    e1
                                                                                                    stc1)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1740,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond1_1739
                                                                                                             stc1
                                                                                                             e1
                                                                                                             jlf0_1737
                                                                                                             jlf1
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1741
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1742
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1741
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1742)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1743 vals ->
                                                                               concatMap
                                                                                 (\ (v_1744, v_1745,
                                                                                     v_1746)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1743
                                                                                      <*>
                                                                                      pure v_1744
                                                                                      <*>
                                                                                      mlbs v_1745
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1746)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1747
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1747,
                                                                                  foldl'
                                                                                    (\ q_1750
                                                                                       f@(Cond
                                                                                            jlf0_1748
                                                                                            e1
                                                                                            jlt1_1749
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1750,
                                                                                             if
                                                                                               leq
                                                                                                 BTrue
                                                                                                 (evaluateConditional
                                                                                                    e1
                                                                                                    stf0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1750,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             jlf0_1748
                                                                                                             stf0
                                                                                                             e1
                                                                                                             jlt1_1749
                                                                                                             jlf1
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1751
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1752,
                                                                                                v_1753,
                                                                                                v_1754)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1751
                                                                                                   jlf0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1752
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1753
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1754)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1755 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1756 ->
                                                                                    StateBefore
                                                                                      v_1755
                                                                                      v_1756
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1757
                                                                            f@(StateBefore lAssign0
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1757,
                                                                                  foldl'
                                                                                    (\ q_1759
                                                                                       f@(Assign
                                                                                            lAssign0_1758
                                                                                            x0 e2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1759,
                                                                                             foldl'
                                                                                               (\ q_1762
                                                                                                  f@(Seq
                                                                                                       lAssign0_1758_1760
                                                                                                       jlf0_1761)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1762,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_1758_1760
                                                                                                                x0
                                                                                                                e2
                                                                                                                stAssign0
                                                                                                                jlf0_1761
                                                                                                                stf0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_1763,
                                                                                                      v_1764)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1763
                                                                                                          lAssign0_1758
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1764
                                                                                                          jlf0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          (v_1765,
                                                                                           v_1766)
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1767
                                                                                               ->
                                                                                               pure
                                                                                                 Assign
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1765
                                                                                                   lAssign0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1766
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1767)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsAssign
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1768 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1769 ->
                                                                                    StateBefore
                                                                                      v_1768
                                                                                      v_1769
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1770
                                                                            f@(StateBefore lAfter0
                                                                                 stAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1770,
                                                                                  foldl'
                                                                                    (\ q_1773
                                                                                       f@(Seq
                                                                                            jlf0_1771
                                                                                            lAfter0_1772)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1773,
                                                                                             foldl'
                                                                                               (\ q_1775
                                                                                                  f@(Assign
                                                                                                       jlf0_1771_1774
                                                                                                       x0
                                                                                                       e2)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1775,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                jlf0_1771_1774
                                                                                                                x0
                                                                                                                e2
                                                                                                                stf0
                                                                                                                lAfter0_1772
                                                                                                                stAfter0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     (v_1776,
                                                                                                      v_1777)
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1778
                                                                                                          ->
                                                                                                          pure
                                                                                                            Assign
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1776
                                                                                                              jlf0_1771
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1777
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1778)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsAssign
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_1779,
                                                                                           v_1780)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1779
                                                                                               jlf0
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1780
                                                                                               lAfter0)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1781 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1782 ->
                                                                                    StateBefore
                                                                                      v_1781
                                                                                      v_1782
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore
                                                                               db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1783 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1784 ->
                                                                         StateBefore v_1783 v_1784 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1714
                                                                    (S.singleton v_1715)))]
        _ -> q_1646

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_1786 vals ->
         S.foldl' (\ acc v_1787 -> StateBefore v_1786 v_1787 : acc) [] vals
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