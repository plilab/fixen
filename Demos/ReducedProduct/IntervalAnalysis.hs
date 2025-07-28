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
                  | AssignStepCont Natural Natural String Expr State State
                  | EvalCondFalseCont Natural State Expr Natural Natural State
                  | CondInitCont Natural Expr Natural Natural
                  | SeqStepCont Natural Natural State State
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | PhiInitCont Natural
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l2 x1 e4)
  = [StateBeforeFact (StateBefore l2 (empty 0))]
evaluate db (AssignStepCont lAssign0 lAfter0 x0 e2 stA0 stBefore0)
  = [StateBeforeFact
       (StateBefore lAfter0
          (join (insert x0 (eval e2 stBefore0) stBefore0) stA0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db (CondInitCont l1 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l1 (empty 0))]
evaluate db (SeqStepCont l10 l20 st10 st20)
  = [StateBeforeFact (StateBefore l20 (join st20 st10))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (join (narrowConditional e1 stc1) stt0))]
evaluate db (PhiInitCont l0)
  = [StateBeforeFact (StateBefore l0 (empty 0))]
evaluate db (VarInitCont l3 x2)
  = [StateBeforeFact (StateBefore l3 (singleton x2 Bot))]

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
step db fact q_10688
  = case fact of
        SeqFact f@(Seq v_10689 v_10690) -> Q.unions
                                             [q_10688,
                                              foldl'
                                                (\ q_10691 f@(Seq l10 l20) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_10691,
                                                         foldl'
                                                           (\ q_10693 f@(StateBefore l10_10692 st10)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_10693,
                                                                    foldl'
                                                                      (\ q_10695
                                                                         f@(StateBefore l20_10694
                                                                              st20)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_10695,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepCont
                                                                                       l10_10692
                                                                                       l20_10694
                                                                                       st10
                                                                                       st20))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_10696 vals ->
                                                                            concatMap
                                                                              (\ v_10697 ->
                                                                                 pure StateBefore
                                                                                   <*>
                                                                                   mlbs v_10696 l20
                                                                                   <*> pure v_10697)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBefore db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_10698 vals ->
                                                                 concatMap
                                                                   (\ v_10699 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_10698 l10
                                                                        <*> pure v_10699)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         foldl'
                                                           (\ q_10701
                                                              f@(StateBefore l10_10700 stBefore0) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_10701,
                                                                    foldl'
                                                                      (\ q_10703
                                                                         f@(StateBefore l20_10702
                                                                              stA0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_10703,
                                                                               foldl'
                                                                                 (\ q_10705
                                                                                    f@(Assign
                                                                                         l10_10700_10704
                                                                                         x0 e2)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_10705,
                                                                                          Q.singleton
                                                                                            (traceConclusion
                                                                                               (AssignStepCont
                                                                                                  l10_10700_10704
                                                                                                  l20_10702
                                                                                                  x0
                                                                                                  e2
                                                                                                  stA0
                                                                                                  stBefore0))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest
                                                                                       (v_10706,
                                                                                        v_10707)
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_10708
                                                                                            ->
                                                                                            pure
                                                                                              Assign
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_10706
                                                                                                l10_10700
                                                                                              <*>
                                                                                              pure
                                                                                                v_10707
                                                                                              <*>
                                                                                              pure
                                                                                                v_10708)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsAssign
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_10709 vals ->
                                                                            concatMap
                                                                              (\ v_10710 ->
                                                                                 pure StateBefore
                                                                                   <*>
                                                                                   mlbs v_10709 l20
                                                                                   <*> pure v_10710)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBefore db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_10711 vals ->
                                                                 concatMap
                                                                   (\ v_10712 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_10711 l10
                                                                        <*> pure v_10712)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_10713, v_10714) ->
                                                      Seq v_10713 v_10714 : rest)
                                                   []
                                                   (S.singleton (v_10689, v_10690)))]
        PhiFact f@(Phi v_10715) -> Q.unions
                                     [q_10688,
                                      foldl'
                                        (\ q_10716 f@(Phi l0) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_10716,
                                                 Q.singleton (traceConclusion (PhiInitCont l0))]))
                                        Q.empty
                                        (S.foldl' (\ rest v_10717 -> Phi v_10717 : rest) []
                                           (S.singleton v_10715))]
        CondFact f@(Cond v_10718 v_10719 v_10720 v_10721) -> Q.unions
                                                               [q_10688,
                                                                foldl'
                                                                  (\ q_10722
                                                                     f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_10722,
                                                                           foldl'
                                                                             (\ q_10724
                                                                                f@(StateBefore
                                                                                     jlf0_10723
                                                                                     stf0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_10724,
                                                                                      foldl'
                                                                                        (\ q_10726
                                                                                           f@(StateBefore
                                                                                                lcond0_10725
                                                                                                stc0)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_10726,
                                                                                                 if
                                                                                                   eq
                                                                                                     (evaluateConditional
                                                                                                        e0
                                                                                                        stc0)
                                                                                                     False
                                                                                                   then
                                                                                                   trace
                                                                                                     ("got: "
                                                                                                        ++
                                                                                                        show
                                                                                                          f)
                                                                                                     (Q.unions
                                                                                                        [q_10726,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondFalseCont
                                                                                                                 lcond0_10725
                                                                                                                 stc0
                                                                                                                 e0
                                                                                                                 jlt0
                                                                                                                 jlf0_10723
                                                                                                                 stf0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_10727
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_10728
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_10727
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_10728)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_10729 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_10730 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_10729
                                                                                            jlf0
                                                                                          <*>
                                                                                          pure
                                                                                            v_10730)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           foldl'
                                                                             (\ q_10732
                                                                                f@(StateBefore
                                                                                     jlt0_10731
                                                                                     stt0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_10732,
                                                                                      foldl'
                                                                                        (\ q_10734
                                                                                           f@(StateBefore
                                                                                                lcond0_10733
                                                                                                stc1)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_10734,
                                                                                                 if
                                                                                                   eq
                                                                                                     (evaluateConditional
                                                                                                        e0
                                                                                                        stc1)
                                                                                                     True
                                                                                                   then
                                                                                                   trace
                                                                                                     ("got: "
                                                                                                        ++
                                                                                                        show
                                                                                                          f)
                                                                                                     (Q.unions
                                                                                                        [q_10734,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondTrueCont
                                                                                                                 lcond0_10733
                                                                                                                 stc1
                                                                                                                 e0
                                                                                                                 jlt0_10731
                                                                                                                 jlf0
                                                                                                                 stt0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_10735
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_10736
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_10735
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_10736)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_10737 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_10738 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_10737
                                                                                            jlt0
                                                                                          <*>
                                                                                          pure
                                                                                            v_10738)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondInitCont lcond0
                                                                                   e0
                                                                                   jlt0
                                                                                   jlf0))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_10739 vals ->
                                                                        S.foldl'
                                                                          (\ acc
                                                                             (v_10740, v_10741,
                                                                              v_10742)
                                                                             ->
                                                                             Cond v_10739 v_10740
                                                                               v_10741
                                                                               v_10742
                                                                               : acc)
                                                                          []
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (M.singleton v_10718
                                                                        (S.singleton
                                                                           (v_10719, v_10720,
                                                                            v_10721))))]
        AssignFact f@(Assign v_10743 v_10744 v_10745) -> Q.unions
                                                           [q_10688,
                                                            foldl'
                                                              (\ q_10746 f@(Assign lAssign0 x0 e2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_10746,
                                                                       foldl'
                                                                         (\ q_10748
                                                                            f@(StateBefore
                                                                                 lAssign0_10747
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_10748,
                                                                                  foldl'
                                                                                    (\ q_10749
                                                                                       f@(StateBefore
                                                                                            lAfter0
                                                                                            stA0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_10749,
                                                                                             foldl'
                                                                                               (\ q_10752
                                                                                                  f@(Seq
                                                                                                       lAssign0_10747_10750
                                                                                                       lAfter0_10751)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_10752,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_10747_10750
                                                                                                                lAfter0_10751
                                                                                                                x0
                                                                                                                e2
                                                                                                                stA0
                                                                                                                stBefore0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_10753,
                                                                                                      v_10754)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_10753
                                                                                                          lAssign0_10747
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_10754
                                                                                                          lAfter0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_10755
                                                                                          vals ->
                                                                                          S.foldl'
                                                                                            (\ acc
                                                                                               v_10756
                                                                                               ->
                                                                                               StateBefore
                                                                                                 v_10755
                                                                                                 v_10756
                                                                                                 :
                                                                                                 acc)
                                                                                            []
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_10757 vals ->
                                                                               concatMap
                                                                                 (\ v_10758 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_10757
                                                                                        lAssign0
                                                                                      <*>
                                                                                      pure v_10758)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (AssignInitCont lAssign0
                                                                               x0
                                                                               e2))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest (v_10759, v_10760) vals ->
                                                                    S.foldl'
                                                                      (\ acc v_10761 ->
                                                                         Assign v_10759 v_10760
                                                                           v_10761
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton (v_10743, v_10744)
                                                                    (S.singleton v_10745)))]
        VarFact f@(Var v_10762 v_10763) -> Q.unions
                                             [q_10688,
                                              foldl'
                                                (\ q_10764 f@(Var l3 x2) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_10764,
                                                         Q.singleton
                                                           (traceConclusion (VarInitCont l3 x2))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_10765, v_10766) ->
                                                      Var v_10765 v_10766 : rest)
                                                   []
                                                   (S.singleton (v_10762, v_10763)))]
        StateBeforeFact f@(StateBefore v_10767 v_10768) -> Q.unions
                                                             [q_10688,
                                                              foldl'
                                                                (\ q_10769 f@(StateBefore l10 st10)
                                                                   ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_10769,
                                                                         foldl'
                                                                           (\ q_10771
                                                                              f@(Seq l10_10770 l20)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10771,
                                                                                    foldl'
                                                                                      (\ q_10773
                                                                                         f@(StateBefore
                                                                                              l20_10772
                                                                                              st20)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10773,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepCont
                                                                                                       l10_10770
                                                                                                       l20_10772
                                                                                                       st10
                                                                                                       st20))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10774
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10775
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10774
                                                                                                     l20
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10775)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_10776, v_10777)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    mlbs v_10776 l10
                                                                                    <*>
                                                                                    pure v_10777)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_10778
                                                                              f@(StateBefore l20
                                                                                   st20)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10778,
                                                                                    foldl'
                                                                                      (\ q_10781
                                                                                         f@(Seq
                                                                                              l20_10779
                                                                                              l10_10780)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10781,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepCont
                                                                                                       l20_10779
                                                                                                       l10_10780
                                                                                                       st20
                                                                                                       st10))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_10782,
                                                                                             v_10783)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10782
                                                                                                 l20
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10783
                                                                                                 l10)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10784 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10785 ->
                                                                                      StateBefore
                                                                                        v_10784
                                                                                        v_10785
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10787
                                                                              f@(Cond lcond0 e0 jlt0
                                                                                   l10_10786)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10787,
                                                                                    foldl'
                                                                                      (\ q_10789
                                                                                         f@(StateBefore
                                                                                              lcond0_10788
                                                                                              stc0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10789,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e0
                                                                                                      stc0)
                                                                                                   False
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_10789,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               lcond0_10788
                                                                                                               stc0
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               l10_10786
                                                                                                               st10))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10790
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10791
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10790
                                                                                                     lcond0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10791)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10792 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_10793,
                                                                                       v_10794,
                                                                                       v_10795)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_10792
                                                                                        <*>
                                                                                        pure v_10793
                                                                                        <*>
                                                                                        pure v_10794
                                                                                        <*>
                                                                                        mlbs v_10795
                                                                                          l10)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_10796
                                                                              f@(StateBefore jlf0
                                                                                   stf0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10796,
                                                                                    foldl'
                                                                                      (\ q_10799
                                                                                         f@(Cond
                                                                                              l10_10797
                                                                                              e0
                                                                                              jlt0
                                                                                              jlf0_10798)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10799,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e0
                                                                                                      st10)
                                                                                                   False
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_10799,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               l10_10797
                                                                                                               st10
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               jlf0_10798
                                                                                                               stf0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10800
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_10801,
                                                                                                  v_10802,
                                                                                                  v_10803)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10800
                                                                                                     l10
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10801
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10802
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10803
                                                                                                     jlf0)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10804 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10805 ->
                                                                                      StateBefore
                                                                                        v_10804
                                                                                        v_10805
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10807
                                                                              f@(Cond lcond1 e1
                                                                                   l10_10806 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10807,
                                                                                    foldl'
                                                                                      (\ q_10809
                                                                                         f@(StateBefore
                                                                                              lcond1_10808
                                                                                              stc1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10809,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e1
                                                                                                      stc1)
                                                                                                   True
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_10809,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               lcond1_10808
                                                                                                               stc1
                                                                                                               e1
                                                                                                               l10_10806
                                                                                                               jlf1
                                                                                                               st10))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10810
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10811
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10810
                                                                                                     lcond1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10811)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10812 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_10813,
                                                                                       v_10814,
                                                                                       v_10815)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_10812
                                                                                        <*>
                                                                                        pure v_10813
                                                                                        <*>
                                                                                        mlbs v_10814
                                                                                          l10
                                                                                        <*>
                                                                                        pure
                                                                                          v_10815)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_10816
                                                                              f@(StateBefore jlt1
                                                                                   stt0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10816,
                                                                                    foldl'
                                                                                      (\ q_10819
                                                                                         f@(Cond
                                                                                              l10_10817
                                                                                              e1
                                                                                              jlt1_10818
                                                                                              jlf1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10819,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e1
                                                                                                      st10)
                                                                                                   True
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_10819,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               l10_10817
                                                                                                               st10
                                                                                                               e1
                                                                                                               jlt1_10818
                                                                                                               jlf1
                                                                                                               stt0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10820
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_10821,
                                                                                                  v_10822,
                                                                                                  v_10823)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10820
                                                                                                     l10
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10821
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10822
                                                                                                     jlt1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10823)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10824 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10825 ->
                                                                                      StateBefore
                                                                                        v_10824
                                                                                        v_10825
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10827
                                                                              f@(Seq l10_10826
                                                                                   lAfter0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10827,
                                                                                    foldl'
                                                                                      (\ q_10829
                                                                                         f@(StateBefore
                                                                                              lAfter0_10828
                                                                                              stA0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10829,
                                                                                               foldl'
                                                                                                 (\ q_10831
                                                                                                    f@(Assign
                                                                                                         l10_10826_10830
                                                                                                         x0
                                                                                                         e2)
                                                                                                    ->
                                                                                                    trace
                                                                                                      ("got: "
                                                                                                         ++
                                                                                                         show
                                                                                                           f)
                                                                                                      (Q.unions
                                                                                                         [q_10831,
                                                                                                          Q.singleton
                                                                                                            (traceConclusion
                                                                                                               (AssignStepCont
                                                                                                                  l10_10826_10830
                                                                                                                  lAfter0_10828
                                                                                                                  x0
                                                                                                                  e2
                                                                                                                  stA0
                                                                                                                  st10))]))
                                                                                                 Q.empty
                                                                                                 (M.foldlWithKey'
                                                                                                    (\ rest
                                                                                                       (v_10832,
                                                                                                        v_10833)
                                                                                                       vals
                                                                                                       ->
                                                                                                       concatMap
                                                                                                         (\ v_10834
                                                                                                            ->
                                                                                                            pure
                                                                                                              Assign
                                                                                                              <*>
                                                                                                              mlbs
                                                                                                                v_10832
                                                                                                                l10_10826
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10833
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10834)
                                                                                                         vals
                                                                                                         ++
                                                                                                         rest)
                                                                                                    []
                                                                                                    (factsAssign
                                                                                                       db))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10835
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10836
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10835
                                                                                                     lAfter0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10836)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_10837, v_10838)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    mlbs v_10837 l10
                                                                                    <*>
                                                                                    pure v_10838)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_10839
                                                                              f@(StateBefore
                                                                                   lAssign0
                                                                                   stBefore0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10839,
                                                                                    foldl'
                                                                                      (\ q_10842
                                                                                         f@(Seq
                                                                                              lAssign0_10840
                                                                                              l10_10841)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10842,
                                                                                               foldl'
                                                                                                 (\ q_10844
                                                                                                    f@(Assign
                                                                                                         lAssign0_10840_10843
                                                                                                         x0
                                                                                                         e2)
                                                                                                    ->
                                                                                                    trace
                                                                                                      ("got: "
                                                                                                         ++
                                                                                                         show
                                                                                                           f)
                                                                                                      (Q.unions
                                                                                                         [q_10844,
                                                                                                          Q.singleton
                                                                                                            (traceConclusion
                                                                                                               (AssignStepCont
                                                                                                                  lAssign0_10840_10843
                                                                                                                  l10_10841
                                                                                                                  x0
                                                                                                                  e2
                                                                                                                  st10
                                                                                                                  stBefore0))]))
                                                                                                 Q.empty
                                                                                                 (M.foldlWithKey'
                                                                                                    (\ rest
                                                                                                       (v_10845,
                                                                                                        v_10846)
                                                                                                       vals
                                                                                                       ->
                                                                                                       concatMap
                                                                                                         (\ v_10847
                                                                                                            ->
                                                                                                            pure
                                                                                                              Assign
                                                                                                              <*>
                                                                                                              mlbs
                                                                                                                v_10845
                                                                                                                lAssign0_10840
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10846
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10847)
                                                                                                         vals
                                                                                                         ++
                                                                                                         rest)
                                                                                                    []
                                                                                                    (factsAssign
                                                                                                       db))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_10848,
                                                                                             v_10849)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10848
                                                                                                 lAssign0
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10849
                                                                                                 l10)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10850 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10851 ->
                                                                                      StateBefore
                                                                                        v_10850
                                                                                        v_10851
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_10852 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_10853 ->
                                                                           StateBefore v_10852
                                                                             v_10853
                                                                             : acc)
                                                                        []
                                                                        vals
                                                                        ++ rest)
                                                                   []
                                                                   (M.singleton v_10767
                                                                      (S.singleton v_10768)))]

stateBefore :: Natural -> DataBase -> [StateBefore]
stateBefore v_10854 db
  = M.foldlWithKey'
      (\ rest v_10856 vals ->
         concatMap
           (\ v_10857 ->
              pure StateBefore <*> mlbs v_10856 v_10854 <*> pure v_10857)
           vals
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