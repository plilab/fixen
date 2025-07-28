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

data StateAfter = StateAfter Natural State
                    deriving (Eq, Show, Generic)

instance Hashable StateAfter

instance PartialOrd StateAfter where
        leq (StateAfter v0 v1) (StateAfter v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkStateAfter v0 v1 = StateAfterFact (StateAfter v0 v1)

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
          | StateAfterFact StateAfter
          | StateBeforeFact StateBefore
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | VarInitAfterCont Natural String
                  | AssignInitCont Natural String Expr
                  | PhiInitAfterCont Natural
                  | AssignStepCont Natural String Expr State
                  | EvalCondFalseCont Natural State Expr Natural Natural State
                  | CondInitAfterCont Natural Expr Natural Natural
                  | PhiStepCont Natural State
                  | CondInitCont Natural Expr Natural Natural
                  | AssignInitAfterCont Natural String Expr
                  | SeqStepCont Natural Natural State State
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | PhiInitCont Natural
                  | SeqStepStateCont Natural Natural State State
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (VarInitAfterCont l4 x2)
  = [StateAfterFact (StateAfter l4 (singleton x2 Bot))]
evaluate db (AssignInitCont l7 x3 e6)
  = [StateBeforeFact (StateBefore l7 (empty 0))]
evaluate db (PhiInitAfterCont l1)
  = [StateAfterFact (StateAfter l1 (empty 0))]
evaluate db (AssignStepCont lAssign0 x0 e2 stBefore0)
  = [StateAfterFact
       (StateAfter lAssign0 (insert x0 (eval e2 stBefore0) stBefore0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact (StateBefore jlf0 (join stc0 stf0))]
evaluate db (CondInitAfterCont l2 e3 jlt2 jlf2)
  = [StateAfterFact (StateAfter l2 (empty 0))]
evaluate db (PhiStepCont l0 st0)
  = [StateAfterFact (StateAfter l0 st0)]
evaluate db (CondInitCont l6 e5 jlt3 jlf3)
  = [StateBeforeFact (StateBefore l6 (empty 0))]
evaluate db (AssignInitAfterCont l3 x1 e4)
  = [StateAfterFact (StateAfter l3 (empty 0))]
evaluate db (SeqStepCont l11 l21 st11 st21)
  = [StateBeforeFact (StateBefore l21 (join st21 st11))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact (StateBefore jlt1 (join stc1 stt0))]
evaluate db (PhiInitCont l5)
  = [StateBeforeFact (StateBefore l5 (empty 0))]
evaluate db (SeqStepStateCont l10 l20 st10 st20)
  = [StateBeforeFact (StateBefore l20 (join st20 st10))]

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
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr),
                         factsStateAfter :: M.HashMap Natural (S.HashSet State)}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB
  = DataBase M.empty S.empty S.empty M.empty S.empty M.empty M.empty

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
          StateAfterFact (StateAfter v0 v1) -> if
                                                 M.member v0 (factsStateAfter db) then
                                                 first
                                                   (\ hset ->
                                                      db{factsStateAfter =
                                                           M.insert v0 hset (factsStateAfter db)})
                                                   (update ((M.!) (factsStateAfter db) v0) v1)
                                                 else
                                                 (db{factsStateAfter =
                                                       M.insert v0 (S.singleton v1)
                                                         (factsStateAfter db)},
                                                  True)

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_11844
  = case fact of
        SeqFact f@(Seq v_11845 v_11846) -> Q.unions
                                             [q_11844,
                                              foldl'
                                                (\ q_11847 f@(Seq l10 l20) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_11847,
                                                         foldl'
                                                           (\ q_11849 f@(StateBefore l20_11848 st20)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_11849,
                                                                    foldl'
                                                                      (\ q_11851
                                                                         f@(StateAfter l10_11850
                                                                              st10)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_11851,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepStateCont
                                                                                       l10_11850
                                                                                       l20_11848
                                                                                       st10
                                                                                       st20))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_11852 vals ->
                                                                            concatMap
                                                                              (\ v_11853 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_11852 l10
                                                                                   <*> pure v_11853)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_11854 vals ->
                                                                 concatMap
                                                                   (\ v_11855 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_11854 l20
                                                                        <*> pure v_11855)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         foldl'
                                                           (\ q_11857 f@(StateBefore l20_11856 st21)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_11857,
                                                                    foldl'
                                                                      (\ q_11859
                                                                         f@(StateAfter l10_11858
                                                                              st11)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_11859,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepCont
                                                                                       l10_11858
                                                                                       l20_11856
                                                                                       st11
                                                                                       st21))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_11860 vals ->
                                                                            concatMap
                                                                              (\ v_11861 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_11860 l10
                                                                                   <*> pure v_11861)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_11862 vals ->
                                                                 concatMap
                                                                   (\ v_11863 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_11862 l20
                                                                        <*> pure v_11863)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_11864, v_11865) ->
                                                      Seq v_11864 v_11865 : rest)
                                                   []
                                                   (S.singleton (v_11845, v_11846)))]
        PhiFact f@(Phi v_11866) -> Q.unions
                                     [q_11844,
                                      foldl'
                                        (\ q_11867 f@(Phi l0) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_11867,
                                                 foldl'
                                                   (\ q_11869 f@(StateBefore l0_11868 st0) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_11869,
                                                            Q.singleton
                                                              (traceConclusion
                                                                 (PhiStepCont l0_11868 st0))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_11870 vals ->
                                                         concatMap
                                                           (\ v_11871 ->
                                                              pure StateBefore <*> mlbs v_11870 l0
                                                                <*> pure v_11871)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateBefore db)),
                                                 Q.singleton
                                                   (traceConclusion (PhiInitAfterCont l0)),
                                                 Q.singleton (traceConclusion (PhiInitCont l0))]))
                                        Q.empty
                                        (S.foldl' (\ rest v_11872 -> Phi v_11872 : rest) []
                                           (S.singleton v_11866))]
        CondFact f@(Cond v_11873 v_11874 v_11875 v_11876) -> Q.unions
                                                               [q_11844,
                                                                foldl'
                                                                  (\ q_11877
                                                                     f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_11877,
                                                                           foldl'
                                                                             (\ q_11879
                                                                                f@(StateBefore
                                                                                     jlf0_11878
                                                                                     stf0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_11879,
                                                                                      foldl'
                                                                                        (\ q_11881
                                                                                           f@(StateBefore
                                                                                                lcond0_11880
                                                                                                stc0)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_11881,
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
                                                                                                        [q_11881,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondFalseCont
                                                                                                                 lcond0_11880
                                                                                                                 stc0
                                                                                                                 e0
                                                                                                                 jlt0
                                                                                                                 jlf0_11878
                                                                                                                 stf0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_11882
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_11883
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_11882
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_11883)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_11884 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_11885 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_11884
                                                                                            jlf0
                                                                                          <*>
                                                                                          pure
                                                                                            v_11885)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           foldl'
                                                                             (\ q_11887
                                                                                f@(StateBefore
                                                                                     jlt0_11886
                                                                                     stt0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_11887,
                                                                                      foldl'
                                                                                        (\ q_11889
                                                                                           f@(StateBefore
                                                                                                lcond0_11888
                                                                                                stc1)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_11889,
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
                                                                                                        [q_11889,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondTrueCont
                                                                                                                 lcond0_11888
                                                                                                                 stc1
                                                                                                                 e0
                                                                                                                 jlt0_11886
                                                                                                                 jlf0
                                                                                                                 stt0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_11890
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_11891
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_11890
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_11891)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_11892 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_11893 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_11892
                                                                                            jlt0
                                                                                          <*>
                                                                                          pure
                                                                                            v_11893)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondInitAfterCont
                                                                                   lcond0
                                                                                   e0
                                                                                   jlt0
                                                                                   jlf0)),
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondInitCont lcond0
                                                                                   e0
                                                                                   jlt0
                                                                                   jlf0))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_11894 vals ->
                                                                        S.foldl'
                                                                          (\ acc
                                                                             (v_11895, v_11896,
                                                                              v_11897)
                                                                             ->
                                                                             Cond v_11894 v_11895
                                                                               v_11896
                                                                               v_11897
                                                                               : acc)
                                                                          []
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (M.singleton v_11873
                                                                        (S.singleton
                                                                           (v_11874, v_11875,
                                                                            v_11876))))]
        AssignFact f@(Assign v_11898 v_11899 v_11900) -> Q.unions
                                                           [q_11844,
                                                            foldl'
                                                              (\ q_11901 f@(Assign lAssign0 x0 e2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_11901,
                                                                       foldl'
                                                                         (\ q_11903
                                                                            f@(StateBefore
                                                                                 lAssign0_11902
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_11903,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (AssignStepCont
                                                                                          lAssign0_11902
                                                                                          x0
                                                                                          e2
                                                                                          stBefore0))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_11904 vals ->
                                                                               concatMap
                                                                                 (\ v_11905 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_11904
                                                                                        lAssign0
                                                                                      <*>
                                                                                      pure v_11905)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (AssignInitAfterCont
                                                                               lAssign0
                                                                               x0
                                                                               e2)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (AssignInitCont lAssign0
                                                                               x0
                                                                               e2))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest (v_11906, v_11907) vals ->
                                                                    S.foldl'
                                                                      (\ acc v_11908 ->
                                                                         Assign v_11906 v_11907
                                                                           v_11908
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton (v_11898, v_11899)
                                                                    (S.singleton v_11900)))]
        VarFact f@(Var v_11909 v_11910) -> Q.unions
                                             [q_11844,
                                              foldl'
                                                (\ q_11911 f@(Var l4 x2) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_11911,
                                                         Q.singleton
                                                           (traceConclusion
                                                              (VarInitAfterCont l4 x2))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_11912, v_11913) ->
                                                      Var v_11912 v_11913 : rest)
                                                   []
                                                   (S.singleton (v_11909, v_11910)))]
        StateAfterFact f@(StateAfter v_11914 v_11915) -> Q.unions
                                                           [q_11844,
                                                            foldl'
                                                              (\ q_11916 f@(StateAfter l10 st10) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_11916,
                                                                       foldl'
                                                                         (\ q_11918
                                                                            f@(Seq l10_11917 l20) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_11918,
                                                                                  foldl'
                                                                                    (\ q_11920
                                                                                       f@(StateBefore
                                                                                            l20_11919
                                                                                            st20)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_11920,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepStateCont
                                                                                                     l10_11917
                                                                                                     l20_11919
                                                                                                     st10
                                                                                                     st20))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_11921
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_11922
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_11921
                                                                                                   l20
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_11922)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_11923, v_11924) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_11923 l10
                                                                                  <*> pure v_11924)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_11926
                                                                            f@(Seq l10_11925 l21) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_11926,
                                                                                  foldl'
                                                                                    (\ q_11928
                                                                                       f@(StateBefore
                                                                                            l21_11927
                                                                                            st21)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_11928,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepCont
                                                                                                     l10_11925
                                                                                                     l21_11927
                                                                                                     st10
                                                                                                     st21))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_11929
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_11930
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_11929
                                                                                                   l21
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_11930)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_11931, v_11932) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_11931 l10
                                                                                  <*> pure v_11932)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_11933 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_11934 ->
                                                                         StateAfter v_11933 v_11934
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_11914
                                                                    (S.singleton v_11915)))]
        StateBeforeFact f@(StateBefore v_11935 v_11936) -> Q.unions
                                                             [q_11844,
                                                              foldl'
                                                                (\ q_11937 f@(StateBefore l20 st20)
                                                                   ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_11937,
                                                                         foldl'
                                                                           (\ q_11939
                                                                              f@(Seq l10 l20_11938)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11939,
                                                                                    foldl'
                                                                                      (\ q_11941
                                                                                         f@(StateAfter
                                                                                              l10_11940
                                                                                              st10)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11941,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepStateCont
                                                                                                       l10_11940
                                                                                                       l20_11938
                                                                                                       st10
                                                                                                       st20))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11942
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11943
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateAfter
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11942
                                                                                                     l10
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11943)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateAfter
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_11944, v_11945)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    pure v_11944
                                                                                    <*>
                                                                                    mlbs v_11945
                                                                                      l20)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_11947
                                                                              f@(Seq l11 l20_11946)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11947,
                                                                                    foldl'
                                                                                      (\ q_11949
                                                                                         f@(StateAfter
                                                                                              l11_11948
                                                                                              st11)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11949,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepCont
                                                                                                       l11_11948
                                                                                                       l20_11946
                                                                                                       st11
                                                                                                       st20))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11950
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11951
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateAfter
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11950
                                                                                                     l11
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11951)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateAfter
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_11952, v_11953)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    pure v_11952
                                                                                    <*>
                                                                                    mlbs v_11953
                                                                                      l20)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_11955
                                                                              f@(Phi l20_11954) ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11955,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (PhiStepCont
                                                                                            l20_11954
                                                                                            st20))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest v_11956 ->
                                                                                 (pure Phi <*>
                                                                                    mlbs v_11956
                                                                                      l20)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsPhi db)),
                                                                         foldl'
                                                                           (\ q_11958
                                                                              f@(Cond lcond0 e0 jlt0
                                                                                   l20_11957)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11958,
                                                                                    foldl'
                                                                                      (\ q_11960
                                                                                         f@(StateBefore
                                                                                              lcond0_11959
                                                                                              stc0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11960,
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
                                                                                                      [q_11960,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               lcond0_11959
                                                                                                               stc0
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               l20_11957
                                                                                                               st20))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11961
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11962
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11961
                                                                                                     lcond0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11962)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_11963 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_11964,
                                                                                       v_11965,
                                                                                       v_11966)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_11963
                                                                                        <*>
                                                                                        pure v_11964
                                                                                        <*>
                                                                                        pure v_11965
                                                                                        <*>
                                                                                        mlbs v_11966
                                                                                          l20)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_11968
                                                                              f@(Cond l20_11967 e0
                                                                                   jlt0 jlf0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11968,
                                                                                    foldl'
                                                                                      (\ q_11970
                                                                                         f@(StateBefore
                                                                                              jlf0_11969
                                                                                              stf0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11970,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e0
                                                                                                      st20)
                                                                                                   False
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_11970,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               l20_11967
                                                                                                               st20
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               jlf0_11969
                                                                                                               stf0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11971
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11972
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11971
                                                                                                     jlf0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11972)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_11973 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_11974,
                                                                                       v_11975,
                                                                                       v_11976)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        mlbs v_11973
                                                                                          l20
                                                                                        <*>
                                                                                        pure v_11974
                                                                                        <*>
                                                                                        pure v_11975
                                                                                        <*>
                                                                                        pure
                                                                                          v_11976)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_11978
                                                                              f@(Cond lcond1 e1
                                                                                   l20_11977 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11978,
                                                                                    foldl'
                                                                                      (\ q_11980
                                                                                         f@(StateBefore
                                                                                              lcond1_11979
                                                                                              stc1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11980,
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
                                                                                                      [q_11980,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               lcond1_11979
                                                                                                               stc1
                                                                                                               e1
                                                                                                               l20_11977
                                                                                                               jlf1
                                                                                                               st20))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11981
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11982
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11981
                                                                                                     lcond1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11982)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_11983 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_11984,
                                                                                       v_11985,
                                                                                       v_11986)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_11983
                                                                                        <*>
                                                                                        pure v_11984
                                                                                        <*>
                                                                                        mlbs v_11985
                                                                                          l20
                                                                                        <*>
                                                                                        pure
                                                                                          v_11986)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_11988
                                                                              f@(Cond l20_11987 e1
                                                                                   jlt1 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11988,
                                                                                    foldl'
                                                                                      (\ q_11990
                                                                                         f@(StateBefore
                                                                                              jlt1_11989
                                                                                              stt0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_11990,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e1
                                                                                                      st20)
                                                                                                   True
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_11990,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               l20_11987
                                                                                                               st20
                                                                                                               e1
                                                                                                               jlt1_11989
                                                                                                               jlf1
                                                                                                               stt0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_11991
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_11992
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_11991
                                                                                                     jlt1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_11992)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_11993 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_11994,
                                                                                       v_11995,
                                                                                       v_11996)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        mlbs v_11993
                                                                                          l20
                                                                                        <*>
                                                                                        pure v_11994
                                                                                        <*>
                                                                                        pure v_11995
                                                                                        <*>
                                                                                        pure
                                                                                          v_11996)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_11998
                                                                              f@(Assign l20_11997 x0
                                                                                   e2)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_11998,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (AssignStepCont
                                                                                            l20_11997
                                                                                            x0
                                                                                            e2
                                                                                            st20))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest
                                                                                 (v_11999, v_12000)
                                                                                 vals ->
                                                                                 concatMap
                                                                                   (\ v_12001 ->
                                                                                      pure Assign
                                                                                        <*>
                                                                                        mlbs v_11999
                                                                                          l20
                                                                                        <*>
                                                                                        pure v_12000
                                                                                        <*>
                                                                                        pure
                                                                                          v_12001)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsAssign db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_12002 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_12003 ->
                                                                           StateBefore v_12002
                                                                             v_12003
                                                                             : acc)
                                                                        []
                                                                        vals
                                                                        ++ rest)
                                                                   []
                                                                   (M.singleton v_11935
                                                                      (S.singleton v_11936)))]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_12004 db
  = M.foldlWithKey'
      (\ rest v_12006 vals ->
         concatMap
           (\ v_12007 ->
              pure StateAfter <*> mlbs v_12006 v_12004 <*> pure v_12007)
           vals
           ++ rest)
      []
      (factsStateAfter db)

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