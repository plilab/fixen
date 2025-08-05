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
                  | PhiStepCont Natural IState Natural
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondTrueCont Natural IState Expr Natural Natural IState
                  | PhiInitCont Natural
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l4 x2 e4)
  = [StateBeforeFact (StateBefore l4 (emptyI 0))]
evaluate db
  (AssignStepCont lAssign0 x0 e2 stAssign0 lAfter1 stAfter0)
  = [StateBeforeFact
       (StateBefore lAfter1
          (joinI (insertI x0 (evalI e2 stAssign0) stAssign0) stAfter0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (joinI (narrowConditionalIFalse e0 stc0) stf0))]
evaluate db (PhiStepCont l0 st0 lAfter0)
  = [StateBeforeFact (StateBefore lAfter0 st0)]
evaluate db (CondInitCont l3 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l3 (emptyI 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (joinI (narrowConditionalI e1 stc1) stt0))]
evaluate db (PhiInitCont l2)
  = [StateBeforeFact (StateBefore l2 (emptyI 0))]
evaluate db (VarInitCont l1 x1)
  = [StateBeforeFact (StateBefore l1 (emptyI 0))]

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
step db fact q_1003
  = case fact of
        SeqFact f@(Seq v_1004 v_1005) -> Q.unions
                                           [q_1003,
                                            foldl'
                                              (\ q_1006 f@(Seq l0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1006,
                                                       foldl'
                                                         (\ q_1008 f@(Phi l0_1007) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1008,
                                                                  foldl'
                                                                    (\ q_1010
                                                                       f@(StateBefore l0_1007_1009
                                                                            st0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1010,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (PhiStepCont
                                                                                     l0_1007_1009
                                                                                     st0
                                                                                     lAfter0))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1011 vals ->
                                                                          concatMap
                                                                            (\ v_1012 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1011 l0_1007
                                                                                 <*> pure v_1012)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (foldl'
                                                            (\ rest v_1013 ->
                                                               (pure Phi <*> mlbs v_1013 l0) ++
                                                                 rest)
                                                            []
                                                            (factsPhi db)),
                                                       foldl'
                                                         (\ q_1015 f@(Assign l0_1014 x0 e2) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1015,
                                                                  foldl'
                                                                    (\ q_1017
                                                                       f@(StateBefore lAfter0_1016
                                                                            stAfter0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1017,
                                                                             foldl'
                                                                               (\ q_1019
                                                                                  f@(StateBefore
                                                                                       l0_1014_1018
                                                                                       stAssign0)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_1019,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                l0_1014_1018
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_1016
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest v_1020
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_1021 ->
                                                                                          pure
                                                                                            StateBefore
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_1020
                                                                                              l0_1014
                                                                                            <*>
                                                                                            pure
                                                                                              v_1021)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsStateBefore
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1022 vals ->
                                                                          concatMap
                                                                            (\ v_1023 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1022 lAfter0
                                                                                 <*> pure v_1023)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest (v_1024, v_1025) vals ->
                                                               concatMap
                                                                 (\ v_1026 ->
                                                                    pure Assign <*> mlbs v_1024 l0
                                                                      <*> pure v_1025
                                                                      <*> pure v_1026)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsAssign db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1027, v_1028) ->
                                                    Seq v_1027 v_1028 : rest)
                                                 []
                                                 (S.singleton (v_1004, v_1005)))]
        PhiFact f@(Phi v_1029) -> Q.unions
                                    [q_1003,
                                     foldl'
                                       (\ q_1030 f@(Phi l0) ->
                                          trace ("got: " ++ show f)
                                            (Q.unions
                                               [q_1030,
                                                foldl'
                                                  (\ q_1032 f@(Seq l0_1031 lAfter0) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_1032,
                                                           foldl'
                                                             (\ q_1034
                                                                f@(StateBefore l0_1031_1033 st0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_1034,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (PhiStepCont l0_1031_1033
                                                                              st0
                                                                              lAfter0))]))
                                                             Q.empty
                                                             (M.foldlWithKey'
                                                                (\ rest v_1035 vals ->
                                                                   concatMap
                                                                     (\ v_1036 ->
                                                                        pure StateBefore <*>
                                                                          mlbs v_1035 l0_1031
                                                                          <*> pure v_1036)
                                                                     vals
                                                                     ++ rest)
                                                                []
                                                                (factsStateBefore db))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (v_1037, v_1038) ->
                                                        (pure Seq <*> mlbs v_1037 l0 <*>
                                                           pure v_1038)
                                                          ++ rest)
                                                     []
                                                     (factsSeq db)),
                                                Q.singleton (traceConclusion (PhiInitCont l0))]))
                                       Q.empty
                                       (S.foldl' (\ rest v_1039 -> Phi v_1039 : rest) []
                                          (S.singleton v_1029))]
        CondFact f@(Cond v_1040 v_1041 v_1042 v_1043) -> Q.unions
                                                           [q_1003,
                                                            foldl'
                                                              (\ q_1044 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1044,
                                                                       foldl'
                                                                         (\ q_1046
                                                                            f@(StateBefore jlf0_1045
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1046,
                                                                                  foldl'
                                                                                    (\ q_1048
                                                                                       f@(StateBefore
                                                                                            lcond0_1047
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1048,
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
                                                                                                    [q_1048,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1047
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1045
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1049
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1050
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1049
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1050)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1051 vals ->
                                                                               concatMap
                                                                                 (\ v_1052 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1051
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1052)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1054
                                                                            f@(StateBefore jlt0_1053
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1054,
                                                                                  foldl'
                                                                                    (\ q_1056
                                                                                       f@(StateBefore
                                                                                            lcond0_1055
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1056,
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
                                                                                                    [q_1056,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond0_1055
                                                                                                             stc1
                                                                                                             e0
                                                                                                             jlt0_1053
                                                                                                             jlf0
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1057
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1058
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1057
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1058)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1059 vals ->
                                                                               concatMap
                                                                                 (\ v_1060 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1059
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_1060)
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
                                                                 (\ rest v_1061 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1062, v_1063, v_1064) ->
                                                                         Cond v_1061 v_1062 v_1063
                                                                           v_1064
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1040
                                                                    (S.singleton
                                                                       (v_1041, v_1042, v_1043))))]
        AssignFact f@(Assign v_1065 v_1066 v_1067) -> Q.unions
                                                        [q_1003,
                                                         foldl'
                                                           (\ q_1068 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1068,
                                                                    foldl'
                                                                      (\ q_1069
                                                                         f@(StateBefore lAfter1
                                                                              stAfter0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1069,
                                                                               foldl'
                                                                                 (\ q_1071
                                                                                    f@(StateBefore
                                                                                         lAssign0_1070
                                                                                         stAssign0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_1071,
                                                                                          foldl'
                                                                                            (\ q_1074
                                                                                               f@(Seq
                                                                                                    lAssign0_1070_1072
                                                                                                    lAfter1_1073)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1074,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_1070_1072
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter1_1073
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_1075,
                                                                                                   v_1076)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1075
                                                                                                       lAssign0_1070
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1076
                                                                                                       lAfter1)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_1077
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_1078
                                                                                            ->
                                                                                            pure
                                                                                              StateBefore
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_1077
                                                                                                lAssign0
                                                                                              <*>
                                                                                              pure
                                                                                                v_1078)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1079 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_1080 ->
                                                                                 StateBefore v_1079
                                                                                   v_1080
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
                                                              (\ rest (v_1081, v_1082) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1083 ->
                                                                      Assign v_1081 v_1082 v_1083 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1065, v_1066)
                                                                 (S.singleton v_1067)))]
        VarFact f@(Var v_1084 v_1085) -> Q.unions
                                           [q_1003,
                                            foldl'
                                              (\ q_1086 f@(Var l1 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1086,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l1 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1087, v_1088) ->
                                                    Var v_1087 v_1088 : rest)
                                                 []
                                                 (S.singleton (v_1084, v_1085)))]
        StateBeforeFact f@(StateBefore v_1089 v_1090) -> Q.unions
                                                           [q_1003,
                                                            foldl'
                                                              (\ q_1091 f@(StateBefore l0 st0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1091,
                                                                       foldl'
                                                                         (\ q_1093
                                                                            f@(Seq l0_1092 lAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1093,
                                                                                  foldl'
                                                                                    (\ q_1095
                                                                                       f@(Phi
                                                                                            l0_1092_1094)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1095,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (PhiStepCont
                                                                                                     l0_1092_1094
                                                                                                     st0
                                                                                                     lAfter0))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          v_1096 ->
                                                                                          (pure Phi
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1096
                                                                                               l0_1092)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsPhi
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1097, v_1098)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_1097 l0
                                                                                  <*> pure v_1098)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_1100
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 l0_1099)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1100,
                                                                                  foldl'
                                                                                    (\ q_1102
                                                                                       f@(StateBefore
                                                                                            lcond0_1101
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1102,
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
                                                                                                    [q_1102,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1101
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             l0_1099
                                                                                                             st0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1103
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1104
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1103
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1104)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1105 vals ->
                                                                               concatMap
                                                                                 (\ (v_1106, v_1107,
                                                                                     v_1108)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1105
                                                                                      <*>
                                                                                      pure v_1106
                                                                                      <*>
                                                                                      pure v_1107
                                                                                      <*>
                                                                                      mlbs v_1108
                                                                                        l0)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1109
                                                                            f@(StateBefore jlf0
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1109,
                                                                                  foldl'
                                                                                    (\ q_1112
                                                                                       f@(Cond
                                                                                            l0_1110
                                                                                            e0 jlt0
                                                                                            jlf0_1111)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1112,
                                                                                             if
                                                                                               leq
                                                                                                 BFalse
                                                                                                 (evaluateConditional
                                                                                                    e0
                                                                                                    st0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1112,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             l0_1110
                                                                                                             st0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1111
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1113
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1114,
                                                                                                v_1115,
                                                                                                v_1116)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1113
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1114
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1115
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1116
                                                                                                   jlf0)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1117 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1118 ->
                                                                                    StateBefore
                                                                                      v_1117
                                                                                      v_1118
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1120
                                                                            f@(Cond lcond1 e1
                                                                                 l0_1119 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1120,
                                                                                  foldl'
                                                                                    (\ q_1122
                                                                                       f@(StateBefore
                                                                                            lcond1_1121
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1122,
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
                                                                                                    [q_1122,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond1_1121
                                                                                                             stc1
                                                                                                             e1
                                                                                                             l0_1119
                                                                                                             jlf1
                                                                                                             st0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1123
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1124
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1123
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1124)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1125 vals ->
                                                                               concatMap
                                                                                 (\ (v_1126, v_1127,
                                                                                     v_1128)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1125
                                                                                      <*>
                                                                                      pure v_1126
                                                                                      <*>
                                                                                      mlbs v_1127 l0
                                                                                      <*>
                                                                                      pure v_1128)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1129
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1129,
                                                                                  foldl'
                                                                                    (\ q_1132
                                                                                       f@(Cond
                                                                                            l0_1130
                                                                                            e1
                                                                                            jlt1_1131
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1132,
                                                                                             if
                                                                                               leq
                                                                                                 BTrue
                                                                                                 (evaluateConditional
                                                                                                    e1
                                                                                                    st0)
                                                                                               then
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1132,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             l0_1130
                                                                                                             st0
                                                                                                             e1
                                                                                                             jlt1_1131
                                                                                                             jlf1
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1133
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1134,
                                                                                                v_1135,
                                                                                                v_1136)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1133
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1134
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1135
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1136)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1137 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1138 ->
                                                                                    StateBefore
                                                                                      v_1137
                                                                                      v_1138
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1139
                                                                            f@(StateBefore lAssign0
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1139,
                                                                                  foldl'
                                                                                    (\ q_1141
                                                                                       f@(Assign
                                                                                            lAssign0_1140
                                                                                            x0 e2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1141,
                                                                                             foldl'
                                                                                               (\ q_1144
                                                                                                  f@(Seq
                                                                                                       lAssign0_1140_1142
                                                                                                       l0_1143)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1144,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_1140_1142
                                                                                                                x0
                                                                                                                e2
                                                                                                                stAssign0
                                                                                                                l0_1143
                                                                                                                st0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_1145,
                                                                                                      v_1146)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1145
                                                                                                          lAssign0_1140
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1146
                                                                                                          l0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          (v_1147,
                                                                                           v_1148)
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1149
                                                                                               ->
                                                                                               pure
                                                                                                 Assign
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1147
                                                                                                   lAssign0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1148
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1149)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsAssign
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1150 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1151 ->
                                                                                    StateBefore
                                                                                      v_1150
                                                                                      v_1151
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1153
                                                                            f@(Assign l0_1152 x0 e2)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1153,
                                                                                  foldl'
                                                                                    (\ q_1155
                                                                                       f@(Seq
                                                                                            l0_1152_1154
                                                                                            lAfter1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1155,
                                                                                             foldl'
                                                                                               (\ q_1157
                                                                                                  f@(StateBefore
                                                                                                       lAfter1_1156
                                                                                                       stAfter0)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1157,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                l0_1152_1154
                                                                                                                x0
                                                                                                                e2
                                                                                                                st0
                                                                                                                lAfter1_1156
                                                                                                                stAfter0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     v_1158
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1159
                                                                                                          ->
                                                                                                          pure
                                                                                                            StateBefore
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1158
                                                                                                              lAfter1
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1159)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsStateBefore
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_1160,
                                                                                           v_1161)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1160
                                                                                               l0_1152
                                                                                             <*>
                                                                                             pure
                                                                                               v_1161)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest (v_1162, v_1163)
                                                                               vals ->
                                                                               concatMap
                                                                                 (\ v_1164 ->
                                                                                    pure Assign <*>
                                                                                      mlbs v_1162 l0
                                                                                      <*>
                                                                                      pure v_1163
                                                                                      <*>
                                                                                      pure v_1164)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsAssign db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1165 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1166 ->
                                                                         StateBefore v_1165 v_1166 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1089
                                                                    (S.singleton v_1090)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_1168 vals ->
         S.foldl' (\ acc v_1169 -> StateBefore v_1168 v_1169 : acc) [] vals
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