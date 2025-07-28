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
step db fact q_8988
  = case fact of
        SeqFact f@(Seq v_8989 v_8990) -> Q.unions
                                           [q_8988,
                                            foldl'
                                              (\ q_8991 f@(Seq l10 l20) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_8991,
                                                       foldl'
                                                         (\ q_8993 f@(StateBefore l10_8992 st10) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_8993,
                                                                  foldl'
                                                                    (\ q_8995
                                                                       f@(StateBefore l20_8994 st20)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_8995,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (SeqStepCont
                                                                                     l10_8992
                                                                                     l20_8994
                                                                                     st10
                                                                                     st20))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_8996 vals ->
                                                                          concatMap
                                                                            (\ v_8997 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_8996 l20
                                                                                 <*> pure v_8997)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_8998 vals ->
                                                               concatMap
                                                                 (\ v_8999 ->
                                                                    pure StateBefore <*>
                                                                      mlbs v_8998 l10
                                                                      <*> pure v_8999)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateBefore db)),
                                                       foldl'
                                                         (\ q_9001
                                                            f@(StateBefore l10_9000 stBefore0) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_9001,
                                                                  foldl'
                                                                    (\ q_9003
                                                                       f@(StateBefore l20_9002 stA0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_9003,
                                                                             foldl'
                                                                               (\ q_9005
                                                                                  f@(Assign
                                                                                       l10_9000_9004
                                                                                       x0 e2)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_9005,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                l10_9000_9004
                                                                                                l20_9002
                                                                                                x0
                                                                                                e2
                                                                                                stA0
                                                                                                stBefore0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest
                                                                                     (v_9006,
                                                                                      v_9007)
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_9008 ->
                                                                                          pure
                                                                                            Assign
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_9006
                                                                                              l10_9000
                                                                                            <*>
                                                                                            pure
                                                                                              v_9007
                                                                                            <*>
                                                                                            pure
                                                                                              v_9008)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsAssign
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_9009 vals ->
                                                                          concatMap
                                                                            (\ v_9010 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_9009 l20
                                                                                 <*> pure v_9010)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_9011 vals ->
                                                               concatMap
                                                                 (\ v_9012 ->
                                                                    pure StateBefore <*>
                                                                      mlbs v_9011 l10
                                                                      <*> pure v_9012)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateBefore db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_9013, v_9014) ->
                                                    Seq v_9013 v_9014 : rest)
                                                 []
                                                 (S.singleton (v_8989, v_8990)))]
        PhiFact f@(Phi v_9015) -> Q.unions
                                    [q_8988,
                                     foldl'
                                       (\ q_9016 f@(Phi l0) ->
                                          trace ("got: " ++ show f)
                                            (Q.unions
                                               [q_9016,
                                                Q.singleton (traceConclusion (PhiInitCont l0))]))
                                       Q.empty
                                       (S.foldl' (\ rest v_9017 -> Phi v_9017 : rest) []
                                          (S.singleton v_9015))]
        CondFact f@(Cond v_9018 v_9019 v_9020 v_9021) -> Q.unions
                                                           [q_8988,
                                                            foldl'
                                                              (\ q_9022 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_9022,
                                                                       foldl'
                                                                         (\ q_9024
                                                                            f@(StateBefore jlf0_9023
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9024,
                                                                                  foldl'
                                                                                    (\ q_9026
                                                                                       f@(StateBefore
                                                                                            lcond0_9025
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9026,
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
                                                                                                    [q_9026,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_9025
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_9023
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9027
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9028
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9027
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9028)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9029 vals ->
                                                                               concatMap
                                                                                 (\ v_9030 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_9029
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_9030)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_9032
                                                                            f@(StateBefore jlt0_9031
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9032,
                                                                                  foldl'
                                                                                    (\ q_9034
                                                                                       f@(StateBefore
                                                                                            lcond0_9033
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9034,
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
                                                                                                    [q_9034,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond0_9033
                                                                                                             stc1
                                                                                                             e0
                                                                                                             jlt0_9031
                                                                                                             jlf0
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9035
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9036
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9035
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9036)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9037 vals ->
                                                                               concatMap
                                                                                 (\ v_9038 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_9037
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_9038)
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
                                                                 (\ rest v_9039 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_9040, v_9041, v_9042) ->
                                                                         Cond v_9039 v_9040 v_9041
                                                                           v_9042
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_9018
                                                                    (S.singleton
                                                                       (v_9019, v_9020, v_9021))))]
        AssignFact f@(Assign v_9043 v_9044 v_9045) -> Q.unions
                                                        [q_8988,
                                                         foldl'
                                                           (\ q_9046 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_9046,
                                                                    foldl'
                                                                      (\ q_9048
                                                                         f@(StateBefore
                                                                              lAssign0_9047
                                                                              stBefore0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_9048,
                                                                               foldl'
                                                                                 (\ q_9049
                                                                                    f@(StateBefore
                                                                                         lAfter0
                                                                                         stA0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_9049,
                                                                                          foldl'
                                                                                            (\ q_9052
                                                                                               f@(Seq
                                                                                                    lAssign0_9047_9050
                                                                                                    lAfter0_9051)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_9052,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_9047_9050
                                                                                                             lAfter0_9051
                                                                                                             x0
                                                                                                             e2
                                                                                                             stA0
                                                                                                             stBefore0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_9053,
                                                                                                   v_9054)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_9053
                                                                                                       lAssign0_9047
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_9054
                                                                                                       lAfter0)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_9055
                                                                                       vals ->
                                                                                       S.foldl'
                                                                                         (\ acc
                                                                                            v_9056
                                                                                            ->
                                                                                            StateBefore
                                                                                              v_9055
                                                                                              v_9056
                                                                                              : acc)
                                                                                         []
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_9057 vals ->
                                                                            concatMap
                                                                              (\ v_9058 ->
                                                                                 pure StateBefore
                                                                                   <*>
                                                                                   mlbs v_9057
                                                                                     lAssign0
                                                                                   <*> pure v_9058)
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
                                                              (\ rest (v_9059, v_9060) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_9061 ->
                                                                      Assign v_9059 v_9060 v_9061 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_9043, v_9044)
                                                                 (S.singleton v_9045)))]
        VarFact f@(Var v_9062 v_9063) -> Q.unions
                                           [q_8988,
                                            foldl'
                                              (\ q_9064 f@(Var l3 x2) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_9064,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l3 x2))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_9065, v_9066) ->
                                                    Var v_9065 v_9066 : rest)
                                                 []
                                                 (S.singleton (v_9062, v_9063)))]
        StateBeforeFact f@(StateBefore v_9067 v_9068) -> Q.unions
                                                           [q_8988,
                                                            foldl'
                                                              (\ q_9069 f@(StateBefore l10 st10) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_9069,
                                                                       foldl'
                                                                         (\ q_9071
                                                                            f@(Seq l10_9070 l20) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9071,
                                                                                  foldl'
                                                                                    (\ q_9073
                                                                                       f@(StateBefore
                                                                                            l20_9072
                                                                                            st20)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9073,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepCont
                                                                                                     l10_9070
                                                                                                     l20_9072
                                                                                                     st10
                                                                                                     st20))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9074
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9075
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9074
                                                                                                   l20
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9075)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_9076, v_9077)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_9076 l10
                                                                                  <*> pure v_9077)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_9078
                                                                            f@(StateBefore l20 st20)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9078,
                                                                                  foldl'
                                                                                    (\ q_9081
                                                                                       f@(Seq
                                                                                            l20_9079
                                                                                            l10_9080)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9081,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepCont
                                                                                                     l20_9079
                                                                                                     l10_9080
                                                                                                     st20
                                                                                                     st10))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_9082,
                                                                                           v_9083)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_9082
                                                                                               l20
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_9083
                                                                                               l10)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9084 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_9085 ->
                                                                                    StateBefore
                                                                                      v_9084
                                                                                      v_9085
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_9087
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 l10_9086)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9087,
                                                                                  foldl'
                                                                                    (\ q_9089
                                                                                       f@(StateBefore
                                                                                            lcond0_9088
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9089,
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
                                                                                                    [q_9089,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_9088
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             l10_9086
                                                                                                             st10))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9090
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9091
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9090
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9091)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9092 vals ->
                                                                               concatMap
                                                                                 (\ (v_9093, v_9094,
                                                                                     v_9095)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_9092
                                                                                      <*>
                                                                                      pure v_9093
                                                                                      <*>
                                                                                      pure v_9094
                                                                                      <*>
                                                                                      mlbs v_9095
                                                                                        l10)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_9096
                                                                            f@(StateBefore jlf0
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9096,
                                                                                  foldl'
                                                                                    (\ q_9099
                                                                                       f@(Cond
                                                                                            l10_9097
                                                                                            e0 jlt0
                                                                                            jlf0_9098)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9099,
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
                                                                                                    [q_9099,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             l10_9097
                                                                                                             st10
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_9098
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9100
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_9101,
                                                                                                v_9102,
                                                                                                v_9103)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9100
                                                                                                   l10
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9101
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9102
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9103
                                                                                                   jlf0)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9104 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_9105 ->
                                                                                    StateBefore
                                                                                      v_9104
                                                                                      v_9105
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_9107
                                                                            f@(Cond lcond1 e1
                                                                                 l10_9106 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9107,
                                                                                  foldl'
                                                                                    (\ q_9109
                                                                                       f@(StateBefore
                                                                                            lcond1_9108
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9109,
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
                                                                                                    [q_9109,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond1_9108
                                                                                                             stc1
                                                                                                             e1
                                                                                                             l10_9106
                                                                                                             jlf1
                                                                                                             st10))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9110
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9111
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9110
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9111)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9112 vals ->
                                                                               concatMap
                                                                                 (\ (v_9113, v_9114,
                                                                                     v_9115)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_9112
                                                                                      <*>
                                                                                      pure v_9113
                                                                                      <*>
                                                                                      mlbs v_9114
                                                                                        l10
                                                                                      <*>
                                                                                      pure v_9115)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_9116
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9116,
                                                                                  foldl'
                                                                                    (\ q_9119
                                                                                       f@(Cond
                                                                                            l10_9117
                                                                                            e1
                                                                                            jlt1_9118
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9119,
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
                                                                                                    [q_9119,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             l10_9117
                                                                                                             st10
                                                                                                             e1
                                                                                                             jlt1_9118
                                                                                                             jlf1
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9120
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_9121,
                                                                                                v_9122,
                                                                                                v_9123)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9120
                                                                                                   l10
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9121
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9122
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9123)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9124 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_9125 ->
                                                                                    StateBefore
                                                                                      v_9124
                                                                                      v_9125
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_9127
                                                                            f@(Seq l10_9126 lAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9127,
                                                                                  foldl'
                                                                                    (\ q_9129
                                                                                       f@(StateBefore
                                                                                            lAfter0_9128
                                                                                            stA0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9129,
                                                                                             foldl'
                                                                                               (\ q_9131
                                                                                                  f@(Assign
                                                                                                       l10_9126_9130
                                                                                                       x0
                                                                                                       e2)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_9131,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                l10_9126_9130
                                                                                                                lAfter0_9128
                                                                                                                x0
                                                                                                                e2
                                                                                                                stA0
                                                                                                                st10))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     (v_9132,
                                                                                                      v_9133)
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_9134
                                                                                                          ->
                                                                                                          pure
                                                                                                            Assign
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_9132
                                                                                                              l10_9126
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_9133
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_9134)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsAssign
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_9135
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_9136
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_9135
                                                                                                   lAfter0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_9136)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_9137, v_9138)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_9137 l10
                                                                                  <*> pure v_9138)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_9139
                                                                            f@(StateBefore lAssign0
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_9139,
                                                                                  foldl'
                                                                                    (\ q_9142
                                                                                       f@(Seq
                                                                                            lAssign0_9140
                                                                                            l10_9141)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_9142,
                                                                                             foldl'
                                                                                               (\ q_9144
                                                                                                  f@(Assign
                                                                                                       lAssign0_9140_9143
                                                                                                       x0
                                                                                                       e2)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_9144,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_9140_9143
                                                                                                                l10_9141
                                                                                                                x0
                                                                                                                e2
                                                                                                                st10
                                                                                                                stBefore0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     (v_9145,
                                                                                                      v_9146)
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_9147
                                                                                                          ->
                                                                                                          pure
                                                                                                            Assign
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_9145
                                                                                                              lAssign0_9140
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_9146
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_9147)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsAssign
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_9148,
                                                                                           v_9149)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_9148
                                                                                               lAssign0
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_9149
                                                                                               l10)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_9150 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_9151 ->
                                                                                    StateBefore
                                                                                      v_9150
                                                                                      v_9151
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore
                                                                               db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_9152 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_9153 ->
                                                                         StateBefore v_9152 v_9153 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_9067
                                                                    (S.singleton v_9068)))]

stateBefore :: Natural -> DataBase -> [StateBefore]
stateBefore v_9154 db
  = M.foldlWithKey'
      (\ rest v_9156 vals ->
         concatMap
           (\ v_9157 ->
              pure StateBefore <*> mlbs v_9156 v_9154 <*> pure v_9157)
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