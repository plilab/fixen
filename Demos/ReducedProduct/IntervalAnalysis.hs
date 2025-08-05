{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.IntervalAnalysis where
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
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr)}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase M.empty S.empty S.empty M.empty M.empty

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
step db fact q_1
  = case fact of
        SeqFact f@(Seq v_2 v_3) -> Q.unions
                                     [q_1,
                                      foldl'
                                        (\ q_4 f@(Seq lAssign0 lAfter0) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_4,
                                                 foldl'
                                                   (\ q_6 f@(StateBefore lAfter0_5 stAfter0) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_6,
                                                            foldl'
                                                              (\ q_8
                                                                 f@(StateBefore lAssign0_7
                                                                      stAssign0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_8,
                                                                       foldl'
                                                                         (\ q_10
                                                                            f@(Assign lAssign0_7_9
                                                                                 x0 e2)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_10,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (AssignStepCont
                                                                                          lAssign0_7_9
                                                                                          x0
                                                                                          e2
                                                                                          stAssign0
                                                                                          lAfter0_5
                                                                                          stAfter0))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest (v_11, v_12)
                                                                               vals ->
                                                                               concatMap
                                                                                 (\ v_13 ->
                                                                                    pure Assign <*>
                                                                                      mlbs v_11
                                                                                        lAssign0_7
                                                                                      <*> pure v_12
                                                                                      <*> pure v_13)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsAssign db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_14 vals ->
                                                                    concatMap
                                                                      (\ v_15 ->
                                                                         pure StateBefore <*>
                                                                           mlbs v_14 lAssign0
                                                                           <*> pure v_15)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateBefore db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_16 vals ->
                                                         concatMap
                                                           (\ v_17 ->
                                                              pure StateBefore <*> mlbs v_16 lAfter0
                                                                <*> pure v_17)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateBefore db))]))
                                        Q.empty
                                        (S.foldl' (\ rest (v_18, v_19) -> Seq v_18 v_19 : rest) []
                                           (S.singleton (v_2, v_3)))]
        CondFact f@(Cond v_20 v_21 v_22 v_23) -> Q.unions
                                                   [q_1,
                                                    foldl'
                                                      (\ q_24 f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_24,
                                                               foldl'
                                                                 (\ q_26
                                                                    f@(StateBefore jlf0_25 stf0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_26,
                                                                          foldl'
                                                                            (\ q_28
                                                                               f@(StateBefore
                                                                                    lcond0_27 stc0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_28,
                                                                                     if
                                                                                       leq BFalse
                                                                                         (evaluateConditional
                                                                                            e0
                                                                                            stc0)
                                                                                       then
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_28,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     lcond0_27
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_25
                                                                                                     stf0))])
                                                                                       else
                                                                                       Q.empty]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_29 vals ->
                                                                                  concatMap
                                                                                    (\ v_30 ->
                                                                                       pure
                                                                                         StateBefore
                                                                                         <*>
                                                                                         mlbs v_29
                                                                                           lcond0
                                                                                         <*>
                                                                                         pure v_30)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBefore
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_31 vals ->
                                                                       concatMap
                                                                         (\ v_32 ->
                                                                            pure StateBefore <*>
                                                                              mlbs v_31 jlf0
                                                                              <*> pure v_32)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateBefore db)),
                                                               foldl'
                                                                 (\ q_34
                                                                    f@(StateBefore jlt0_33 stt0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_34,
                                                                          foldl'
                                                                            (\ q_36
                                                                               f@(StateBefore
                                                                                    lcond0_35 stc1)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_36,
                                                                                     if
                                                                                       leq BTrue
                                                                                         (evaluateConditional
                                                                                            e0
                                                                                            stc1)
                                                                                       then
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_36,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     lcond0_35
                                                                                                     stc1
                                                                                                     e0
                                                                                                     jlt0_33
                                                                                                     jlf0
                                                                                                     stt0))])
                                                                                       else
                                                                                       Q.empty]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_37 vals ->
                                                                                  concatMap
                                                                                    (\ v_38 ->
                                                                                       pure
                                                                                         StateBefore
                                                                                         <*>
                                                                                         mlbs v_37
                                                                                           lcond0
                                                                                         <*>
                                                                                         pure v_38)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBefore
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_39 vals ->
                                                                       concatMap
                                                                         (\ v_40 ->
                                                                            pure StateBefore <*>
                                                                              mlbs v_39 jlt0
                                                                              <*> pure v_40)
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
                                                         (\ rest v_41 vals ->
                                                            S.foldl'
                                                              (\ acc (v_42, v_43, v_44) ->
                                                                 Cond v_41 v_42 v_43 v_44 : acc)
                                                              []
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_20
                                                            (S.singleton (v_21, v_22, v_23))))]
        AssignFact f@(Assign v_45 v_46 v_47) -> Q.unions
                                                  [q_1,
                                                   foldl'
                                                     (\ q_48 f@(Assign lAssign0 x0 e2) ->
                                                        trace ("got: " ++ show f)
                                                          (Q.unions
                                                             [q_48,
                                                              foldl'
                                                                (\ q_49
                                                                   f@(StateBefore lAfter0 stAfter0)
                                                                   ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_49,
                                                                         foldl'
                                                                           (\ q_51
                                                                              f@(StateBefore
                                                                                   lAssign0_50
                                                                                   stAssign0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_51,
                                                                                    foldl'
                                                                                      (\ q_54
                                                                                         f@(Seq
                                                                                              lAssign0_50_52
                                                                                              lAfter0_53)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_54,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (AssignStepCont
                                                                                                       lAssign0_50_52
                                                                                                       x0
                                                                                                       e2
                                                                                                       stAssign0
                                                                                                       lAfter0_53
                                                                                                       stAfter0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_55,
                                                                                             v_56)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_55
                                                                                                 lAssign0_50
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_56
                                                                                                 lAfter0)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_57 vals ->
                                                                                 concatMap
                                                                                   (\ v_58 ->
                                                                                      pure
                                                                                        StateBefore
                                                                                        <*>
                                                                                        mlbs v_57
                                                                                          lAssign0
                                                                                        <*>
                                                                                        pure v_58)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_59 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_60 ->
                                                                           StateBefore v_59 v_60 :
                                                                             acc)
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
                                                        (\ rest (v_61, v_62) vals ->
                                                           S.foldl'
                                                             (\ acc v_63 ->
                                                                Assign v_61 v_62 v_63 : acc)
                                                             []
                                                             vals
                                                             ++ rest)
                                                        []
                                                        (M.singleton (v_45, v_46)
                                                           (S.singleton v_47)))]
        VarFact f@(Var v_64 v_65) -> Q.unions
                                       [q_1,
                                        foldl'
                                          (\ q_66 f@(Var l0 x1) ->
                                             trace ("got: " ++ show f)
                                               (Q.unions
                                                  [q_66,
                                                   Q.singleton
                                                     (traceConclusion (VarInitCont l0 x1))]))
                                          Q.empty
                                          (S.foldl' (\ rest (v_67, v_68) -> Var v_67 v_68 : rest) []
                                             (S.singleton (v_64, v_65)))]
        StateBeforeFact f@(StateBefore v_69 v_70) -> Q.unions
                                                       [q_1,
                                                        foldl'
                                                          (\ q_71 f@(StateBefore jlf0 stf0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_71,
                                                                   foldl'
                                                                     (\ q_73
                                                                        f@(Cond lcond0 e0 jlt0
                                                                             jlf0_72)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_73,
                                                                              foldl'
                                                                                (\ q_75
                                                                                   f@(StateBefore
                                                                                        lcond0_74
                                                                                        stc0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_75,
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
                                                                                                [q_75,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondFalseCont
                                                                                                         lcond0_74
                                                                                                         stc0
                                                                                                         e0
                                                                                                         jlt0
                                                                                                         jlf0_72
                                                                                                         stf0))])
                                                                                           else
                                                                                           Q.empty]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_76 vals
                                                                                      ->
                                                                                      concatMap
                                                                                        (\ v_77 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_76
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_77)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_78 vals ->
                                                                           concatMap
                                                                             (\ (v_79, v_80, v_81)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  pure v_78
                                                                                  <*> pure v_79
                                                                                  <*> pure v_80
                                                                                  <*>
                                                                                  mlbs v_81 jlf0)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_83
                                                                        f@(Cond jlf0_82 e0 jlt0
                                                                             lcond0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_83,
                                                                              foldl'
                                                                                (\ q_85
                                                                                   f@(StateBefore
                                                                                        lcond0_84
                                                                                        stc0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_85,
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
                                                                                                [q_85,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondFalseCont
                                                                                                         jlf0_82
                                                                                                         stf0
                                                                                                         e0
                                                                                                         jlt0
                                                                                                         lcond0_84
                                                                                                         stc0))])
                                                                                           else
                                                                                           Q.empty]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_86 vals
                                                                                      ->
                                                                                      concatMap
                                                                                        (\ v_87 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_86
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_87)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_88 vals ->
                                                                           concatMap
                                                                             (\ (v_89, v_90, v_91)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  mlbs v_88 jlf0
                                                                                  <*> pure v_89
                                                                                  <*> pure v_90
                                                                                  <*> pure v_91)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_93
                                                                        f@(Cond lcond1 e1 jlf0_92
                                                                             jlf1)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_93,
                                                                              foldl'
                                                                                (\ q_95
                                                                                   f@(StateBefore
                                                                                        lcond1_94
                                                                                        stc1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_95,
                                                                                         if
                                                                                           leq BTrue
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
                                                                                                [q_95,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondTrueCont
                                                                                                         lcond1_94
                                                                                                         stc1
                                                                                                         e1
                                                                                                         jlf0_92
                                                                                                         jlf1
                                                                                                         stf0))])
                                                                                           else
                                                                                           Q.empty]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_96 vals
                                                                                      ->
                                                                                      concatMap
                                                                                        (\ v_97 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_96
                                                                                               lcond1
                                                                                             <*>
                                                                                             pure
                                                                                               v_97)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_98 vals ->
                                                                           concatMap
                                                                             (\ (v_99, v_100, v_101)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  pure v_98
                                                                                  <*> pure v_99
                                                                                  <*>
                                                                                  mlbs v_100 jlf0
                                                                                  <*> pure v_101)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_102
                                                                        f@(StateBefore jlt1 stt0) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_102,
                                                                              foldl'
                                                                                (\ q_105
                                                                                   f@(Cond jlf0_103
                                                                                        e1 jlt1_104
                                                                                        jlf1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_105,
                                                                                         if
                                                                                           leq BTrue
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
                                                                                                [q_105,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondTrueCont
                                                                                                         jlf0_103
                                                                                                         stf0
                                                                                                         e1
                                                                                                         jlt1_104
                                                                                                         jlf1
                                                                                                         stt0))])
                                                                                           else
                                                                                           Q.empty]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_106
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_107,
                                                                                            v_108,
                                                                                            v_109)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_106
                                                                                               jlf0
                                                                                             <*>
                                                                                             pure
                                                                                               v_107
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_108
                                                                                               jlt1
                                                                                             <*>
                                                                                             pure
                                                                                               v_109)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_110 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_111 ->
                                                                                StateBefore v_110
                                                                                  v_111
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_112
                                                                        f@(StateBefore lAssign0
                                                                             stAssign0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_112,
                                                                              foldl'
                                                                                (\ q_114
                                                                                   f@(Assign
                                                                                        lAssign0_113
                                                                                        x0 e2)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_114,
                                                                                         foldl'
                                                                                           (\ q_117
                                                                                              f@(Seq
                                                                                                   lAssign0_113_115
                                                                                                   jlf0_116)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_117,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (AssignStepCont
                                                                                                            lAssign0_113_115
                                                                                                            x0
                                                                                                            e2
                                                                                                            stAssign0
                                                                                                            jlf0_116
                                                                                                            stf0))]))
                                                                                           Q.empty
                                                                                           (foldl'
                                                                                              (\ rest
                                                                                                 (v_118,
                                                                                                  v_119)
                                                                                                 ->
                                                                                                 (pure
                                                                                                    Seq
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_118
                                                                                                      lAssign0_113
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_119
                                                                                                      jlf0)
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsSeq
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest
                                                                                      (v_120, v_121)
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_122 ->
                                                                                           pure
                                                                                             Assign
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_120
                                                                                               lAssign0
                                                                                             <*>
                                                                                             pure
                                                                                               v_121
                                                                                             <*>
                                                                                             pure
                                                                                               v_122)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsAssign
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_123 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_124 ->
                                                                                StateBefore v_123
                                                                                  v_124
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_125
                                                                        f@(StateBefore lAfter0
                                                                             stAfter0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_125,
                                                                              foldl'
                                                                                (\ q_128
                                                                                   f@(Seq jlf0_126
                                                                                        lAfter0_127)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_128,
                                                                                         foldl'
                                                                                           (\ q_130
                                                                                              f@(Assign
                                                                                                   jlf0_126_129
                                                                                                   x0
                                                                                                   e2)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_130,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (AssignStepCont
                                                                                                            jlf0_126_129
                                                                                                            x0
                                                                                                            e2
                                                                                                            stf0
                                                                                                            lAfter0_127
                                                                                                            stAfter0))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 (v_131,
                                                                                                  v_132)
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ v_133
                                                                                                      ->
                                                                                                      pure
                                                                                                        Assign
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_131
                                                                                                          jlf0_126
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_132
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_133)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsAssign
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (foldl'
                                                                                   (\ rest
                                                                                      (v_134, v_135)
                                                                                      ->
                                                                                      (pure Seq <*>
                                                                                         mlbs v_134
                                                                                           jlf0
                                                                                         <*>
                                                                                         mlbs v_135
                                                                                           lAfter0)
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsSeq db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_136 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_137 ->
                                                                                StateBefore v_136
                                                                                  v_137
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_138 vals ->
                                                                S.foldl'
                                                                  (\ acc v_139 ->
                                                                     StateBefore v_138 v_139 : acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (M.singleton v_69 (S.singleton v_70)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_141 vals ->
         S.foldl' (\ acc v_142 -> StateBefore v_141 v_142 : acc) [] vals ++
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