{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.CongruenceAnalysis where
import ReducedProduct.Congruence
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
step db fact q_1
  = case fact of
        SeqFact f@(Seq v_2 v_3) -> Q.unions
                                     [q_1,
                                      foldl'
                                        (\ q_4 f@(Seq l0 lAfter0) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_4,
                                                 foldl'
                                                   (\ q_6 f@(Phi l0_5) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_6,
                                                            foldl'
                                                              (\ q_8 f@(StateBefore l0_5_7 st0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_8,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (PhiStepCont l0_5_7 st0
                                                                               lAfter0))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_9 vals ->
                                                                    concatMap
                                                                      (\ v_10 ->
                                                                         pure StateBefore <*>
                                                                           mlbs v_9 l0_5
                                                                           <*> pure v_10)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateBefore db))]))
                                                   Q.empty
                                                   (foldl'
                                                      (\ rest v_11 ->
                                                         (pure Phi <*> mlbs v_11 l0) ++ rest)
                                                      []
                                                      (factsPhi db)),
                                                 foldl'
                                                   (\ q_13 f@(Assign l0_12 x0 e2) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_13,
                                                            foldl'
                                                              (\ q_15
                                                                 f@(StateBefore lAfter0_14 stAfter0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_15,
                                                                       foldl'
                                                                         (\ q_17
                                                                            f@(StateBefore l0_12_16
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_17,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (AssignStepCont
                                                                                          l0_12_16
                                                                                          x0
                                                                                          e2
                                                                                          stAssign0
                                                                                          lAfter0_14
                                                                                          stAfter0))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_18 vals ->
                                                                               concatMap
                                                                                 (\ v_19 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_18
                                                                                        l0_12
                                                                                      <*> pure v_19)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore
                                                                               db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_20 vals ->
                                                                    concatMap
                                                                      (\ v_21 ->
                                                                         pure StateBefore <*>
                                                                           mlbs v_20 lAfter0
                                                                           <*> pure v_21)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateBefore db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest (v_22, v_23) vals ->
                                                         concatMap
                                                           (\ v_24 ->
                                                              pure Assign <*> mlbs v_22 l0 <*>
                                                                pure v_23
                                                                <*> pure v_24)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsAssign db))]))
                                        Q.empty
                                        (S.foldl' (\ rest (v_25, v_26) -> Seq v_25 v_26 : rest) []
                                           (S.singleton (v_2, v_3)))]
        PhiFact f@(Phi v_27) -> Q.unions
                                  [q_1,
                                   foldl'
                                     (\ q_28 f@(Phi l0) ->
                                        trace ("got: " ++ show f)
                                          (Q.unions
                                             [q_28,
                                              foldl'
                                                (\ q_30 f@(Seq l0_29 lAfter0) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_30,
                                                         foldl'
                                                           (\ q_32 f@(StateBefore l0_29_31 st0) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_32,
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (PhiStepCont l0_29_31 st0
                                                                            lAfter0))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_33 vals ->
                                                                 concatMap
                                                                   (\ v_34 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_33 l0_29
                                                                        <*> pure v_34)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (foldl'
                                                   (\ rest (v_35, v_36) ->
                                                      (pure Seq <*> mlbs v_35 l0 <*> pure v_36) ++
                                                        rest)
                                                   []
                                                   (factsSeq db)),
                                              Q.singleton (traceConclusion (PhiInitCont l0))]))
                                     Q.empty
                                     (S.foldl' (\ rest v_37 -> Phi v_37 : rest) []
                                        (S.singleton v_27))]
        CondFact f@(Cond v_38 v_39 v_40 v_41) -> Q.unions
                                                   [q_1,
                                                    foldl'
                                                      (\ q_42 f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_42,
                                                               foldl'
                                                                 (\ q_44
                                                                    f@(StateBefore jlf0_43 stf0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_44,
                                                                          foldl'
                                                                            (\ q_46
                                                                               f@(StateBefore
                                                                                    lcond0_45 stc0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_46,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondFalseCont
                                                                                             lcond0_45
                                                                                             stc0
                                                                                             e0
                                                                                             jlt0
                                                                                             jlf0_43
                                                                                             stf0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_47 vals ->
                                                                                  concatMap
                                                                                    (\ v_48 ->
                                                                                       pure
                                                                                         StateBefore
                                                                                         <*>
                                                                                         mlbs v_47
                                                                                           lcond0
                                                                                         <*>
                                                                                         pure v_48)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBefore
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_49 vals ->
                                                                       concatMap
                                                                         (\ v_50 ->
                                                                            pure StateBefore <*>
                                                                              mlbs v_49 jlf0
                                                                              <*> pure v_50)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateBefore db)),
                                                               foldl'
                                                                 (\ q_52
                                                                    f@(StateBefore jlt0_51 stt0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_52,
                                                                          foldl'
                                                                            (\ q_54
                                                                               f@(StateBefore
                                                                                    lcond0_53 stc1)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_54,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondTrueCont
                                                                                             lcond0_53
                                                                                             stc1
                                                                                             e0
                                                                                             jlt0_51
                                                                                             jlf0
                                                                                             stt0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_55 vals ->
                                                                                  concatMap
                                                                                    (\ v_56 ->
                                                                                       pure
                                                                                         StateBefore
                                                                                         <*>
                                                                                         mlbs v_55
                                                                                           lcond0
                                                                                         <*>
                                                                                         pure v_56)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateBefore
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_57 vals ->
                                                                       concatMap
                                                                         (\ v_58 ->
                                                                            pure StateBefore <*>
                                                                              mlbs v_57 jlt0
                                                                              <*> pure v_58)
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
                                                         (\ rest v_59 vals ->
                                                            S.foldl'
                                                              (\ acc (v_60, v_61, v_62) ->
                                                                 Cond v_59 v_60 v_61 v_62 : acc)
                                                              []
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_38
                                                            (S.singleton (v_39, v_40, v_41))))]
        AssignFact f@(Assign v_63 v_64 v_65) -> Q.unions
                                                  [q_1,
                                                   foldl'
                                                     (\ q_66 f@(Assign lAssign0 x0 e2) ->
                                                        trace ("got: " ++ show f)
                                                          (Q.unions
                                                             [q_66,
                                                              foldl'
                                                                (\ q_67
                                                                   f@(StateBefore lAfter1 stAfter0)
                                                                   ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_67,
                                                                         foldl'
                                                                           (\ q_69
                                                                              f@(StateBefore
                                                                                   lAssign0_68
                                                                                   stAssign0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_69,
                                                                                    foldl'
                                                                                      (\ q_72
                                                                                         f@(Seq
                                                                                              lAssign0_68_70
                                                                                              lAfter1_71)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_72,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (AssignStepCont
                                                                                                       lAssign0_68_70
                                                                                                       x0
                                                                                                       e2
                                                                                                       stAssign0
                                                                                                       lAfter1_71
                                                                                                       stAfter0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_73,
                                                                                             v_74)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_73
                                                                                                 lAssign0_68
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_74
                                                                                                 lAfter1)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_75 vals ->
                                                                                 concatMap
                                                                                   (\ v_76 ->
                                                                                      pure
                                                                                        StateBefore
                                                                                        <*>
                                                                                        mlbs v_75
                                                                                          lAssign0
                                                                                        <*>
                                                                                        pure v_76)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_77 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_78 ->
                                                                           StateBefore v_77 v_78 :
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
                                                        (\ rest (v_79, v_80) vals ->
                                                           S.foldl'
                                                             (\ acc v_81 ->
                                                                Assign v_79 v_80 v_81 : acc)
                                                             []
                                                             vals
                                                             ++ rest)
                                                        []
                                                        (M.singleton (v_63, v_64)
                                                           (S.singleton v_65)))]
        VarFact f@(Var v_82 v_83) -> Q.unions
                                       [q_1,
                                        foldl'
                                          (\ q_84 f@(Var l1 x1) ->
                                             trace ("got: " ++ show f)
                                               (Q.unions
                                                  [q_84,
                                                   Q.singleton
                                                     (traceConclusion (VarInitCont l1 x1))]))
                                          Q.empty
                                          (S.foldl' (\ rest (v_85, v_86) -> Var v_85 v_86 : rest) []
                                             (S.singleton (v_82, v_83)))]
        StateBeforeFact f@(StateBefore v_87 v_88) -> Q.unions
                                                       [q_1,
                                                        foldl'
                                                          (\ q_89 f@(StateBefore l0 st0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_89,
                                                                   foldl'
                                                                     (\ q_91 f@(Seq l0_90 lAfter0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_91,
                                                                              foldl'
                                                                                (\ q_93
                                                                                   f@(Phi l0_90_92)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_93,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (PhiStepCont
                                                                                                 l0_90_92
                                                                                                 st0
                                                                                                 lAfter0))]))
                                                                                Q.empty
                                                                                (foldl'
                                                                                   (\ rest v_94 ->
                                                                                      (pure Phi <*>
                                                                                         mlbs v_94
                                                                                           l0_90)
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsPhi db))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest (v_95, v_96) ->
                                                                           (pure Seq <*>
                                                                              mlbs v_95 l0
                                                                              <*> pure v_96)
                                                                             ++ rest)
                                                                        []
                                                                        (factsSeq db)),
                                                                   foldl'
                                                                     (\ q_98
                                                                        f@(Cond lcond0 e0 jlt0
                                                                             l0_97)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_98,
                                                                              foldl'
                                                                                (\ q_100
                                                                                   f@(StateBefore
                                                                                        lcond0_99
                                                                                        stc0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_100,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondFalseCont
                                                                                                 lcond0_99
                                                                                                 stc0
                                                                                                 e0
                                                                                                 jlt0
                                                                                                 l0_97
                                                                                                 st0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_101
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_102 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_101
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_102)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_103 vals ->
                                                                           concatMap
                                                                             (\ (v_104, v_105,
                                                                                 v_106)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  pure v_103
                                                                                  <*> pure v_104
                                                                                  <*> pure v_105
                                                                                  <*> mlbs v_106 l0)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_107
                                                                        f@(StateBefore jlf0 stf0) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_107,
                                                                              foldl'
                                                                                (\ q_110
                                                                                   f@(Cond l0_108 e0
                                                                                        jlt0
                                                                                        jlf0_109)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_110,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondFalseCont
                                                                                                 l0_108
                                                                                                 st0
                                                                                                 e0
                                                                                                 jlt0
                                                                                                 jlf0_109
                                                                                                 stf0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_111
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_112,
                                                                                            v_113,
                                                                                            v_114)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_111
                                                                                               l0
                                                                                             <*>
                                                                                             pure
                                                                                               v_112
                                                                                             <*>
                                                                                             pure
                                                                                               v_113
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_114
                                                                                               jlf0)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_115 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_116 ->
                                                                                StateBefore v_115
                                                                                  v_116
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_118
                                                                        f@(Cond lcond1 e1 l0_117
                                                                             jlf1)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_118,
                                                                              foldl'
                                                                                (\ q_120
                                                                                   f@(StateBefore
                                                                                        lcond1_119
                                                                                        stc1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_120,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondTrueCont
                                                                                                 lcond1_119
                                                                                                 stc1
                                                                                                 e1
                                                                                                 l0_117
                                                                                                 jlf1
                                                                                                 st0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_121
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_122 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_121
                                                                                               lcond1
                                                                                             <*>
                                                                                             pure
                                                                                               v_122)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_123 vals ->
                                                                           concatMap
                                                                             (\ (v_124, v_125,
                                                                                 v_126)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  pure v_123
                                                                                  <*> pure v_124
                                                                                  <*> mlbs v_125 l0
                                                                                  <*> pure v_126)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_127
                                                                        f@(StateBefore jlt1 stt0) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_127,
                                                                              foldl'
                                                                                (\ q_130
                                                                                   f@(Cond l0_128 e1
                                                                                        jlt1_129
                                                                                        jlf1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_130,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondTrueCont
                                                                                                 l0_128
                                                                                                 st0
                                                                                                 e1
                                                                                                 jlt1_129
                                                                                                 jlf1
                                                                                                 stt0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_131
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_132,
                                                                                            v_133,
                                                                                            v_134)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_131
                                                                                               l0
                                                                                             <*>
                                                                                             pure
                                                                                               v_132
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_133
                                                                                               jlt1
                                                                                             <*>
                                                                                             pure
                                                                                               v_134)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_135 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_136 ->
                                                                                StateBefore v_135
                                                                                  v_136
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_137
                                                                        f@(StateBefore lAssign0
                                                                             stAssign0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_137,
                                                                              foldl'
                                                                                (\ q_139
                                                                                   f@(Assign
                                                                                        lAssign0_138
                                                                                        x0 e2)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_139,
                                                                                         foldl'
                                                                                           (\ q_142
                                                                                              f@(Seq
                                                                                                   lAssign0_138_140
                                                                                                   l0_141)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_142,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (AssignStepCont
                                                                                                            lAssign0_138_140
                                                                                                            x0
                                                                                                            e2
                                                                                                            stAssign0
                                                                                                            l0_141
                                                                                                            st0))]))
                                                                                           Q.empty
                                                                                           (foldl'
                                                                                              (\ rest
                                                                                                 (v_143,
                                                                                                  v_144)
                                                                                                 ->
                                                                                                 (pure
                                                                                                    Seq
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_143
                                                                                                      lAssign0_138
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_144
                                                                                                      l0)
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsSeq
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest
                                                                                      (v_145, v_146)
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_147 ->
                                                                                           pure
                                                                                             Assign
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_145
                                                                                               lAssign0
                                                                                             <*>
                                                                                             pure
                                                                                               v_146
                                                                                             <*>
                                                                                             pure
                                                                                               v_147)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsAssign
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_148 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_149 ->
                                                                                StateBefore v_148
                                                                                  v_149
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_151
                                                                        f@(Assign l0_150 x0 e2) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_151,
                                                                              foldl'
                                                                                (\ q_153
                                                                                   f@(Seq l0_150_152
                                                                                        lAfter1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_153,
                                                                                         foldl'
                                                                                           (\ q_155
                                                                                              f@(StateBefore
                                                                                                   lAfter1_154
                                                                                                   stAfter0)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_155,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (AssignStepCont
                                                                                                            l0_150_152
                                                                                                            x0
                                                                                                            e2
                                                                                                            st0
                                                                                                            lAfter1_154
                                                                                                            stAfter0))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 v_156
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ v_157
                                                                                                      ->
                                                                                                      pure
                                                                                                        StateBefore
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_156
                                                                                                          lAfter1
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_157)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsStateBefore
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (foldl'
                                                                                   (\ rest
                                                                                      (v_158, v_159)
                                                                                      ->
                                                                                      (pure Seq <*>
                                                                                         mlbs v_158
                                                                                           l0_150
                                                                                         <*>
                                                                                         pure v_159)
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsSeq db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest (v_160, v_161) vals
                                                                           ->
                                                                           concatMap
                                                                             (\ v_162 ->
                                                                                pure Assign <*>
                                                                                  mlbs v_160 l0
                                                                                  <*> pure v_161
                                                                                  <*> pure v_162)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsAssign db))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_163 vals ->
                                                                S.foldl'
                                                                  (\ acc v_164 ->
                                                                     StateBefore v_163 v_164 : acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (M.singleton v_87 (S.singleton v_88)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_166 vals ->
         S.foldl' (\ acc v_167 -> StateBefore v_166 v_167 : acc) [] vals ++
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