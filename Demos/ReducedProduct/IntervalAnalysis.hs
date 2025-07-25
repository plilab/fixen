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

data CondVal = CondVal Natural BBool
                 deriving (Eq, Show, Generic)

instance Hashable CondVal

instance PartialOrd CondVal where
        leq (CondVal v0 v1) (CondVal v0' v1')
          = (v0 `leq` v0') && (v1 `leq` v1')
mkCondVal v0 v1 = CondValFact (CondVal v0 v1)

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

data Fact = SeqFact Seq
          | CondValFact CondVal
          | CondFact Cond
          | AssignFact Assign
          | VarFact Var
          | StateAfterFact StateAfter
              deriving (Show, Eq)

data Continuation = Initial Fact
                  | CondStepFalseCont Natural State Natural State Expr Natural
                  | AssignInitCont Natural String Expr
                  | AssignStepCont Natural State String Expr
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondCont Natural Natural State Expr Natural Natural
                  | SeqStepCont Natural Natural State State
                  | CondStepTrueCont Natural State Natural State Expr Natural
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (CondStepFalseCont l0 st0 jlf0 stf0 e0 jlt0)
  = widenState (StateAfterFact . StateAfter jlf0) (join st0 stf0)
      (M.lookupDefault S.empty jlf0 (factsStateAfter db))
evaluate db (AssignInitCont l4 x1 e5)
  = widenState (StateAfterFact . StateAfter l4) (empty 0)
      (M.lookupDefault S.empty l4 (factsStateAfter db))
evaluate db (AssignStepCont l2 st3 x0 e3)
  = widenState (StateAfterFact . StateAfter l2)
      (insert x0 (eval e3 st3) st3)
      (M.lookupDefault S.empty l2 (factsStateAfter db))
evaluate db (CondInitCont l3 e4 jlt3 jlf3)
  = widenState (StateAfterFact . StateAfter l3) (empty 0)
      (M.lookupDefault S.empty l3 (factsStateAfter db))
evaluate db (EvalCondCont l00 lcond0 st2 e2 jlt2 jlf2)
  = joinBool (CondValFact . CondVal lcond0)
      (evaluateConditional e2 st2)
      (M.lookupDefault S.empty lcond0 (factsCondVal db))
evaluate db (SeqStepCont l10 l20 st10 st20)
  = widenState (StateAfterFact . StateAfter l20) (join st20 st10)
      (M.lookupDefault S.empty l20 (factsStateAfter db))
evaluate db (CondStepTrueCont l1 st1 jlt1 stt0 e1 jlf1)
  = widenState (StateAfterFact . StateAfter jlt1) (join st1 stt0)
      (M.lookupDefault S.empty jlt1 (factsStateAfter db))
evaluate db (VarInitCont l5 x2)
  = widenState (StateAfterFact . StateAfter l5) (singleton x2 Bot)
      (M.lookupDefault S.empty l5 (factsStateAfter db))

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsSeq :: S.HashSet (Natural, Natural),
                         factsVar :: S.HashSet (Natural, String),
                         factsCond ::
                         M.HashMap Natural (S.HashSet (Expr, Natural, Natural)),
                         factsCondVal :: M.HashMap Natural (S.HashSet BBool),
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr),
                         factsStateAfter :: M.HashMap Natural (S.HashSet State)}
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
          CondValFact (CondVal v0 v1) -> if M.member v0 (factsCondVal db)
                                           then
                                           first
                                             (\ hset ->
                                                db{factsCondVal =
                                                     M.insert v0 hset (factsCondVal db)})
                                             (update ((M.!) (factsCondVal db) v0) v1)
                                           else
                                           (db{factsCondVal =
                                                 M.insert v0 (S.singleton v1) (factsCondVal db)},
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
step db fact q_1
  = case fact of
        SeqFact f@(Seq v_2 v_3) -> Q.unions
                                     [q_1,
                                      foldl'
                                        (\ q_4 f@(Seq l10 l20) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_4,
                                                 foldl'
                                                   (\ q_6 f@(StateAfter l10_5 st10) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_6,
                                                            foldl'
                                                              (\ q_8 f@(StateAfter l20_7 st20) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_8,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (SeqStepCont l10_5 l20_7
                                                                               st10
                                                                               st20))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_9 vals ->
                                                                    concatMap
                                                                      (\ v_10 ->
                                                                         pure StateAfter <*>
                                                                           mlbs v_9 l20
                                                                           <*> pure v_10)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_11 vals ->
                                                         concatMap
                                                           (\ v_12 ->
                                                              pure StateAfter <*> mlbs v_11 l10 <*>
                                                                pure v_12)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateAfter db)),
                                                 foldl'
                                                   (\ q_14 f@(Cond l20_13 e2 jlt2 jlf2) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_14,
                                                            foldl'
                                                              (\ q_16 f@(StateAfter l10_15 st2) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_16,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (EvalCondCont l10_15
                                                                               l20_13
                                                                               st2
                                                                               e2
                                                                               jlt2
                                                                               jlf2))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_17 vals ->
                                                                    concatMap
                                                                      (\ v_18 ->
                                                                         pure StateAfter <*>
                                                                           mlbs v_17 l10
                                                                           <*> pure v_18)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_19 vals ->
                                                         concatMap
                                                           (\ (v_20, v_21, v_22) ->
                                                              pure Cond <*> mlbs v_19 l20 <*>
                                                                pure v_20
                                                                <*> pure v_21
                                                                <*> pure v_22)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsCond db))]))
                                        Q.empty
                                        (S.foldl' (\ rest (v_23, v_24) -> Seq v_23 v_24 : rest) []
                                           (S.singleton (v_2, v_3)))]
        CondValFact f@(CondVal v_25 v_26) -> Q.unions
                                               [q_1,
                                                foldl'
                                                  (\ q_27 f@(CondVal l1 _) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_27,
                                                           foldl'
                                                             (\ q_28 f@(StateAfter jlt1 stt0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_28,
                                                                      foldl'
                                                                        (\ q_30
                                                                           f@(StateAfter l1_29 st1)
                                                                           ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_30,
                                                                                 foldl'
                                                                                   (\ q_33
                                                                                      f@(Cond
                                                                                           l1_29_31
                                                                                           e1
                                                                                           jlt1_32
                                                                                           jlf1)
                                                                                      ->
                                                                                      trace
                                                                                        ("got: " ++
                                                                                           show f)
                                                                                        (Q.unions
                                                                                           [q_33,
                                                                                            Q.singleton
                                                                                              (traceConclusion
                                                                                                 (CondStepTrueCont
                                                                                                    l1_29_31
                                                                                                    st1
                                                                                                    jlt1_32
                                                                                                    stt0
                                                                                                    e1
                                                                                                    jlf1))]))
                                                                                   Q.empty
                                                                                   (M.foldlWithKey'
                                                                                      (\ rest v_34
                                                                                         vals ->
                                                                                         concatMap
                                                                                           (\ (v_35,
                                                                                               v_36,
                                                                                               v_37)
                                                                                              ->
                                                                                              pure
                                                                                                Cond
                                                                                                <*>
                                                                                                mlbs
                                                                                                  v_34
                                                                                                  l1_29
                                                                                                <*>
                                                                                                pure
                                                                                                  v_35
                                                                                                <*>
                                                                                                mlbs
                                                                                                  v_36
                                                                                                  jlt1
                                                                                                <*>
                                                                                                pure
                                                                                                  v_37)
                                                                                           vals
                                                                                           ++ rest)
                                                                                      []
                                                                                      (factsCond
                                                                                         db))]))
                                                                        Q.empty
                                                                        (M.foldlWithKey'
                                                                           (\ rest v_38 vals ->
                                                                              concatMap
                                                                                (\ v_39 ->
                                                                                   pure StateAfter
                                                                                     <*>
                                                                                     mlbs v_38 l1
                                                                                     <*> pure v_39)
                                                                                vals
                                                                                ++ rest)
                                                                           []
                                                                           (factsStateAfter db))]))
                                                             Q.empty
                                                             (M.foldlWithKey'
                                                                (\ rest v_40 vals ->
                                                                   S.foldl'
                                                                     (\ acc v_41 ->
                                                                        StateAfter v_40 v_41 : acc)
                                                                     []
                                                                     vals
                                                                     ++ rest)
                                                                []
                                                                (factsStateAfter db))]))
                                                  Q.empty
                                                  (M.foldlWithKey'
                                                     (\ rest v_42 vals ->
                                                        concatMap
                                                          (\ v_43 ->
                                                             pure CondVal <*> pure v_42 <*>
                                                               mlbs v_43 BTrue)
                                                          vals
                                                          ++ rest)
                                                     []
                                                     (M.singleton v_25 (S.singleton v_26))),
                                                foldl'
                                                  (\ q_44 f@(CondVal l0 _) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_44,
                                                           foldl'
                                                             (\ q_46 f@(StateAfter l0_45 st0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_46,
                                                                      foldl'
                                                                        (\ q_47
                                                                           f@(StateAfter jlf0 stf0)
                                                                           ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_47,
                                                                                 foldl'
                                                                                   (\ q_50
                                                                                      f@(Cond
                                                                                           l0_45_48
                                                                                           e0 jlt0
                                                                                           jlf0_49)
                                                                                      ->
                                                                                      trace
                                                                                        ("got: " ++
                                                                                           show f)
                                                                                        (Q.unions
                                                                                           [q_50,
                                                                                            Q.singleton
                                                                                              (traceConclusion
                                                                                                 (CondStepFalseCont
                                                                                                    l0_45_48
                                                                                                    st0
                                                                                                    jlf0_49
                                                                                                    stf0
                                                                                                    e0
                                                                                                    jlt0))]))
                                                                                   Q.empty
                                                                                   (M.foldlWithKey'
                                                                                      (\ rest v_51
                                                                                         vals ->
                                                                                         concatMap
                                                                                           (\ (v_52,
                                                                                               v_53,
                                                                                               v_54)
                                                                                              ->
                                                                                              pure
                                                                                                Cond
                                                                                                <*>
                                                                                                mlbs
                                                                                                  v_51
                                                                                                  l0_45
                                                                                                <*>
                                                                                                pure
                                                                                                  v_52
                                                                                                <*>
                                                                                                pure
                                                                                                  v_53
                                                                                                <*>
                                                                                                mlbs
                                                                                                  v_54
                                                                                                  jlf0)
                                                                                           vals
                                                                                           ++ rest)
                                                                                      []
                                                                                      (factsCond
                                                                                         db))]))
                                                                        Q.empty
                                                                        (M.foldlWithKey'
                                                                           (\ rest v_55 vals ->
                                                                              S.foldl'
                                                                                (\ acc v_56 ->
                                                                                   StateAfter v_55
                                                                                     v_56
                                                                                     : acc)
                                                                                []
                                                                                vals
                                                                                ++ rest)
                                                                           []
                                                                           (factsStateAfter db))]))
                                                             Q.empty
                                                             (M.foldlWithKey'
                                                                (\ rest v_57 vals ->
                                                                   concatMap
                                                                     (\ v_58 ->
                                                                        pure StateAfter <*>
                                                                          mlbs v_57 l0
                                                                          <*> pure v_58)
                                                                     vals
                                                                     ++ rest)
                                                                []
                                                                (factsStateAfter db))]))
                                                  Q.empty
                                                  (M.foldlWithKey'
                                                     (\ rest v_59 vals ->
                                                        concatMap
                                                          (\ v_60 ->
                                                             pure CondVal <*> pure v_59 <*>
                                                               mlbs v_60 BFalse)
                                                          vals
                                                          ++ rest)
                                                     []
                                                     (M.singleton v_25 (S.singleton v_26)))]
        CondFact f@(Cond v_61 v_62 v_63 v_64) -> Q.unions
                                                   [q_1,
                                                    foldl'
                                                      (\ q_65 f@(Cond l0 e0 jlt0 jlf0) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_65,
                                                               foldl'
                                                                 (\ q_67 f@(StateAfter l0_66 st0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_67,
                                                                          foldl'
                                                                            (\ q_69
                                                                               f@(StateAfter jlf0_68
                                                                                    stf0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_69,
                                                                                     foldl'
                                                                                       (\ q_71
                                                                                          f@(CondVal
                                                                                               l0_66_70
                                                                                               _)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_71,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (CondStepFalseCont
                                                                                                        l0_66_70
                                                                                                        st0
                                                                                                        jlf0_68
                                                                                                        stf0
                                                                                                        e0
                                                                                                        jlt0))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             v_72
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ v_73
                                                                                                  ->
                                                                                                  pure
                                                                                                    CondVal
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_72
                                                                                                      l0_66
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_73
                                                                                                      BFalse)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsCondVal
                                                                                             db))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_74 vals ->
                                                                                  concatMap
                                                                                    (\ v_75 ->
                                                                                       pure
                                                                                         StateAfter
                                                                                         <*>
                                                                                         mlbs v_74
                                                                                           jlf0
                                                                                         <*>
                                                                                         pure v_75)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateAfter
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_76 vals ->
                                                                       concatMap
                                                                         (\ v_77 ->
                                                                            pure StateAfter <*>
                                                                              mlbs v_76 l0
                                                                              <*> pure v_77)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateAfter db)),
                                                               foldl'
                                                                 (\ q_79 f@(CondVal l0_78 _) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_79,
                                                                          foldl'
                                                                            (\ q_81
                                                                               f@(StateAfter
                                                                                    l0_78_80 st1)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_81,
                                                                                     foldl'
                                                                                       (\ q_83
                                                                                          f@(StateAfter
                                                                                               jlt0_82
                                                                                               stt0)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_83,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (CondStepTrueCont
                                                                                                        l0_78_80
                                                                                                        st1
                                                                                                        jlt0_82
                                                                                                        stt0
                                                                                                        e0
                                                                                                        jlf0))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             v_84
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ v_85
                                                                                                  ->
                                                                                                  pure
                                                                                                    StateAfter
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_84
                                                                                                      jlt0
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_85)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsStateAfter
                                                                                             db))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_86 vals ->
                                                                                  concatMap
                                                                                    (\ v_87 ->
                                                                                       pure
                                                                                         StateAfter
                                                                                         <*>
                                                                                         mlbs v_86
                                                                                           l0_78
                                                                                         <*>
                                                                                         pure v_87)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateAfter
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_88 vals ->
                                                                       concatMap
                                                                         (\ v_89 ->
                                                                            pure CondVal <*>
                                                                              mlbs v_88 l0
                                                                              <*> mlbs v_89 BTrue)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsCondVal db)),
                                                               foldl'
                                                                 (\ q_91 f@(Seq l00 l0_90) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_91,
                                                                          foldl'
                                                                            (\ q_93
                                                                               f@(StateAfter l00_92
                                                                                    st2)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_93,
                                                                                     Q.singleton
                                                                                       (traceConclusion
                                                                                          (EvalCondCont
                                                                                             l00_92
                                                                                             l0_90
                                                                                             st2
                                                                                             e0
                                                                                             jlt0
                                                                                             jlf0))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_94 vals ->
                                                                                  concatMap
                                                                                    (\ v_95 ->
                                                                                       pure
                                                                                         StateAfter
                                                                                         <*>
                                                                                         mlbs v_94
                                                                                           l00
                                                                                         <*>
                                                                                         pure v_95)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateAfter
                                                                                  db))]))
                                                                 Q.empty
                                                                 (foldl'
                                                                    (\ rest (v_96, v_97) ->
                                                                       (pure Seq <*> pure v_96 <*>
                                                                          mlbs v_97 l0)
                                                                         ++ rest)
                                                                    []
                                                                    (factsSeq db)),
                                                               Q.singleton
                                                                 (traceConclusion
                                                                    (CondInitCont l0 e0 jlt0
                                                                       jlf0))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_98 vals ->
                                                            S.foldl'
                                                              (\ acc (v_99, v_100, v_101) ->
                                                                 Cond v_98 v_99 v_100 v_101 : acc)
                                                              []
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_61
                                                            (S.singleton (v_62, v_63, v_64))))]
        AssignFact f@(Assign v_102 v_103 v_104) -> Q.unions
                                                     [q_1,
                                                      foldl'
                                                        (\ q_105 f@(Assign l2 x0 e3) ->
                                                           trace ("got: " ++ show f)
                                                             (Q.unions
                                                                [q_105,
                                                                 foldl'
                                                                   (\ q_107
                                                                      f@(StateAfter l2_106 st3) ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_107,
                                                                            Q.singleton
                                                                              (traceConclusion
                                                                                 (AssignStepCont
                                                                                    l2_106
                                                                                    st3
                                                                                    x0
                                                                                    e3))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_108 vals ->
                                                                         concatMap
                                                                           (\ v_109 ->
                                                                              pure StateAfter <*>
                                                                                mlbs v_108 l2
                                                                                <*> pure v_109)
                                                                           vals
                                                                           ++ rest)
                                                                      []
                                                                      (factsStateAfter db)),
                                                                 Q.singleton
                                                                   (traceConclusion
                                                                      (AssignInitCont l2 x0 e3))]))
                                                        Q.empty
                                                        (M.foldlWithKey'
                                                           (\ rest (v_110, v_111) vals ->
                                                              S.foldl'
                                                                (\ acc v_112 ->
                                                                   Assign v_110 v_111 v_112 : acc)
                                                                []
                                                                vals
                                                                ++ rest)
                                                           []
                                                           (M.singleton (v_102, v_103)
                                                              (S.singleton v_104)))]
        VarFact f@(Var v_113 v_114) -> Q.unions
                                         [q_1,
                                          foldl'
                                            (\ q_115 f@(Var l5 x2) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_115,
                                                     Q.singleton
                                                       (traceConclusion (VarInitCont l5 x2))]))
                                            Q.empty
                                            (S.foldl'
                                               (\ rest (v_116, v_117) -> Var v_116 v_117 : rest)
                                               []
                                               (S.singleton (v_113, v_114)))]
        StateAfterFact f@(StateAfter v_118 v_119) -> Q.unions
                                                       [q_1,
                                                        foldl'
                                                          (\ q_120 f@(StateAfter l10 st10) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_120,
                                                                   foldl'
                                                                     (\ q_122 f@(Seq l10_121 l20) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_122,
                                                                              foldl'
                                                                                (\ q_124
                                                                                   f@(StateAfter
                                                                                        l20_123
                                                                                        st20)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_124,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (SeqStepCont
                                                                                                 l10_121
                                                                                                 l20_123
                                                                                                 st10
                                                                                                 st20))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_125
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_126 ->
                                                                                           pure
                                                                                             StateAfter
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_125
                                                                                               l20
                                                                                             <*>
                                                                                             pure
                                                                                               v_126)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateAfter
                                                                                      db))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest (v_127, v_128) ->
                                                                           (pure Seq <*>
                                                                              mlbs v_127 l10
                                                                              <*> pure v_128)
                                                                             ++ rest)
                                                                        []
                                                                        (factsSeq db)),
                                                                   foldl'
                                                                     (\ q_130 f@(Seq l20 l10_129) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_130,
                                                                              foldl'
                                                                                (\ q_132
                                                                                   f@(StateAfter
                                                                                        l20_131
                                                                                        st20)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_132,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (SeqStepCont
                                                                                                 l20_131
                                                                                                 l10_129
                                                                                                 st20
                                                                                                 st10))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_133
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_134 ->
                                                                                           pure
                                                                                             StateAfter
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_133
                                                                                               l20
                                                                                             <*>
                                                                                             pure
                                                                                               v_134)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateAfter
                                                                                      db))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest (v_135, v_136) ->
                                                                           (pure Seq <*> pure v_135
                                                                              <*> mlbs v_136 l10)
                                                                             ++ rest)
                                                                        []
                                                                        (factsSeq db)),
                                                                   foldl'
                                                                     (\ q_137
                                                                        f@(StateAfter jlf0 stf0) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_137,
                                                                              foldl'
                                                                                (\ q_140
                                                                                   f@(Cond l10_138
                                                                                        e0 jlt0
                                                                                        jlf0_139)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_140,
                                                                                         foldl'
                                                                                           (\ q_142
                                                                                              f@(CondVal
                                                                                                   l10_138_141
                                                                                                   _)
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
                                                                                                         (CondStepFalseCont
                                                                                                            l10_138_141
                                                                                                            st10
                                                                                                            jlf0_139
                                                                                                            stf0
                                                                                                            e0
                                                                                                            jlt0))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 v_143
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ v_144
                                                                                                      ->
                                                                                                      pure
                                                                                                        CondVal
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_143
                                                                                                          l10_138
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_144
                                                                                                          BFalse)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsCondVal
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_145
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_146,
                                                                                            v_147,
                                                                                            v_148)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_145
                                                                                               l10
                                                                                             <*>
                                                                                             pure
                                                                                               v_146
                                                                                             <*>
                                                                                             pure
                                                                                               v_147
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_148
                                                                                               jlf0)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_149 vals ->
                                                                           S.foldl'
                                                                             (\ acc v_150 ->
                                                                                StateAfter v_149
                                                                                  v_150
                                                                                  : acc)
                                                                             []
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateAfter db)),
                                                                   foldl'
                                                                     (\ q_152
                                                                        f@(Cond l0 e0 jlt0 l10_151)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_152,
                                                                              foldl'
                                                                                (\ q_154
                                                                                   f@(StateAfter
                                                                                        l0_153 st0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_154,
                                                                                         foldl'
                                                                                           (\ q_156
                                                                                              f@(CondVal
                                                                                                   l0_153_155
                                                                                                   _)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_156,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (CondStepFalseCont
                                                                                                            l0_153_155
                                                                                                            st0
                                                                                                            l10_151
                                                                                                            st10
                                                                                                            e0
                                                                                                            jlt0))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 v_157
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ v_158
                                                                                                      ->
                                                                                                      pure
                                                                                                        CondVal
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_157
                                                                                                          l0_153
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_158
                                                                                                          BFalse)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsCondVal
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_159
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_160 ->
                                                                                           pure
                                                                                             StateAfter
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_159
                                                                                               l0
                                                                                             <*>
                                                                                             pure
                                                                                               v_160)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateAfter
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_161 vals ->
                                                                           concatMap
                                                                             (\ (v_162, v_163,
                                                                                 v_164)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  pure v_161
                                                                                  <*> pure v_162
                                                                                  <*> pure v_163
                                                                                  <*>
                                                                                  mlbs v_164 l10)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db)),
                                                                   foldl'
                                                                     (\ q_165 f@(CondVal l1 _) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_165,
                                                                              foldl'
                                                                                (\ q_168
                                                                                   f@(Cond l1_166 e1
                                                                                        l10_167
                                                                                        jlf1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_168,
                                                                                         foldl'
                                                                                           (\ q_170
                                                                                              f@(StateAfter
                                                                                                   l1_166_169
                                                                                                   st1)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_170,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (CondStepTrueCont
                                                                                                            l1_166_169
                                                                                                            st1
                                                                                                            l10_167
                                                                                                            st10
                                                                                                            e1
                                                                                                            jlf1))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 v_171
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ v_172
                                                                                                      ->
                                                                                                      pure
                                                                                                        StateAfter
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_171
                                                                                                          l1_166
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_172)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsStateAfter
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_173
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_174,
                                                                                            v_175,
                                                                                            v_176)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_173
                                                                                               l1
                                                                                             <*>
                                                                                             pure
                                                                                               v_174
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_175
                                                                                               l10
                                                                                             <*>
                                                                                             pure
                                                                                               v_176)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_177 vals ->
                                                                           concatMap
                                                                             (\ v_178 ->
                                                                                pure CondVal <*>
                                                                                  pure v_177
                                                                                  <*>
                                                                                  mlbs v_178 BTrue)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCondVal db)),
                                                                   foldl'
                                                                     (\ q_180 f@(CondVal l10_179 _)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_180,
                                                                              foldl'
                                                                                (\ q_181
                                                                                   f@(StateAfter
                                                                                        jlt1 stt0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_181,
                                                                                         foldl'
                                                                                           (\ q_184
                                                                                              f@(Cond
                                                                                                   l10_179_182
                                                                                                   e1
                                                                                                   jlt1_183
                                                                                                   jlf1)
                                                                                              ->
                                                                                              trace
                                                                                                ("got: "
                                                                                                   ++
                                                                                                   show
                                                                                                     f)
                                                                                                (Q.unions
                                                                                                   [q_184,
                                                                                                    Q.singleton
                                                                                                      (traceConclusion
                                                                                                         (CondStepTrueCont
                                                                                                            l10_179_182
                                                                                                            st10
                                                                                                            jlt1_183
                                                                                                            stt0
                                                                                                            e1
                                                                                                            jlf1))]))
                                                                                           Q.empty
                                                                                           (M.foldlWithKey'
                                                                                              (\ rest
                                                                                                 v_185
                                                                                                 vals
                                                                                                 ->
                                                                                                 concatMap
                                                                                                   (\ (v_186,
                                                                                                       v_187,
                                                                                                       v_188)
                                                                                                      ->
                                                                                                      pure
                                                                                                        Cond
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_185
                                                                                                          l10_179
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_186
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_187
                                                                                                          jlt1
                                                                                                        <*>
                                                                                                        pure
                                                                                                          v_188)
                                                                                                   vals
                                                                                                   ++
                                                                                                   rest)
                                                                                              []
                                                                                              (factsCond
                                                                                                 db))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_189
                                                                                      vals ->
                                                                                      S.foldl'
                                                                                        (\ acc v_190
                                                                                           ->
                                                                                           StateAfter
                                                                                             v_189
                                                                                             v_190
                                                                                             : acc)
                                                                                        []
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateAfter
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_191 vals ->
                                                                           concatMap
                                                                             (\ v_192 ->
                                                                                pure CondVal <*>
                                                                                  mlbs v_191 l10
                                                                                  <*>
                                                                                  mlbs v_192 BTrue)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCondVal db)),
                                                                   foldl'
                                                                     (\ q_194 f@(Seq l10_193 lcond0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_194,
                                                                              foldl'
                                                                                (\ q_196
                                                                                   f@(Cond
                                                                                        lcond0_195
                                                                                        e2 jlt2
                                                                                        jlf2)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_196,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondCont
                                                                                                 l10_193
                                                                                                 lcond0_195
                                                                                                 st10
                                                                                                 e2
                                                                                                 jlt2
                                                                                                 jlf2))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_197
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ (v_198,
                                                                                            v_199,
                                                                                            v_200)
                                                                                           ->
                                                                                           pure Cond
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_197
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_198
                                                                                             <*>
                                                                                             pure
                                                                                               v_199
                                                                                             <*>
                                                                                             pure
                                                                                               v_200)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsCond
                                                                                      db))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest (v_201, v_202) ->
                                                                           (pure Seq <*>
                                                                              mlbs v_201 l10
                                                                              <*> pure v_202)
                                                                             ++ rest)
                                                                        []
                                                                        (factsSeq db)),
                                                                   foldl'
                                                                     (\ q_204
                                                                        f@(Assign l10_203 x0 e3) ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_204,
                                                                              Q.singleton
                                                                                (traceConclusion
                                                                                   (AssignStepCont
                                                                                      l10_203
                                                                                      st10
                                                                                      x0
                                                                                      e3))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest (v_205, v_206) vals
                                                                           ->
                                                                           concatMap
                                                                             (\ v_207 ->
                                                                                pure Assign <*>
                                                                                  mlbs v_205 l10
                                                                                  <*> pure v_206
                                                                                  <*> pure v_207)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsAssign db))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_208 vals ->
                                                                S.foldl'
                                                                  (\ acc v_209 ->
                                                                     StateAfter v_208 v_209 : acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (M.singleton v_118
                                                                (S.singleton v_119)))]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_210 db
  = M.foldlWithKey'
      (\ rest v_212 vals ->
         concatMap
           (\ v_213 -> pure StateAfter <*> mlbs v_212 v_210 <*> pure v_213)
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