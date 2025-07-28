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
step db fact q_10178
  = case fact of
        SeqFact f@(Seq v_10179 v_10180) -> Q.unions
                                             [q_10178,
                                              foldl'
                                                (\ q_10181 f@(Seq l10 l20) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_10181,
                                                         foldl'
                                                           (\ q_10183 f@(StateBefore l10_10182 st10)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_10183,
                                                                    foldl'
                                                                      (\ q_10185
                                                                         f@(StateBefore l20_10184
                                                                              st20)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_10185,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepCont
                                                                                       l10_10182
                                                                                       l20_10184
                                                                                       st10
                                                                                       st20))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_10186 vals ->
                                                                            concatMap
                                                                              (\ v_10187 ->
                                                                                 pure StateBefore
                                                                                   <*>
                                                                                   mlbs v_10186 l20
                                                                                   <*> pure v_10187)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBefore db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_10188 vals ->
                                                                 concatMap
                                                                   (\ v_10189 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_10188 l10
                                                                        <*> pure v_10189)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         foldl'
                                                           (\ q_10191
                                                              f@(StateBefore l10_10190 stBefore0) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_10191,
                                                                    foldl'
                                                                      (\ q_10193
                                                                         f@(StateBefore l20_10192
                                                                              stA0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_10193,
                                                                               foldl'
                                                                                 (\ q_10195
                                                                                    f@(Assign
                                                                                         l10_10190_10194
                                                                                         x0 e2)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_10195,
                                                                                          Q.singleton
                                                                                            (traceConclusion
                                                                                               (AssignStepCont
                                                                                                  l10_10190_10194
                                                                                                  l20_10192
                                                                                                  x0
                                                                                                  e2
                                                                                                  stA0
                                                                                                  stBefore0))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest
                                                                                       (v_10196,
                                                                                        v_10197)
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_10198
                                                                                            ->
                                                                                            pure
                                                                                              Assign
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_10196
                                                                                                l10_10190
                                                                                              <*>
                                                                                              pure
                                                                                                v_10197
                                                                                              <*>
                                                                                              pure
                                                                                                v_10198)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsAssign
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_10199 vals ->
                                                                            concatMap
                                                                              (\ v_10200 ->
                                                                                 pure StateBefore
                                                                                   <*>
                                                                                   mlbs v_10199 l20
                                                                                   <*> pure v_10200)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateBefore db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_10201 vals ->
                                                                 concatMap
                                                                   (\ v_10202 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_10201 l10
                                                                        <*> pure v_10202)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_10203, v_10204) ->
                                                      Seq v_10203 v_10204 : rest)
                                                   []
                                                   (S.singleton (v_10179, v_10180)))]
        PhiFact f@(Phi v_10205) -> Q.unions
                                     [q_10178,
                                      foldl'
                                        (\ q_10206 f@(Phi l0) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_10206,
                                                 Q.singleton (traceConclusion (PhiInitCont l0))]))
                                        Q.empty
                                        (S.foldl' (\ rest v_10207 -> Phi v_10207 : rest) []
                                           (S.singleton v_10205))]
        CondFact f@(Cond v_10208 v_10209 v_10210 v_10211) -> Q.unions
                                                               [q_10178,
                                                                foldl'
                                                                  (\ q_10212
                                                                     f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_10212,
                                                                           foldl'
                                                                             (\ q_10214
                                                                                f@(StateBefore
                                                                                     jlf0_10213
                                                                                     stf0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_10214,
                                                                                      foldl'
                                                                                        (\ q_10216
                                                                                           f@(StateBefore
                                                                                                lcond0_10215
                                                                                                stc0)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_10216,
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
                                                                                                        [q_10216,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondFalseCont
                                                                                                                 lcond0_10215
                                                                                                                 stc0
                                                                                                                 e0
                                                                                                                 jlt0
                                                                                                                 jlf0_10213
                                                                                                                 stf0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_10217
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_10218
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_10217
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_10218)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_10219 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_10220 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_10219
                                                                                            jlf0
                                                                                          <*>
                                                                                          pure
                                                                                            v_10220)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           foldl'
                                                                             (\ q_10222
                                                                                f@(StateBefore
                                                                                     jlt0_10221
                                                                                     stt0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_10222,
                                                                                      foldl'
                                                                                        (\ q_10224
                                                                                           f@(StateBefore
                                                                                                lcond0_10223
                                                                                                stc1)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_10224,
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
                                                                                                        [q_10224,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondTrueCont
                                                                                                                 lcond0_10223
                                                                                                                 stc1
                                                                                                                 e0
                                                                                                                 jlt0_10221
                                                                                                                 jlf0
                                                                                                                 stt0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_10225
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_10226
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_10225
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_10226)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_10227 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_10228 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_10227
                                                                                            jlt0
                                                                                          <*>
                                                                                          pure
                                                                                            v_10228)
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
                                                                     (\ rest v_10229 vals ->
                                                                        S.foldl'
                                                                          (\ acc
                                                                             (v_10230, v_10231,
                                                                              v_10232)
                                                                             ->
                                                                             Cond v_10229 v_10230
                                                                               v_10231
                                                                               v_10232
                                                                               : acc)
                                                                          []
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (M.singleton v_10208
                                                                        (S.singleton
                                                                           (v_10209, v_10210,
                                                                            v_10211))))]
        AssignFact f@(Assign v_10233 v_10234 v_10235) -> Q.unions
                                                           [q_10178,
                                                            foldl'
                                                              (\ q_10236 f@(Assign lAssign0 x0 e2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_10236,
                                                                       foldl'
                                                                         (\ q_10238
                                                                            f@(StateBefore
                                                                                 lAssign0_10237
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_10238,
                                                                                  foldl'
                                                                                    (\ q_10239
                                                                                       f@(StateBefore
                                                                                            lAfter0
                                                                                            stA0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_10239,
                                                                                             foldl'
                                                                                               (\ q_10242
                                                                                                  f@(Seq
                                                                                                       lAssign0_10237_10240
                                                                                                       lAfter0_10241)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_10242,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_10237_10240
                                                                                                                lAfter0_10241
                                                                                                                x0
                                                                                                                e2
                                                                                                                stA0
                                                                                                                stBefore0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_10243,
                                                                                                      v_10244)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_10243
                                                                                                          lAssign0_10237
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_10244
                                                                                                          lAfter0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_10245
                                                                                          vals ->
                                                                                          S.foldl'
                                                                                            (\ acc
                                                                                               v_10246
                                                                                               ->
                                                                                               StateBefore
                                                                                                 v_10245
                                                                                                 v_10246
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
                                                                            (\ rest v_10247 vals ->
                                                                               concatMap
                                                                                 (\ v_10248 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_10247
                                                                                        lAssign0
                                                                                      <*>
                                                                                      pure v_10248)
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
                                                                 (\ rest (v_10249, v_10250) vals ->
                                                                    S.foldl'
                                                                      (\ acc v_10251 ->
                                                                         Assign v_10249 v_10250
                                                                           v_10251
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton (v_10233, v_10234)
                                                                    (S.singleton v_10235)))]
        VarFact f@(Var v_10252 v_10253) -> Q.unions
                                             [q_10178,
                                              foldl'
                                                (\ q_10254 f@(Var l3 x2) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_10254,
                                                         Q.singleton
                                                           (traceConclusion (VarInitCont l3 x2))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_10255, v_10256) ->
                                                      Var v_10255 v_10256 : rest)
                                                   []
                                                   (S.singleton (v_10252, v_10253)))]
        StateBeforeFact f@(StateBefore v_10257 v_10258) -> Q.unions
                                                             [q_10178,
                                                              foldl'
                                                                (\ q_10259 f@(StateBefore l10 st10)
                                                                   ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_10259,
                                                                         foldl'
                                                                           (\ q_10261
                                                                              f@(Seq l10_10260 l20)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10261,
                                                                                    foldl'
                                                                                      (\ q_10263
                                                                                         f@(StateBefore
                                                                                              l20_10262
                                                                                              st20)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10263,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepCont
                                                                                                       l10_10260
                                                                                                       l20_10262
                                                                                                       st10
                                                                                                       st20))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10264
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10265
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10264
                                                                                                     l20
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10265)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_10266, v_10267)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    mlbs v_10266 l10
                                                                                    <*>
                                                                                    pure v_10267)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_10268
                                                                              f@(StateBefore l20
                                                                                   st20)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10268,
                                                                                    foldl'
                                                                                      (\ q_10271
                                                                                         f@(Seq
                                                                                              l20_10269
                                                                                              l10_10270)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10271,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepCont
                                                                                                       l20_10269
                                                                                                       l10_10270
                                                                                                       st20
                                                                                                       st10))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_10272,
                                                                                             v_10273)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10272
                                                                                                 l20
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10273
                                                                                                 l10)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10274 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10275 ->
                                                                                      StateBefore
                                                                                        v_10274
                                                                                        v_10275
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10277
                                                                              f@(Cond lcond0 e0 jlt0
                                                                                   l10_10276)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10277,
                                                                                    foldl'
                                                                                      (\ q_10279
                                                                                         f@(StateBefore
                                                                                              lcond0_10278
                                                                                              stc0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10279,
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
                                                                                                      [q_10279,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               lcond0_10278
                                                                                                               stc0
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               l10_10276
                                                                                                               st10))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10280
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10281
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10280
                                                                                                     lcond0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10281)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10282 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_10283,
                                                                                       v_10284,
                                                                                       v_10285)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_10282
                                                                                        <*>
                                                                                        pure v_10283
                                                                                        <*>
                                                                                        pure v_10284
                                                                                        <*>
                                                                                        mlbs v_10285
                                                                                          l10)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_10286
                                                                              f@(StateBefore jlf0
                                                                                   stf0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10286,
                                                                                    foldl'
                                                                                      (\ q_10289
                                                                                         f@(Cond
                                                                                              l10_10287
                                                                                              e0
                                                                                              jlt0
                                                                                              jlf0_10288)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10289,
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
                                                                                                      [q_10289,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               l10_10287
                                                                                                               st10
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               jlf0_10288
                                                                                                               stf0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10290
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_10291,
                                                                                                  v_10292,
                                                                                                  v_10293)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10290
                                                                                                     l10
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10291
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10292
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10293
                                                                                                     jlf0)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10294 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10295 ->
                                                                                      StateBefore
                                                                                        v_10294
                                                                                        v_10295
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10297
                                                                              f@(Cond lcond1 e1
                                                                                   l10_10296 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10297,
                                                                                    foldl'
                                                                                      (\ q_10299
                                                                                         f@(StateBefore
                                                                                              lcond1_10298
                                                                                              stc1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10299,
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
                                                                                                      [q_10299,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               lcond1_10298
                                                                                                               stc1
                                                                                                               e1
                                                                                                               l10_10296
                                                                                                               jlf1
                                                                                                               st10))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10300
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10301
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10300
                                                                                                     lcond1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10301)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10302 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_10303,
                                                                                       v_10304,
                                                                                       v_10305)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_10302
                                                                                        <*>
                                                                                        pure v_10303
                                                                                        <*>
                                                                                        mlbs v_10304
                                                                                          l10
                                                                                        <*>
                                                                                        pure
                                                                                          v_10305)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_10306
                                                                              f@(StateBefore jlt1
                                                                                   stt0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10306,
                                                                                    foldl'
                                                                                      (\ q_10309
                                                                                         f@(Cond
                                                                                              l10_10307
                                                                                              e1
                                                                                              jlt1_10308
                                                                                              jlf1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10309,
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
                                                                                                      [q_10309,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               l10_10307
                                                                                                               st10
                                                                                                               e1
                                                                                                               jlt1_10308
                                                                                                               jlf1
                                                                                                               stt0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10310
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_10311,
                                                                                                  v_10312,
                                                                                                  v_10313)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10310
                                                                                                     l10
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10311
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10312
                                                                                                     jlt1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10313)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10314 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10315 ->
                                                                                      StateBefore
                                                                                        v_10314
                                                                                        v_10315
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_10317
                                                                              f@(Seq l10_10316
                                                                                   lAfter0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10317,
                                                                                    foldl'
                                                                                      (\ q_10319
                                                                                         f@(StateBefore
                                                                                              lAfter0_10318
                                                                                              stA0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10319,
                                                                                               foldl'
                                                                                                 (\ q_10321
                                                                                                    f@(Assign
                                                                                                         l10_10316_10320
                                                                                                         x0
                                                                                                         e2)
                                                                                                    ->
                                                                                                    trace
                                                                                                      ("got: "
                                                                                                         ++
                                                                                                         show
                                                                                                           f)
                                                                                                      (Q.unions
                                                                                                         [q_10321,
                                                                                                          Q.singleton
                                                                                                            (traceConclusion
                                                                                                               (AssignStepCont
                                                                                                                  l10_10316_10320
                                                                                                                  lAfter0_10318
                                                                                                                  x0
                                                                                                                  e2
                                                                                                                  stA0
                                                                                                                  st10))]))
                                                                                                 Q.empty
                                                                                                 (M.foldlWithKey'
                                                                                                    (\ rest
                                                                                                       (v_10322,
                                                                                                        v_10323)
                                                                                                       vals
                                                                                                       ->
                                                                                                       concatMap
                                                                                                         (\ v_10324
                                                                                                            ->
                                                                                                            pure
                                                                                                              Assign
                                                                                                              <*>
                                                                                                              mlbs
                                                                                                                v_10322
                                                                                                                l10_10316
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10323
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10324)
                                                                                                         vals
                                                                                                         ++
                                                                                                         rest)
                                                                                                    []
                                                                                                    (factsAssign
                                                                                                       db))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_10325
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_10326
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_10325
                                                                                                     lAfter0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_10326)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_10327, v_10328)
                                                                                 ->
                                                                                 (pure Seq <*>
                                                                                    mlbs v_10327 l10
                                                                                    <*>
                                                                                    pure v_10328)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsSeq db)),
                                                                         foldl'
                                                                           (\ q_10329
                                                                              f@(StateBefore
                                                                                   lAssign0
                                                                                   stBefore0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_10329,
                                                                                    foldl'
                                                                                      (\ q_10332
                                                                                         f@(Seq
                                                                                              lAssign0_10330
                                                                                              l10_10331)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_10332,
                                                                                               foldl'
                                                                                                 (\ q_10334
                                                                                                    f@(Assign
                                                                                                         lAssign0_10330_10333
                                                                                                         x0
                                                                                                         e2)
                                                                                                    ->
                                                                                                    trace
                                                                                                      ("got: "
                                                                                                         ++
                                                                                                         show
                                                                                                           f)
                                                                                                      (Q.unions
                                                                                                         [q_10334,
                                                                                                          Q.singleton
                                                                                                            (traceConclusion
                                                                                                               (AssignStepCont
                                                                                                                  lAssign0_10330_10333
                                                                                                                  l10_10331
                                                                                                                  x0
                                                                                                                  e2
                                                                                                                  st10
                                                                                                                  stBefore0))]))
                                                                                                 Q.empty
                                                                                                 (M.foldlWithKey'
                                                                                                    (\ rest
                                                                                                       (v_10335,
                                                                                                        v_10336)
                                                                                                       vals
                                                                                                       ->
                                                                                                       concatMap
                                                                                                         (\ v_10337
                                                                                                            ->
                                                                                                            pure
                                                                                                              Assign
                                                                                                              <*>
                                                                                                              mlbs
                                                                                                                v_10335
                                                                                                                lAssign0_10330
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10336
                                                                                                              <*>
                                                                                                              pure
                                                                                                                v_10337)
                                                                                                         vals
                                                                                                         ++
                                                                                                         rest)
                                                                                                    []
                                                                                                    (factsAssign
                                                                                                       db))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_10338,
                                                                                             v_10339)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10338
                                                                                                 lAssign0
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_10339
                                                                                                 l10)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_10340 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_10341 ->
                                                                                      StateBefore
                                                                                        v_10340
                                                                                        v_10341
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_10342 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_10343 ->
                                                                           StateBefore v_10342
                                                                             v_10343
                                                                             : acc)
                                                                        []
                                                                        vals
                                                                        ++ rest)
                                                                   []
                                                                   (M.singleton v_10257
                                                                      (S.singleton v_10258)))]

stateBefore :: Natural -> DataBase -> [StateBefore]
stateBefore v_10344 db
  = M.foldlWithKey'
      (\ rest v_10346 vals ->
         concatMap
           (\ v_10347 ->
              pure StateBefore <*> mlbs v_10346 v_10344 <*> pure v_10347)
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