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
                  | SeqStepAfterCont Natural Natural State State
                  | VarStepAfterCont Natural String State
                  | AssignStepCont Natural String Expr State
                  | EvalCondFalseCont Natural State Expr Natural Natural State
                  | CondInitAfterCont Natural Expr Natural Natural
                  | PhiStepCont Natural State
                  | CondInitCont Natural Expr Natural Natural
                  | SeqStepBeforeCont Natural Natural State State
                  | AssignInitAfterCont Natural String Expr
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | PhiInitCont Natural
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (VarInitAfterCont l5 x3)
  = [StateAfterFact (StateAfter l5 (singleton x3 Bot))]
evaluate db (AssignInitCont l8 x4 e6)
  = [StateBeforeFact (StateBefore l8 (empty 0))]
evaluate db (PhiInitAfterCont l2)
  = [StateAfterFact (StateAfter l2 (empty 0))]
evaluate db (SeqStepAfterCont l10 l20 st10 st20)
  = [StateBeforeFact (StateBefore l20 (join st20 st10))]
evaluate db (VarStepAfterCont l0 x0 st0)
  = [StateAfterFact (StateAfter l0 (insert x0 Bot st0))]
evaluate db (AssignStepCont lAssign0 x1 e2 stBefore0)
  = [StateAfterFact
       (StateAfter lAssign0 (insert x1 (eval e2 stBefore0) stBefore0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db (CondInitAfterCont l3 e3 jlt2 jlf2)
  = [StateAfterFact (StateAfter l3 (empty 0))]
evaluate db (PhiStepCont l1 st1)
  = [StateAfterFact (StateAfter l1 st1)]
evaluate db (CondInitCont l7 e5 jlt3 jlf3)
  = [StateBeforeFact (StateBefore l7 (empty 0))]
evaluate db (SeqStepBeforeCont l11 l21 st11 st21)
  = [StateBeforeFact (StateBefore l21 (join st21 st11))]
evaluate db (AssignInitAfterCont l4 x2 e4)
  = [StateAfterFact (StateAfter l4 (empty 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (join (narrowConditional e1 stc1) stt0))]
evaluate db (PhiInitCont l6)
  = [StateBeforeFact (StateBefore l6 (empty 0))]

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
step db fact q_13196
  = case fact of
        SeqFact f@(Seq v_13197 v_13198) -> Q.unions
                                             [q_13196,
                                              foldl'
                                                (\ q_13199 f@(Seq l10 l20) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_13199,
                                                         foldl'
                                                           (\ q_13201 f@(StateBefore l20_13200 st20)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13201,
                                                                    foldl'
                                                                      (\ q_13203
                                                                         f@(StateAfter l10_13202
                                                                              st10)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_13203,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepAfterCont
                                                                                       l10_13202
                                                                                       l20_13200
                                                                                       st10
                                                                                       st20))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_13204 vals ->
                                                                            concatMap
                                                                              (\ v_13205 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_13204 l10
                                                                                   <*> pure v_13205)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13206 vals ->
                                                                 concatMap
                                                                   (\ v_13207 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13206 l20
                                                                        <*> pure v_13207)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         foldl'
                                                           (\ q_13209 f@(StateBefore l20_13208 st21)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13209,
                                                                    foldl'
                                                                      (\ q_13211
                                                                         f@(StateAfter l10_13210
                                                                              st11)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_13211,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepBeforeCont
                                                                                       l10_13210
                                                                                       l20_13208
                                                                                       st11
                                                                                       st21))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_13212 vals ->
                                                                            concatMap
                                                                              (\ v_13213 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_13212 l10
                                                                                   <*> pure v_13213)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13214 vals ->
                                                                 concatMap
                                                                   (\ v_13215 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13214 l20
                                                                        <*> pure v_13215)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_13216, v_13217) ->
                                                      Seq v_13216 v_13217 : rest)
                                                   []
                                                   (S.singleton (v_13197, v_13198)))]
        PhiFact f@(Phi v_13218) -> Q.unions
                                     [q_13196,
                                      foldl'
                                        (\ q_13219 f@(Phi l1) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_13219,
                                                 foldl'
                                                   (\ q_13221 f@(StateBefore l1_13220 st1) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_13221,
                                                            Q.singleton
                                                              (traceConclusion
                                                                 (PhiStepCont l1_13220 st1))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_13222 vals ->
                                                         concatMap
                                                           (\ v_13223 ->
                                                              pure StateBefore <*> mlbs v_13222 l1
                                                                <*> pure v_13223)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateBefore db)),
                                                 Q.singleton
                                                   (traceConclusion (PhiInitAfterCont l1)),
                                                 Q.singleton (traceConclusion (PhiInitCont l1))]))
                                        Q.empty
                                        (S.foldl' (\ rest v_13224 -> Phi v_13224 : rest) []
                                           (S.singleton v_13218))]
        CondFact f@(Cond v_13225 v_13226 v_13227 v_13228) -> Q.unions
                                                               [q_13196,
                                                                foldl'
                                                                  (\ q_13229
                                                                     f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_13229,
                                                                           foldl'
                                                                             (\ q_13231
                                                                                f@(StateBefore
                                                                                     jlf0_13230
                                                                                     stf0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_13231,
                                                                                      foldl'
                                                                                        (\ q_13233
                                                                                           f@(StateBefore
                                                                                                lcond0_13232
                                                                                                stc0)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_13233,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondFalseCont
                                                                                                         lcond0_13232
                                                                                                         stc0
                                                                                                         e0
                                                                                                         jlt0
                                                                                                         jlf0_13230
                                                                                                         stf0))]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_13234
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_13235
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_13234
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_13235)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_13236 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_13237 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_13236
                                                                                            jlf0
                                                                                          <*>
                                                                                          pure
                                                                                            v_13237)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           foldl'
                                                                             (\ q_13239
                                                                                f@(StateBefore
                                                                                     jlt0_13238
                                                                                     stt0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_13239,
                                                                                      foldl'
                                                                                        (\ q_13241
                                                                                           f@(StateBefore
                                                                                                lcond0_13240
                                                                                                stc1)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_13241,
                                                                                                 Q.singleton
                                                                                                   (traceConclusion
                                                                                                      (EvalCondTrueCont
                                                                                                         lcond0_13240
                                                                                                         stc1
                                                                                                         e0
                                                                                                         jlt0_13238
                                                                                                         jlf0
                                                                                                         stt0))]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_13242
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_13243
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_13242
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_13243)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_13244 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_13245 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_13244
                                                                                            jlt0
                                                                                          <*>
                                                                                          pure
                                                                                            v_13245)
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
                                                                     (\ rest v_13246 vals ->
                                                                        S.foldl'
                                                                          (\ acc
                                                                             (v_13247, v_13248,
                                                                              v_13249)
                                                                             ->
                                                                             Cond v_13246 v_13247
                                                                               v_13248
                                                                               v_13249
                                                                               : acc)
                                                                          []
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (M.singleton v_13225
                                                                        (S.singleton
                                                                           (v_13226, v_13227,
                                                                            v_13228))))]
        AssignFact f@(Assign v_13250 v_13251 v_13252) -> Q.unions
                                                           [q_13196,
                                                            foldl'
                                                              (\ q_13253 f@(Assign lAssign0 x1 e2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_13253,
                                                                       foldl'
                                                                         (\ q_13255
                                                                            f@(StateBefore
                                                                                 lAssign0_13254
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13255,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (AssignStepCont
                                                                                          lAssign0_13254
                                                                                          x1
                                                                                          e2
                                                                                          stBefore0))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_13256 vals ->
                                                                               concatMap
                                                                                 (\ v_13257 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_13256
                                                                                        lAssign0
                                                                                      <*>
                                                                                      pure v_13257)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (AssignInitAfterCont
                                                                               lAssign0
                                                                               x1
                                                                               e2)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (AssignInitCont lAssign0
                                                                               x1
                                                                               e2))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest (v_13258, v_13259) vals ->
                                                                    S.foldl'
                                                                      (\ acc v_13260 ->
                                                                         Assign v_13258 v_13259
                                                                           v_13260
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton (v_13250, v_13251)
                                                                    (S.singleton v_13252)))]
        VarFact f@(Var v_13261 v_13262) -> Q.unions
                                             [q_13196,
                                              foldl'
                                                (\ q_13263 f@(Var l0 x0) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_13263,
                                                         foldl'
                                                           (\ q_13265 f@(StateBefore l0_13264 st0)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13265,
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (VarStepAfterCont l0_13264
                                                                            x0
                                                                            st0))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13266 vals ->
                                                                 concatMap
                                                                   (\ v_13267 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13266 l0
                                                                        <*> pure v_13267)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         Q.singleton
                                                           (traceConclusion
                                                              (VarInitAfterCont l0 x0))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_13268, v_13269) ->
                                                      Var v_13268 v_13269 : rest)
                                                   []
                                                   (S.singleton (v_13261, v_13262)))]
        StateAfterFact f@(StateAfter v_13270 v_13271) -> Q.unions
                                                           [q_13196,
                                                            foldl'
                                                              (\ q_13272 f@(StateAfter l10 st10) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_13272,
                                                                       foldl'
                                                                         (\ q_13274
                                                                            f@(Seq l10_13273 l20) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13274,
                                                                                  foldl'
                                                                                    (\ q_13276
                                                                                       f@(StateBefore
                                                                                            l20_13275
                                                                                            st20)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_13276,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepAfterCont
                                                                                                     l10_13273
                                                                                                     l20_13275
                                                                                                     st10
                                                                                                     st20))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_13277
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_13278
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_13277
                                                                                                   l20
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_13278)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_13279, v_13280) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_13279 l10
                                                                                  <*> pure v_13280)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_13282
                                                                            f@(Seq l10_13281 l21) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13282,
                                                                                  foldl'
                                                                                    (\ q_13284
                                                                                       f@(StateBefore
                                                                                            l21_13283
                                                                                            st21)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_13284,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepBeforeCont
                                                                                                     l10_13281
                                                                                                     l21_13283
                                                                                                     st10
                                                                                                     st21))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_13285
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_13286
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_13285
                                                                                                   l21
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_13286)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_13287, v_13288) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_13287 l10
                                                                                  <*> pure v_13288)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_13289 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_13290 ->
                                                                         StateAfter v_13289 v_13290
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_13270
                                                                    (S.singleton v_13271)))]
        StateBeforeFact f@(StateBefore v_13291 v_13292) -> Q.unions
                                                             [q_13196,
                                                              foldl'
                                                                (\ q_13293 f@(StateBefore l0 st0) ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_13293,
                                                                         foldl'
                                                                           (\ q_13295
                                                                              f@(Var l0_13294 x0) ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13295,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (VarStepAfterCont
                                                                                            l0_13294
                                                                                            x0
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_13296, v_13297)
                                                                                 ->
                                                                                 (pure Var <*>
                                                                                    mlbs v_13296 l0
                                                                                    <*>
                                                                                    pure v_13297)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsVar db)),
                                                                         foldl'
                                                                           (\ q_13298
                                                                              f@(StateAfter l10
                                                                                   st10)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13298,
                                                                                    foldl'
                                                                                      (\ q_13301
                                                                                         f@(Seq
                                                                                              l10_13299
                                                                                              l0_13300)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13301,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepAfterCont
                                                                                                       l10_13299
                                                                                                       l0_13300
                                                                                                       st10
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_13302,
                                                                                             v_13303)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13302
                                                                                                 l10
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13303
                                                                                                 l0)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13304 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13305 ->
                                                                                      StateAfter
                                                                                        v_13304
                                                                                        v_13305
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateAfter db)),
                                                                         foldl'
                                                                           (\ q_13306
                                                                              f@(StateAfter l11
                                                                                   st11)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13306,
                                                                                    foldl'
                                                                                      (\ q_13309
                                                                                         f@(Seq
                                                                                              l11_13307
                                                                                              l0_13308)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13309,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepBeforeCont
                                                                                                       l11_13307
                                                                                                       l0_13308
                                                                                                       st11
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_13310,
                                                                                             v_13311)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13310
                                                                                                 l11
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13311
                                                                                                 l0)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13312 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13313 ->
                                                                                      StateAfter
                                                                                        v_13312
                                                                                        v_13313
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateAfter db)),
                                                                         foldl'
                                                                           (\ q_13315
                                                                              f@(Phi l0_13314) ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13315,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (PhiStepCont
                                                                                            l0_13314
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest v_13316 ->
                                                                                 (pure Phi <*>
                                                                                    mlbs v_13316 l0)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsPhi db)),
                                                                         foldl'
                                                                           (\ q_13318
                                                                              f@(Cond lcond0 e0 jlt0
                                                                                   l0_13317)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13318,
                                                                                    foldl'
                                                                                      (\ q_13320
                                                                                         f@(StateBefore
                                                                                              lcond0_13319
                                                                                              stc0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13320,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondFalseCont
                                                                                                       lcond0_13319
                                                                                                       stc0
                                                                                                       e0
                                                                                                       jlt0
                                                                                                       l0_13317
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13321
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_13322
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13321
                                                                                                     lcond0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13322)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13323 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_13324,
                                                                                       v_13325,
                                                                                       v_13326)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_13323
                                                                                        <*>
                                                                                        pure v_13324
                                                                                        <*>
                                                                                        pure v_13325
                                                                                        <*>
                                                                                        mlbs v_13326
                                                                                          l0)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_13327
                                                                              f@(StateBefore jlf0
                                                                                   stf0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13327,
                                                                                    foldl'
                                                                                      (\ q_13330
                                                                                         f@(Cond
                                                                                              l0_13328
                                                                                              e0
                                                                                              jlt0
                                                                                              jlf0_13329)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13330,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondFalseCont
                                                                                                       l0_13328
                                                                                                       st0
                                                                                                       e0
                                                                                                       jlt0
                                                                                                       jlf0_13329
                                                                                                       stf0))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13331
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_13332,
                                                                                                  v_13333,
                                                                                                  v_13334)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13331
                                                                                                     l0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13332
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13333
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13334
                                                                                                     jlf0)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13335 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13336 ->
                                                                                      StateBefore
                                                                                        v_13335
                                                                                        v_13336
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_13338
                                                                              f@(Cond lcond1 e1
                                                                                   l0_13337 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13338,
                                                                                    foldl'
                                                                                      (\ q_13340
                                                                                         f@(StateBefore
                                                                                              lcond1_13339
                                                                                              stc1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13340,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondTrueCont
                                                                                                       lcond1_13339
                                                                                                       stc1
                                                                                                       e1
                                                                                                       l0_13337
                                                                                                       jlf1
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13341
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_13342
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13341
                                                                                                     lcond1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13342)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13343 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_13344,
                                                                                       v_13345,
                                                                                       v_13346)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_13343
                                                                                        <*>
                                                                                        pure v_13344
                                                                                        <*>
                                                                                        mlbs v_13345
                                                                                          l0
                                                                                        <*>
                                                                                        pure
                                                                                          v_13346)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_13347
                                                                              f@(StateBefore jlt1
                                                                                   stt0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13347,
                                                                                    foldl'
                                                                                      (\ q_13350
                                                                                         f@(Cond
                                                                                              l0_13348
                                                                                              e1
                                                                                              jlt1_13349
                                                                                              jlf1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13350,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (EvalCondTrueCont
                                                                                                       l0_13348
                                                                                                       st0
                                                                                                       e1
                                                                                                       jlt1_13349
                                                                                                       jlf1
                                                                                                       stt0))]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13351
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_13352,
                                                                                                  v_13353,
                                                                                                  v_13354)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13351
                                                                                                     l0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13352
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13353
                                                                                                     jlt1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13354)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13355 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13356 ->
                                                                                      StateBefore
                                                                                        v_13355
                                                                                        v_13356
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_13358
                                                                              f@(Assign l0_13357 x1
                                                                                   e2)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13358,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (AssignStepCont
                                                                                            l0_13357
                                                                                            x1
                                                                                            e2
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest
                                                                                 (v_13359, v_13360)
                                                                                 vals ->
                                                                                 concatMap
                                                                                   (\ v_13361 ->
                                                                                      pure Assign
                                                                                        <*>
                                                                                        mlbs v_13359
                                                                                          l0
                                                                                        <*>
                                                                                        pure v_13360
                                                                                        <*>
                                                                                        pure
                                                                                          v_13361)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsAssign db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_13362 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_13363 ->
                                                                           StateBefore v_13362
                                                                             v_13363
                                                                             : acc)
                                                                        []
                                                                        vals
                                                                        ++ rest)
                                                                   []
                                                                   (M.singleton v_13291
                                                                      (S.singleton v_13292)))]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_13364 db
  = M.foldlWithKey'
      (\ rest v_13366 vals ->
         concatMap
           (\ v_13367 ->
              pure StateAfter <*> mlbs v_13366 v_13364 <*> pure v_13367)
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