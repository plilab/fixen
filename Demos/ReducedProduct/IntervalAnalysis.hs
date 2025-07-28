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
step db fact q_13024
  = case fact of
        SeqFact f@(Seq v_13025 v_13026) -> Q.unions
                                             [q_13024,
                                              foldl'
                                                (\ q_13027 f@(Seq l10 l20) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_13027,
                                                         foldl'
                                                           (\ q_13029 f@(StateBefore l20_13028 st20)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13029,
                                                                    foldl'
                                                                      (\ q_13031
                                                                         f@(StateAfter l10_13030
                                                                              st10)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_13031,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepAfterCont
                                                                                       l10_13030
                                                                                       l20_13028
                                                                                       st10
                                                                                       st20))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_13032 vals ->
                                                                            concatMap
                                                                              (\ v_13033 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_13032 l10
                                                                                   <*> pure v_13033)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13034 vals ->
                                                                 concatMap
                                                                   (\ v_13035 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13034 l20
                                                                        <*> pure v_13035)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         foldl'
                                                           (\ q_13037 f@(StateBefore l20_13036 st21)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13037,
                                                                    foldl'
                                                                      (\ q_13039
                                                                         f@(StateAfter l10_13038
                                                                              st11)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_13039,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (SeqStepBeforeCont
                                                                                       l10_13038
                                                                                       l20_13036
                                                                                       st11
                                                                                       st21))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_13040 vals ->
                                                                            concatMap
                                                                              (\ v_13041 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_13040 l10
                                                                                   <*> pure v_13041)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13042 vals ->
                                                                 concatMap
                                                                   (\ v_13043 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13042 l20
                                                                        <*> pure v_13043)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_13044, v_13045) ->
                                                      Seq v_13044 v_13045 : rest)
                                                   []
                                                   (S.singleton (v_13025, v_13026)))]
        PhiFact f@(Phi v_13046) -> Q.unions
                                     [q_13024,
                                      foldl'
                                        (\ q_13047 f@(Phi l1) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_13047,
                                                 foldl'
                                                   (\ q_13049 f@(StateBefore l1_13048 st1) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_13049,
                                                            Q.singleton
                                                              (traceConclusion
                                                                 (PhiStepCont l1_13048 st1))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_13050 vals ->
                                                         concatMap
                                                           (\ v_13051 ->
                                                              pure StateBefore <*> mlbs v_13050 l1
                                                                <*> pure v_13051)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateBefore db)),
                                                 Q.singleton
                                                   (traceConclusion (PhiInitAfterCont l1)),
                                                 Q.singleton (traceConclusion (PhiInitCont l1))]))
                                        Q.empty
                                        (S.foldl' (\ rest v_13052 -> Phi v_13052 : rest) []
                                           (S.singleton v_13046))]
        CondFact f@(Cond v_13053 v_13054 v_13055 v_13056) -> Q.unions
                                                               [q_13024,
                                                                foldl'
                                                                  (\ q_13057
                                                                     f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_13057,
                                                                           foldl'
                                                                             (\ q_13059
                                                                                f@(StateBefore
                                                                                     jlf0_13058
                                                                                     stf0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_13059,
                                                                                      foldl'
                                                                                        (\ q_13061
                                                                                           f@(StateBefore
                                                                                                lcond0_13060
                                                                                                stc0)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_13061,
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
                                                                                                        [q_13061,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondFalseCont
                                                                                                                 lcond0_13060
                                                                                                                 stc0
                                                                                                                 e0
                                                                                                                 jlt0
                                                                                                                 jlf0_13058
                                                                                                                 stf0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_13062
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_13063
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_13062
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_13063)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_13064 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_13065 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_13064
                                                                                            jlf0
                                                                                          <*>
                                                                                          pure
                                                                                            v_13065)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsStateBefore
                                                                                   db)),
                                                                           foldl'
                                                                             (\ q_13067
                                                                                f@(StateBefore
                                                                                     jlt0_13066
                                                                                     stt0)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_13067,
                                                                                      foldl'
                                                                                        (\ q_13069
                                                                                           f@(StateBefore
                                                                                                lcond0_13068
                                                                                                stc1)
                                                                                           ->
                                                                                           trace
                                                                                             ("got: "
                                                                                                ++
                                                                                                show
                                                                                                  f)
                                                                                             (Q.unions
                                                                                                [q_13069,
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
                                                                                                        [q_13069,
                                                                                                         Q.singleton
                                                                                                           (traceConclusion
                                                                                                              (EvalCondTrueCont
                                                                                                                 lcond0_13068
                                                                                                                 stc1
                                                                                                                 e0
                                                                                                                 jlt0_13066
                                                                                                                 jlf0
                                                                                                                 stt0))])
                                                                                                   else
                                                                                                   Q.empty]))
                                                                                        Q.empty
                                                                                        (M.foldlWithKey'
                                                                                           (\ rest
                                                                                              v_13070
                                                                                              vals
                                                                                              ->
                                                                                              concatMap
                                                                                                (\ v_13071
                                                                                                   ->
                                                                                                   pure
                                                                                                     StateBefore
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_13070
                                                                                                       lcond0
                                                                                                     <*>
                                                                                                     pure
                                                                                                       v_13071)
                                                                                                vals
                                                                                                ++
                                                                                                rest)
                                                                                           []
                                                                                           (factsStateBefore
                                                                                              db))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest v_13072 vals
                                                                                   ->
                                                                                   concatMap
                                                                                     (\ v_13073 ->
                                                                                        pure
                                                                                          StateBefore
                                                                                          <*>
                                                                                          mlbs
                                                                                            v_13072
                                                                                            jlt0
                                                                                          <*>
                                                                                          pure
                                                                                            v_13073)
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
                                                                     (\ rest v_13074 vals ->
                                                                        S.foldl'
                                                                          (\ acc
                                                                             (v_13075, v_13076,
                                                                              v_13077)
                                                                             ->
                                                                             Cond v_13074 v_13075
                                                                               v_13076
                                                                               v_13077
                                                                               : acc)
                                                                          []
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (M.singleton v_13053
                                                                        (S.singleton
                                                                           (v_13054, v_13055,
                                                                            v_13056))))]
        AssignFact f@(Assign v_13078 v_13079 v_13080) -> Q.unions
                                                           [q_13024,
                                                            foldl'
                                                              (\ q_13081 f@(Assign lAssign0 x1 e2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_13081,
                                                                       foldl'
                                                                         (\ q_13083
                                                                            f@(StateBefore
                                                                                 lAssign0_13082
                                                                                 stBefore0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13083,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (AssignStepCont
                                                                                          lAssign0_13082
                                                                                          x1
                                                                                          e2
                                                                                          stBefore0))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_13084 vals ->
                                                                               concatMap
                                                                                 (\ v_13085 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_13084
                                                                                        lAssign0
                                                                                      <*>
                                                                                      pure v_13085)
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
                                                                 (\ rest (v_13086, v_13087) vals ->
                                                                    S.foldl'
                                                                      (\ acc v_13088 ->
                                                                         Assign v_13086 v_13087
                                                                           v_13088
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton (v_13078, v_13079)
                                                                    (S.singleton v_13080)))]
        VarFact f@(Var v_13089 v_13090) -> Q.unions
                                             [q_13024,
                                              foldl'
                                                (\ q_13091 f@(Var l0 x0) ->
                                                   trace ("got: " ++ show f)
                                                     (Q.unions
                                                        [q_13091,
                                                         foldl'
                                                           (\ q_13093 f@(StateBefore l0_13092 st0)
                                                              ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_13093,
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (VarStepAfterCont l0_13092
                                                                            x0
                                                                            st0))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest v_13094 vals ->
                                                                 concatMap
                                                                   (\ v_13095 ->
                                                                      pure StateBefore <*>
                                                                        mlbs v_13094 l0
                                                                        <*> pure v_13095)
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (factsStateBefore db)),
                                                         Q.singleton
                                                           (traceConclusion
                                                              (VarInitAfterCont l0 x0))]))
                                                Q.empty
                                                (S.foldl'
                                                   (\ rest (v_13096, v_13097) ->
                                                      Var v_13096 v_13097 : rest)
                                                   []
                                                   (S.singleton (v_13089, v_13090)))]
        StateAfterFact f@(StateAfter v_13098 v_13099) -> Q.unions
                                                           [q_13024,
                                                            foldl'
                                                              (\ q_13100 f@(StateAfter l10 st10) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_13100,
                                                                       foldl'
                                                                         (\ q_13102
                                                                            f@(Seq l10_13101 l20) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13102,
                                                                                  foldl'
                                                                                    (\ q_13104
                                                                                       f@(StateBefore
                                                                                            l20_13103
                                                                                            st20)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_13104,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepAfterCont
                                                                                                     l10_13101
                                                                                                     l20_13103
                                                                                                     st10
                                                                                                     st20))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_13105
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_13106
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_13105
                                                                                                   l20
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_13106)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_13107, v_13108) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_13107 l10
                                                                                  <*> pure v_13108)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_13110
                                                                            f@(Seq l10_13109 l21) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_13110,
                                                                                  foldl'
                                                                                    (\ q_13112
                                                                                       f@(StateBefore
                                                                                            l21_13111
                                                                                            st21)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_13112,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (SeqStepBeforeCont
                                                                                                     l10_13109
                                                                                                     l21_13111
                                                                                                     st10
                                                                                                     st21))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_13113
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_13114
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_13113
                                                                                                   l21
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_13114)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest
                                                                               (v_13115, v_13116) ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_13115 l10
                                                                                  <*> pure v_13116)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_13117 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_13118 ->
                                                                         StateAfter v_13117 v_13118
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_13098
                                                                    (S.singleton v_13099)))]
        StateBeforeFact f@(StateBefore v_13119 v_13120) -> Q.unions
                                                             [q_13024,
                                                              foldl'
                                                                (\ q_13121 f@(StateBefore l0 st0) ->
                                                                   trace ("got: " ++ show f)
                                                                     (Q.unions
                                                                        [q_13121,
                                                                         foldl'
                                                                           (\ q_13123
                                                                              f@(Var l0_13122 x0) ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13123,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (VarStepAfterCont
                                                                                            l0_13122
                                                                                            x0
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest
                                                                                 (v_13124, v_13125)
                                                                                 ->
                                                                                 (pure Var <*>
                                                                                    mlbs v_13124 l0
                                                                                    <*>
                                                                                    pure v_13125)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsVar db)),
                                                                         foldl'
                                                                           (\ q_13126
                                                                              f@(StateAfter l10
                                                                                   st10)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13126,
                                                                                    foldl'
                                                                                      (\ q_13129
                                                                                         f@(Seq
                                                                                              l10_13127
                                                                                              l0_13128)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13129,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepAfterCont
                                                                                                       l10_13127
                                                                                                       l0_13128
                                                                                                       st10
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_13130,
                                                                                             v_13131)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13130
                                                                                                 l10
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13131
                                                                                                 l0)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13132 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13133 ->
                                                                                      StateAfter
                                                                                        v_13132
                                                                                        v_13133
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateAfter db)),
                                                                         foldl'
                                                                           (\ q_13134
                                                                              f@(StateAfter l11
                                                                                   st11)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13134,
                                                                                    foldl'
                                                                                      (\ q_13137
                                                                                         f@(Seq
                                                                                              l11_13135
                                                                                              l0_13136)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13137,
                                                                                               Q.singleton
                                                                                                 (traceConclusion
                                                                                                    (SeqStepBeforeCont
                                                                                                       l11_13135
                                                                                                       l0_13136
                                                                                                       st11
                                                                                                       st0))]))
                                                                                      Q.empty
                                                                                      (foldl'
                                                                                         (\ rest
                                                                                            (v_13138,
                                                                                             v_13139)
                                                                                            ->
                                                                                            (pure
                                                                                               Seq
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13138
                                                                                                 l11
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_13139
                                                                                                 l0)
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsSeq
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13140 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13141 ->
                                                                                      StateAfter
                                                                                        v_13140
                                                                                        v_13141
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateAfter db)),
                                                                         foldl'
                                                                           (\ q_13143
                                                                              f@(Phi l0_13142) ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13143,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (PhiStepCont
                                                                                            l0_13142
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (foldl'
                                                                              (\ rest v_13144 ->
                                                                                 (pure Phi <*>
                                                                                    mlbs v_13144 l0)
                                                                                   ++ rest)
                                                                              []
                                                                              (factsPhi db)),
                                                                         foldl'
                                                                           (\ q_13146
                                                                              f@(Cond lcond0 e0 jlt0
                                                                                   l0_13145)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13146,
                                                                                    foldl'
                                                                                      (\ q_13148
                                                                                         f@(StateBefore
                                                                                              lcond0_13147
                                                                                              stc0)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13148,
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
                                                                                                      [q_13148,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               lcond0_13147
                                                                                                               stc0
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               l0_13145
                                                                                                               st0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13149
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_13150
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13149
                                                                                                     lcond0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13150)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13151 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_13152,
                                                                                       v_13153,
                                                                                       v_13154)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_13151
                                                                                        <*>
                                                                                        pure v_13152
                                                                                        <*>
                                                                                        pure v_13153
                                                                                        <*>
                                                                                        mlbs v_13154
                                                                                          l0)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_13155
                                                                              f@(StateBefore jlf0
                                                                                   stf0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13155,
                                                                                    foldl'
                                                                                      (\ q_13158
                                                                                         f@(Cond
                                                                                              l0_13156
                                                                                              e0
                                                                                              jlt0
                                                                                              jlf0_13157)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13158,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e0
                                                                                                      st0)
                                                                                                   False
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_13158,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondFalseCont
                                                                                                               l0_13156
                                                                                                               st0
                                                                                                               e0
                                                                                                               jlt0
                                                                                                               jlf0_13157
                                                                                                               stf0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13159
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_13160,
                                                                                                  v_13161,
                                                                                                  v_13162)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13159
                                                                                                     l0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13160
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13161
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13162
                                                                                                     jlf0)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13163 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13164 ->
                                                                                      StateBefore
                                                                                        v_13163
                                                                                        v_13164
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_13166
                                                                              f@(Cond lcond1 e1
                                                                                   l0_13165 jlf1)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13166,
                                                                                    foldl'
                                                                                      (\ q_13168
                                                                                         f@(StateBefore
                                                                                              lcond1_13167
                                                                                              stc1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13168,
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
                                                                                                      [q_13168,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               lcond1_13167
                                                                                                               stc1
                                                                                                               e1
                                                                                                               l0_13165
                                                                                                               jlf1
                                                                                                               st0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13169
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ v_13170
                                                                                                 ->
                                                                                                 pure
                                                                                                   StateBefore
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13169
                                                                                                     lcond1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13170)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsStateBefore
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13171 vals
                                                                                 ->
                                                                                 concatMap
                                                                                   (\ (v_13172,
                                                                                       v_13173,
                                                                                       v_13174)
                                                                                      ->
                                                                                      pure Cond <*>
                                                                                        pure v_13171
                                                                                        <*>
                                                                                        pure v_13172
                                                                                        <*>
                                                                                        mlbs v_13173
                                                                                          l0
                                                                                        <*>
                                                                                        pure
                                                                                          v_13174)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsCond db)),
                                                                         foldl'
                                                                           (\ q_13175
                                                                              f@(StateBefore jlt1
                                                                                   stt0)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13175,
                                                                                    foldl'
                                                                                      (\ q_13178
                                                                                         f@(Cond
                                                                                              l0_13176
                                                                                              e1
                                                                                              jlt1_13177
                                                                                              jlf1)
                                                                                         ->
                                                                                         trace
                                                                                           ("got: "
                                                                                              ++
                                                                                              show
                                                                                                f)
                                                                                           (Q.unions
                                                                                              [q_13178,
                                                                                               if
                                                                                                 eq
                                                                                                   (evaluateConditional
                                                                                                      e1
                                                                                                      st0)
                                                                                                   True
                                                                                                 then
                                                                                                 trace
                                                                                                   ("got: "
                                                                                                      ++
                                                                                                      show
                                                                                                        f)
                                                                                                   (Q.unions
                                                                                                      [q_13178,
                                                                                                       Q.singleton
                                                                                                         (traceConclusion
                                                                                                            (EvalCondTrueCont
                                                                                                               l0_13176
                                                                                                               st0
                                                                                                               e1
                                                                                                               jlt1_13177
                                                                                                               jlf1
                                                                                                               stt0))])
                                                                                                 else
                                                                                                 Q.empty]))
                                                                                      Q.empty
                                                                                      (M.foldlWithKey'
                                                                                         (\ rest
                                                                                            v_13179
                                                                                            vals ->
                                                                                            concatMap
                                                                                              (\ (v_13180,
                                                                                                  v_13181,
                                                                                                  v_13182)
                                                                                                 ->
                                                                                                 pure
                                                                                                   Cond
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13179
                                                                                                     l0
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13180
                                                                                                   <*>
                                                                                                   mlbs
                                                                                                     v_13181
                                                                                                     jlt1
                                                                                                   <*>
                                                                                                   pure
                                                                                                     v_13182)
                                                                                              vals
                                                                                              ++
                                                                                              rest)
                                                                                         []
                                                                                         (factsCond
                                                                                            db))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest v_13183 vals
                                                                                 ->
                                                                                 S.foldl'
                                                                                   (\ acc v_13184 ->
                                                                                      StateBefore
                                                                                        v_13183
                                                                                        v_13184
                                                                                        : acc)
                                                                                   []
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsStateBefore
                                                                                 db)),
                                                                         foldl'
                                                                           (\ q_13186
                                                                              f@(Assign l0_13185 x1
                                                                                   e2)
                                                                              ->
                                                                              trace
                                                                                ("got: " ++ show f)
                                                                                (Q.unions
                                                                                   [q_13186,
                                                                                    Q.singleton
                                                                                      (traceConclusion
                                                                                         (AssignStepCont
                                                                                            l0_13185
                                                                                            x1
                                                                                            e2
                                                                                            st0))]))
                                                                           Q.empty
                                                                           (M.foldlWithKey'
                                                                              (\ rest
                                                                                 (v_13187, v_13188)
                                                                                 vals ->
                                                                                 concatMap
                                                                                   (\ v_13189 ->
                                                                                      pure Assign
                                                                                        <*>
                                                                                        mlbs v_13187
                                                                                          l0
                                                                                        <*>
                                                                                        pure v_13188
                                                                                        <*>
                                                                                        pure
                                                                                          v_13189)
                                                                                   vals
                                                                                   ++ rest)
                                                                              []
                                                                              (factsAssign db))]))
                                                                Q.empty
                                                                (M.foldlWithKey'
                                                                   (\ rest v_13190 vals ->
                                                                      S.foldl'
                                                                        (\ acc v_13191 ->
                                                                           StateBefore v_13190
                                                                             v_13191
                                                                             : acc)
                                                                        []
                                                                        vals
                                                                        ++ rest)
                                                                   []
                                                                   (M.singleton v_13119
                                                                      (S.singleton v_13120)))]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_13192 db
  = M.foldlWithKey'
      (\ rest v_13194 vals ->
         concatMap
           (\ v_13195 ->
              pure StateAfter <*> mlbs v_13194 v_13192 <*> pure v_13195)
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