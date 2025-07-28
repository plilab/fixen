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
step db fact q_1271
  = case fact of
        SeqFact f@(Seq v_1272 v_1273) -> Q.unions
                                           [q_1271,
                                            foldl'
                                              (\ q_1274 f@(Seq l0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1274,
                                                       foldl'
                                                         (\ q_1276 f@(Phi l0_1275) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1276,
                                                                  foldl'
                                                                    (\ q_1278
                                                                       f@(StateBefore l0_1275_1277
                                                                            st0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1278,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (PhiStepCont
                                                                                     l0_1275_1277
                                                                                     st0
                                                                                     lAfter0))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1279 vals ->
                                                                          concatMap
                                                                            (\ v_1280 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1279 l0_1275
                                                                                 <*> pure v_1280)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (foldl'
                                                            (\ rest v_1281 ->
                                                               (pure Phi <*> mlbs v_1281 l0) ++
                                                                 rest)
                                                            []
                                                            (factsPhi db)),
                                                       foldl'
                                                         (\ q_1283 f@(Assign l0_1282 x0 e2) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1283,
                                                                  foldl'
                                                                    (\ q_1285
                                                                       f@(StateBefore lAfter0_1284
                                                                            stAfter0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1285,
                                                                             foldl'
                                                                               (\ q_1287
                                                                                  f@(StateBefore
                                                                                       l0_1282_1286
                                                                                       stAssign0)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_1287,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                l0_1282_1286
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_1284
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest v_1288
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_1289 ->
                                                                                          pure
                                                                                            StateBefore
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_1288
                                                                                              l0_1282
                                                                                            <*>
                                                                                            pure
                                                                                              v_1289)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsStateBefore
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1290 vals ->
                                                                          concatMap
                                                                            (\ v_1291 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1290 lAfter0
                                                                                 <*> pure v_1291)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest (v_1292, v_1293) vals ->
                                                               concatMap
                                                                 (\ v_1294 ->
                                                                    pure Assign <*> mlbs v_1292 l0
                                                                      <*> pure v_1293
                                                                      <*> pure v_1294)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsAssign db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1295, v_1296) ->
                                                    Seq v_1295 v_1296 : rest)
                                                 []
                                                 (S.singleton (v_1272, v_1273)))]
        PhiFact f@(Phi v_1297) -> Q.unions
                                    [q_1271,
                                     foldl'
                                       (\ q_1298 f@(Phi l0) ->
                                          trace ("got: " ++ show f)
                                            (Q.unions
                                               [q_1298,
                                                foldl'
                                                  (\ q_1300 f@(Seq l0_1299 lAfter0) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_1300,
                                                           foldl'
                                                             (\ q_1302
                                                                f@(StateBefore l0_1299_1301 st0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_1302,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (PhiStepCont l0_1299_1301
                                                                              st0
                                                                              lAfter0))]))
                                                             Q.empty
                                                             (M.foldlWithKey'
                                                                (\ rest v_1303 vals ->
                                                                   concatMap
                                                                     (\ v_1304 ->
                                                                        pure StateBefore <*>
                                                                          mlbs v_1303 l0_1299
                                                                          <*> pure v_1304)
                                                                     vals
                                                                     ++ rest)
                                                                []
                                                                (factsStateBefore db))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (v_1305, v_1306) ->
                                                        (pure Seq <*> mlbs v_1305 l0 <*>
                                                           pure v_1306)
                                                          ++ rest)
                                                     []
                                                     (factsSeq db)),
                                                Q.singleton (traceConclusion (PhiInitCont l0))]))
                                       Q.empty
                                       (S.foldl' (\ rest v_1307 -> Phi v_1307 : rest) []
                                          (S.singleton v_1297))]
        CondFact f@(Cond v_1308 v_1309 v_1310 v_1311) -> Q.unions
                                                           [q_1271,
                                                            foldl'
                                                              (\ q_1312 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1312,
                                                                       foldl'
                                                                         (\ q_1314
                                                                            f@(StateBefore jlf0_1313
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1314,
                                                                                  foldl'
                                                                                    (\ q_1316
                                                                                       f@(StateBefore
                                                                                            lcond0_1315
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1316,
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
                                                                                                    [q_1316,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1315
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1313
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1317
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1318
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1317
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1318)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1319 vals ->
                                                                               concatMap
                                                                                 (\ v_1320 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1319
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1320)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1322
                                                                            f@(StateBefore jlt0_1321
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1322,
                                                                                  foldl'
                                                                                    (\ q_1324
                                                                                       f@(StateBefore
                                                                                            lcond0_1323
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1324,
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
                                                                                                    [q_1324,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond0_1323
                                                                                                             stc1
                                                                                                             e0
                                                                                                             jlt0_1321
                                                                                                             jlf0
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1325
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1326
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1325
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1326)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1327 vals ->
                                                                               concatMap
                                                                                 (\ v_1328 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1327
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_1328)
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
                                                                 (\ rest v_1329 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1330, v_1331, v_1332) ->
                                                                         Cond v_1329 v_1330 v_1331
                                                                           v_1332
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1308
                                                                    (S.singleton
                                                                       (v_1309, v_1310, v_1311))))]
        AssignFact f@(Assign v_1333 v_1334 v_1335) -> Q.unions
                                                        [q_1271,
                                                         foldl'
                                                           (\ q_1336 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1336,
                                                                    foldl'
                                                                      (\ q_1337
                                                                         f@(StateBefore lAfter1
                                                                              stAfter0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1337,
                                                                               foldl'
                                                                                 (\ q_1339
                                                                                    f@(StateBefore
                                                                                         lAssign0_1338
                                                                                         stAssign0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_1339,
                                                                                          foldl'
                                                                                            (\ q_1342
                                                                                               f@(Seq
                                                                                                    lAssign0_1338_1340
                                                                                                    lAfter1_1341)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1342,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_1338_1340
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter1_1341
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_1343,
                                                                                                   v_1344)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1343
                                                                                                       lAssign0_1338
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1344
                                                                                                       lAfter1)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_1345
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_1346
                                                                                            ->
                                                                                            pure
                                                                                              StateBefore
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_1345
                                                                                                lAssign0
                                                                                              <*>
                                                                                              pure
                                                                                                v_1346)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1347 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_1348 ->
                                                                                 StateBefore v_1347
                                                                                   v_1348
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
                                                              (\ rest (v_1349, v_1350) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1351 ->
                                                                      Assign v_1349 v_1350 v_1351 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1333, v_1334)
                                                                 (S.singleton v_1335)))]
        VarFact f@(Var v_1352 v_1353) -> Q.unions
                                           [q_1271,
                                            foldl'
                                              (\ q_1354 f@(Var l1 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1354,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l1 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1355, v_1356) ->
                                                    Var v_1355 v_1356 : rest)
                                                 []
                                                 (S.singleton (v_1352, v_1353)))]
        StateBeforeFact f@(StateBefore v_1357 v_1358) -> Q.unions
                                                           [q_1271,
                                                            foldl'
                                                              (\ q_1359 f@(StateBefore l0 st0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1359,
                                                                       foldl'
                                                                         (\ q_1361
                                                                            f@(Seq l0_1360 lAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1361,
                                                                                  foldl'
                                                                                    (\ q_1363
                                                                                       f@(Phi
                                                                                            l0_1360_1362)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1363,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (PhiStepCont
                                                                                                     l0_1360_1362
                                                                                                     st0
                                                                                                     lAfter0))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          v_1364 ->
                                                                                          (pure Phi
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1364
                                                                                               l0_1360)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsPhi
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1365, v_1366)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_1365 l0
                                                                                  <*> pure v_1366)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_1368
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 l0_1367)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1368,
                                                                                  foldl'
                                                                                    (\ q_1370
                                                                                       f@(StateBefore
                                                                                            lcond0_1369
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1370,
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
                                                                                                    [q_1370,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             lcond0_1369
                                                                                                             stc0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             l0_1367
                                                                                                             st0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1371
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1372
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1371
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1372)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1373 vals ->
                                                                               concatMap
                                                                                 (\ (v_1374, v_1375,
                                                                                     v_1376)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1373
                                                                                      <*>
                                                                                      pure v_1374
                                                                                      <*>
                                                                                      pure v_1375
                                                                                      <*>
                                                                                      mlbs v_1376
                                                                                        l0)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1377
                                                                            f@(StateBefore jlf0
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1377,
                                                                                  foldl'
                                                                                    (\ q_1380
                                                                                       f@(Cond
                                                                                            l0_1378
                                                                                            e0 jlt0
                                                                                            jlf0_1379)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1380,
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
                                                                                                    [q_1380,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondFalseCont
                                                                                                             l0_1378
                                                                                                             st0
                                                                                                             e0
                                                                                                             jlt0
                                                                                                             jlf0_1379
                                                                                                             stf0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1381
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1382,
                                                                                                v_1383,
                                                                                                v_1384)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1381
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1382
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1383
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1384
                                                                                                   jlf0)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1385 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1386 ->
                                                                                    StateBefore
                                                                                      v_1385
                                                                                      v_1386
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1388
                                                                            f@(Cond lcond1 e1
                                                                                 l0_1387 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1388,
                                                                                  foldl'
                                                                                    (\ q_1390
                                                                                       f@(StateBefore
                                                                                            lcond1_1389
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1390,
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
                                                                                                    [q_1390,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             lcond1_1389
                                                                                                             stc1
                                                                                                             e1
                                                                                                             l0_1387
                                                                                                             jlf1
                                                                                                             st0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1391
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1392
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1391
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1392)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1393 vals ->
                                                                               concatMap
                                                                                 (\ (v_1394, v_1395,
                                                                                     v_1396)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1393
                                                                                      <*>
                                                                                      pure v_1394
                                                                                      <*>
                                                                                      mlbs v_1395 l0
                                                                                      <*>
                                                                                      pure v_1396)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1397
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1397,
                                                                                  foldl'
                                                                                    (\ q_1400
                                                                                       f@(Cond
                                                                                            l0_1398
                                                                                            e1
                                                                                            jlt1_1399
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1400,
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
                                                                                                    [q_1400,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (EvalCondTrueCont
                                                                                                             l0_1398
                                                                                                             st0
                                                                                                             e1
                                                                                                             jlt1_1399
                                                                                                             jlf1
                                                                                                             stt0))])
                                                                                               else
                                                                                               Q.empty]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1401
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1402,
                                                                                                v_1403,
                                                                                                v_1404)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1401
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1402
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1403
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1404)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1405 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1406 ->
                                                                                    StateBefore
                                                                                      v_1405
                                                                                      v_1406
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1407
                                                                            f@(StateBefore lAssign0
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1407,
                                                                                  foldl'
                                                                                    (\ q_1409
                                                                                       f@(Assign
                                                                                            lAssign0_1408
                                                                                            x0 e2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1409,
                                                                                             foldl'
                                                                                               (\ q_1412
                                                                                                  f@(Seq
                                                                                                       lAssign0_1408_1410
                                                                                                       l0_1411)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1412,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_1408_1410
                                                                                                                x0
                                                                                                                e2
                                                                                                                stAssign0
                                                                                                                l0_1411
                                                                                                                st0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_1413,
                                                                                                      v_1414)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1413
                                                                                                          lAssign0_1408
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1414
                                                                                                          l0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          (v_1415,
                                                                                           v_1416)
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1417
                                                                                               ->
                                                                                               pure
                                                                                                 Assign
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1415
                                                                                                   lAssign0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1416
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1417)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsAssign
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1418 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1419 ->
                                                                                    StateBefore
                                                                                      v_1418
                                                                                      v_1419
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1421
                                                                            f@(Assign l0_1420 x0 e2)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1421,
                                                                                  foldl'
                                                                                    (\ q_1423
                                                                                       f@(Seq
                                                                                            l0_1420_1422
                                                                                            lAfter1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1423,
                                                                                             foldl'
                                                                                               (\ q_1425
                                                                                                  f@(StateBefore
                                                                                                       lAfter1_1424
                                                                                                       stAfter0)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1425,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                l0_1420_1422
                                                                                                                x0
                                                                                                                e2
                                                                                                                st0
                                                                                                                lAfter1_1424
                                                                                                                stAfter0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     v_1426
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1427
                                                                                                          ->
                                                                                                          pure
                                                                                                            StateBefore
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1426
                                                                                                              lAfter1
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1427)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsStateBefore
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_1428,
                                                                                           v_1429)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1428
                                                                                               l0_1420
                                                                                             <*>
                                                                                             pure
                                                                                               v_1429)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest (v_1430, v_1431)
                                                                               vals ->
                                                                               concatMap
                                                                                 (\ v_1432 ->
                                                                                    pure Assign <*>
                                                                                      mlbs v_1430 l0
                                                                                      <*>
                                                                                      pure v_1431
                                                                                      <*>
                                                                                      pure v_1432)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsAssign db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1433 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1434 ->
                                                                         StateBefore v_1433 v_1434 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1357
                                                                    (S.singleton v_1358)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_1436 vals ->
         S.foldl' (\ acc v_1437 -> StateBefore v_1436 v_1437 : acc) [] vals
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