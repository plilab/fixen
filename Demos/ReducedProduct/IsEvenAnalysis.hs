{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.IsEvenAnalysis where
import ReducedProduct.IsEven
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
step db fact q_1337
  = case fact of
        SeqFact f@(Seq v_1338 v_1339) -> Q.unions
                                           [q_1337,
                                            foldl'
                                              (\ q_1340 f@(Seq l0 lAfter0) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1340,
                                                       foldl'
                                                         (\ q_1342 f@(Phi l0_1341) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1342,
                                                                  foldl'
                                                                    (\ q_1344
                                                                       f@(StateBefore l0_1341_1343
                                                                            st0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1344,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (PhiStepCont
                                                                                     l0_1341_1343
                                                                                     st0
                                                                                     lAfter0))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1345 vals ->
                                                                          concatMap
                                                                            (\ v_1346 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1345 l0_1341
                                                                                 <*> pure v_1346)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (foldl'
                                                            (\ rest v_1347 ->
                                                               (pure Phi <*> mlbs v_1347 l0) ++
                                                                 rest)
                                                            []
                                                            (factsPhi db)),
                                                       foldl'
                                                         (\ q_1349 f@(Assign l0_1348 x0 e2) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1349,
                                                                  foldl'
                                                                    (\ q_1351
                                                                       f@(StateBefore lAfter0_1350
                                                                            stAfter0)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1351,
                                                                             foldl'
                                                                               (\ q_1353
                                                                                  f@(StateBefore
                                                                                       l0_1348_1352
                                                                                       stAssign0)
                                                                                  ->
                                                                                  trace
                                                                                    ("got: " ++
                                                                                       show f)
                                                                                    (Q.unions
                                                                                       [q_1353,
                                                                                        Q.singleton
                                                                                          (traceConclusion
                                                                                             (AssignStepCont
                                                                                                l0_1348_1352
                                                                                                x0
                                                                                                e2
                                                                                                stAssign0
                                                                                                lAfter0_1350
                                                                                                stAfter0))]))
                                                                               Q.empty
                                                                               (M.foldlWithKey'
                                                                                  (\ rest v_1354
                                                                                     vals ->
                                                                                     concatMap
                                                                                       (\ v_1355 ->
                                                                                          pure
                                                                                            StateBefore
                                                                                            <*>
                                                                                            mlbs
                                                                                              v_1354
                                                                                              l0_1348
                                                                                            <*>
                                                                                            pure
                                                                                              v_1355)
                                                                                       vals
                                                                                       ++ rest)
                                                                                  []
                                                                                  (factsStateBefore
                                                                                     db))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1356 vals ->
                                                                          concatMap
                                                                            (\ v_1357 ->
                                                                               pure StateBefore <*>
                                                                                 mlbs v_1356 lAfter0
                                                                                 <*> pure v_1357)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateBefore db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest (v_1358, v_1359) vals ->
                                                               concatMap
                                                                 (\ v_1360 ->
                                                                    pure Assign <*> mlbs v_1358 l0
                                                                      <*> pure v_1359
                                                                      <*> pure v_1360)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsAssign db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1361, v_1362) ->
                                                    Seq v_1361 v_1362 : rest)
                                                 []
                                                 (S.singleton (v_1338, v_1339)))]
        PhiFact f@(Phi v_1363) -> Q.unions
                                    [q_1337,
                                     foldl'
                                       (\ q_1364 f@(Phi l0) ->
                                          trace ("got: " ++ show f)
                                            (Q.unions
                                               [q_1364,
                                                foldl'
                                                  (\ q_1366 f@(Seq l0_1365 lAfter0) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_1366,
                                                           foldl'
                                                             (\ q_1368
                                                                f@(StateBefore l0_1365_1367 st0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_1368,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (PhiStepCont l0_1365_1367
                                                                              st0
                                                                              lAfter0))]))
                                                             Q.empty
                                                             (M.foldlWithKey'
                                                                (\ rest v_1369 vals ->
                                                                   concatMap
                                                                     (\ v_1370 ->
                                                                        pure StateBefore <*>
                                                                          mlbs v_1369 l0_1365
                                                                          <*> pure v_1370)
                                                                     vals
                                                                     ++ rest)
                                                                []
                                                                (factsStateBefore db))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (v_1371, v_1372) ->
                                                        (pure Seq <*> mlbs v_1371 l0 <*>
                                                           pure v_1372)
                                                          ++ rest)
                                                     []
                                                     (factsSeq db)),
                                                Q.singleton (traceConclusion (PhiInitCont l0))]))
                                       Q.empty
                                       (S.foldl' (\ rest v_1373 -> Phi v_1373 : rest) []
                                          (S.singleton v_1363))]
        CondFact f@(Cond v_1374 v_1375 v_1376 v_1377) -> Q.unions
                                                           [q_1337,
                                                            foldl'
                                                              (\ q_1378 f@(Cond lcond0 e0 jlt0 jlf0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1378,
                                                                       foldl'
                                                                         (\ q_1380
                                                                            f@(StateBefore jlf0_1379
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1380,
                                                                                  foldl'
                                                                                    (\ q_1382
                                                                                       f@(StateBefore
                                                                                            lcond0_1381
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1382,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     lcond0_1381
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_1379
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1383
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1384
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1383
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1384)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1385 vals ->
                                                                               concatMap
                                                                                 (\ v_1386 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1385
                                                                                        jlf0
                                                                                      <*>
                                                                                      pure v_1386)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1388
                                                                            f@(StateBefore jlt0_1387
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1388,
                                                                                  foldl'
                                                                                    (\ q_1390
                                                                                       f@(StateBefore
                                                                                            lcond0_1389
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1390,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     lcond0_1389
                                                                                                     stc1
                                                                                                     e0
                                                                                                     jlt0_1387
                                                                                                     jlf0
                                                                                                     stt0))]))
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
                                                                                                   lcond0
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
                                                                                 (\ v_1394 ->
                                                                                    pure StateBefore
                                                                                      <*>
                                                                                      mlbs v_1393
                                                                                        jlt0
                                                                                      <*>
                                                                                      pure v_1394)
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
                                                                 (\ rest v_1395 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1396, v_1397, v_1398) ->
                                                                         Cond v_1395 v_1396 v_1397
                                                                           v_1398
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1374
                                                                    (S.singleton
                                                                       (v_1375, v_1376, v_1377))))]
        AssignFact f@(Assign v_1399 v_1400 v_1401) -> Q.unions
                                                        [q_1337,
                                                         foldl'
                                                           (\ q_1402 f@(Assign lAssign0 x0 e2) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1402,
                                                                    foldl'
                                                                      (\ q_1403
                                                                         f@(StateBefore lAfter1
                                                                              stAfter0)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1403,
                                                                               foldl'
                                                                                 (\ q_1405
                                                                                    f@(StateBefore
                                                                                         lAssign0_1404
                                                                                         stAssign0)
                                                                                    ->
                                                                                    trace
                                                                                      ("got: " ++
                                                                                         show f)
                                                                                      (Q.unions
                                                                                         [q_1405,
                                                                                          foldl'
                                                                                            (\ q_1408
                                                                                               f@(Seq
                                                                                                    lAssign0_1404_1406
                                                                                                    lAfter1_1407)
                                                                                               ->
                                                                                               trace
                                                                                                 ("got: "
                                                                                                    ++
                                                                                                    show
                                                                                                      f)
                                                                                                 (Q.unions
                                                                                                    [q_1408,
                                                                                                     Q.singleton
                                                                                                       (traceConclusion
                                                                                                          (AssignStepCont
                                                                                                             lAssign0_1404_1406
                                                                                                             x0
                                                                                                             e2
                                                                                                             stAssign0
                                                                                                             lAfter1_1407
                                                                                                             stAfter0))]))
                                                                                            Q.empty
                                                                                            (foldl'
                                                                                               (\ rest
                                                                                                  (v_1409,
                                                                                                   v_1410)
                                                                                                  ->
                                                                                                  (pure
                                                                                                     Seq
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1409
                                                                                                       lAssign0_1404
                                                                                                     <*>
                                                                                                     mlbs
                                                                                                       v_1410
                                                                                                       lAfter1)
                                                                                                    ++
                                                                                                    rest)
                                                                                               []
                                                                                               (factsSeq
                                                                                                  db))]))
                                                                                 Q.empty
                                                                                 (M.foldlWithKey'
                                                                                    (\ rest v_1411
                                                                                       vals ->
                                                                                       concatMap
                                                                                         (\ v_1412
                                                                                            ->
                                                                                            pure
                                                                                              StateBefore
                                                                                              <*>
                                                                                              mlbs
                                                                                                v_1411
                                                                                                lAssign0
                                                                                              <*>
                                                                                              pure
                                                                                                v_1412)
                                                                                         vals
                                                                                         ++ rest)
                                                                                    []
                                                                                    (factsStateBefore
                                                                                       db))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1413 vals ->
                                                                            S.foldl'
                                                                              (\ acc v_1414 ->
                                                                                 StateBefore v_1413
                                                                                   v_1414
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
                                                              (\ rest (v_1415, v_1416) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1417 ->
                                                                      Assign v_1415 v_1416 v_1417 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1399, v_1400)
                                                                 (S.singleton v_1401)))]
        VarFact f@(Var v_1418 v_1419) -> Q.unions
                                           [q_1337,
                                            foldl'
                                              (\ q_1420 f@(Var l1 x1) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1420,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l1 x1))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1421, v_1422) ->
                                                    Var v_1421 v_1422 : rest)
                                                 []
                                                 (S.singleton (v_1418, v_1419)))]
        StateBeforeFact f@(StateBefore v_1423 v_1424) -> Q.unions
                                                           [q_1337,
                                                            foldl'
                                                              (\ q_1425 f@(StateBefore l0 st0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1425,
                                                                       foldl'
                                                                         (\ q_1427
                                                                            f@(Seq l0_1426 lAfter0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1427,
                                                                                  foldl'
                                                                                    (\ q_1429
                                                                                       f@(Phi
                                                                                            l0_1426_1428)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1429,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (PhiStepCont
                                                                                                     l0_1426_1428
                                                                                                     st0
                                                                                                     lAfter0))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          v_1430 ->
                                                                                          (pure Phi
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1430
                                                                                               l0_1426)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsPhi
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1431, v_1432)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  mlbs v_1431 l0
                                                                                  <*> pure v_1432)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       foldl'
                                                                         (\ q_1434
                                                                            f@(Cond lcond0 e0 jlt0
                                                                                 l0_1433)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1434,
                                                                                  foldl'
                                                                                    (\ q_1436
                                                                                       f@(StateBefore
                                                                                            lcond0_1435
                                                                                            stc0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1436,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     lcond0_1435
                                                                                                     stc0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     l0_1433
                                                                                                     st0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1437
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1438
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1437
                                                                                                   lcond0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1438)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1439 vals ->
                                                                               concatMap
                                                                                 (\ (v_1440, v_1441,
                                                                                     v_1442)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1439
                                                                                      <*>
                                                                                      pure v_1440
                                                                                      <*>
                                                                                      pure v_1441
                                                                                      <*>
                                                                                      mlbs v_1442
                                                                                        l0)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1443
                                                                            f@(StateBefore jlf0
                                                                                 stf0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1443,
                                                                                  foldl'
                                                                                    (\ q_1446
                                                                                       f@(Cond
                                                                                            l0_1444
                                                                                            e0 jlt0
                                                                                            jlf0_1445)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1446,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondFalseCont
                                                                                                     l0_1444
                                                                                                     st0
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0_1445
                                                                                                     stf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1447
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1448,
                                                                                                v_1449,
                                                                                                v_1450)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1447
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1448
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1449
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1450
                                                                                                   jlf0)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1451 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1452 ->
                                                                                    StateBefore
                                                                                      v_1451
                                                                                      v_1452
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1454
                                                                            f@(Cond lcond1 e1
                                                                                 l0_1453 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1454,
                                                                                  foldl'
                                                                                    (\ q_1456
                                                                                       f@(StateBefore
                                                                                            lcond1_1455
                                                                                            stc1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1456,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     lcond1_1455
                                                                                                     stc1
                                                                                                     e1
                                                                                                     l0_1453
                                                                                                     jlf1
                                                                                                     st0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1457
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1458
                                                                                               ->
                                                                                               pure
                                                                                                 StateBefore
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1457
                                                                                                   lcond1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1458)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateBefore
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1459 vals ->
                                                                               concatMap
                                                                                 (\ (v_1460, v_1461,
                                                                                     v_1462)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      pure v_1459
                                                                                      <*>
                                                                                      pure v_1460
                                                                                      <*>
                                                                                      mlbs v_1461 l0
                                                                                      <*>
                                                                                      pure v_1462)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db)),
                                                                       foldl'
                                                                         (\ q_1463
                                                                            f@(StateBefore jlt1
                                                                                 stt0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1463,
                                                                                  foldl'
                                                                                    (\ q_1466
                                                                                       f@(Cond
                                                                                            l0_1464
                                                                                            e1
                                                                                            jlt1_1465
                                                                                            jlf1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1466,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondTrueCont
                                                                                                     l0_1464
                                                                                                     st0
                                                                                                     e1
                                                                                                     jlt1_1465
                                                                                                     jlf1
                                                                                                     stt0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1467
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ (v_1468,
                                                                                                v_1469,
                                                                                                v_1470)
                                                                                               ->
                                                                                               pure
                                                                                                 Cond
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1467
                                                                                                   l0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1468
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1469
                                                                                                   jlt1
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1470)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsCond
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1471 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1472 ->
                                                                                    StateBefore
                                                                                      v_1471
                                                                                      v_1472
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1473
                                                                            f@(StateBefore lAssign0
                                                                                 stAssign0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1473,
                                                                                  foldl'
                                                                                    (\ q_1475
                                                                                       f@(Assign
                                                                                            lAssign0_1474
                                                                                            x0 e2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1475,
                                                                                             foldl'
                                                                                               (\ q_1478
                                                                                                  f@(Seq
                                                                                                       lAssign0_1474_1476
                                                                                                       l0_1477)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1478,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                lAssign0_1474_1476
                                                                                                                x0
                                                                                                                e2
                                                                                                                stAssign0
                                                                                                                l0_1477
                                                                                                                st0))]))
                                                                                               Q.empty
                                                                                               (foldl'
                                                                                                  (\ rest
                                                                                                     (v_1479,
                                                                                                      v_1480)
                                                                                                     ->
                                                                                                     (pure
                                                                                                        Seq
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1479
                                                                                                          lAssign0_1474
                                                                                                        <*>
                                                                                                        mlbs
                                                                                                          v_1480
                                                                                                          l0)
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsSeq
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          (v_1481,
                                                                                           v_1482)
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1483
                                                                                               ->
                                                                                               pure
                                                                                                 Assign
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1481
                                                                                                   lAssign0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1482
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1483)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsAssign
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1484 vals ->
                                                                               S.foldl'
                                                                                 (\ acc v_1485 ->
                                                                                    StateBefore
                                                                                      v_1484
                                                                                      v_1485
                                                                                      : acc)
                                                                                 []
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateBefore db)),
                                                                       foldl'
                                                                         (\ q_1487
                                                                            f@(Assign l0_1486 x0 e2)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1487,
                                                                                  foldl'
                                                                                    (\ q_1489
                                                                                       f@(Seq
                                                                                            l0_1486_1488
                                                                                            lAfter1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1489,
                                                                                             foldl'
                                                                                               (\ q_1491
                                                                                                  f@(StateBefore
                                                                                                       lAfter1_1490
                                                                                                       stAfter0)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1491,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (AssignStepCont
                                                                                                                l0_1486_1488
                                                                                                                x0
                                                                                                                e2
                                                                                                                st0
                                                                                                                lAfter1_1490
                                                                                                                stAfter0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     v_1492
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1493
                                                                                                          ->
                                                                                                          pure
                                                                                                            StateBefore
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1492
                                                                                                              lAfter1
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1493)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsStateBefore
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (foldl'
                                                                                       (\ rest
                                                                                          (v_1494,
                                                                                           v_1495)
                                                                                          ->
                                                                                          (pure Seq
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_1494
                                                                                               l0_1486
                                                                                             <*>
                                                                                             pure
                                                                                               v_1495)
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsSeq
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest (v_1496, v_1497)
                                                                               vals ->
                                                                               concatMap
                                                                                 (\ v_1498 ->
                                                                                    pure Assign <*>
                                                                                      mlbs v_1496 l0
                                                                                      <*>
                                                                                      pure v_1497
                                                                                      <*>
                                                                                      pure v_1498)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsAssign db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1499 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1500 ->
                                                                         StateBefore v_1499 v_1500 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1423
                                                                    (S.singleton v_1424)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_1502 vals ->
         S.foldl' (\ acc v_1503 -> StateBefore v_1502 v_1503 : acc) [] vals
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