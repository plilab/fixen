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
  = [StateAfterFact (StateAfter jlf0 (join st0 stf0))]
evaluate db (AssignInitCont l4 x1 e5)
  = [StateAfterFact (StateAfter l4 (empty 0))]
evaluate db (AssignStepCont l2 st3 x0 e3)
  = [StateAfterFact (StateAfter l2 (insert x0 (eval e3 st3) st3))]
evaluate db (CondInitCont l3 e4 jlt3 jlf3)
  = [StateAfterFact (StateAfter l3 (empty 0))]
evaluate db (EvalCondCont l00 lcond0 st2 e2 jlt2 jlf2)
  = [CondValFact (CondVal lcond0 (evaluateConditional e2 st2))]
evaluate db (SeqStepCont l10 l20 st10 st20)
  = [StateAfterFact (StateAfter l20 (join st20 st10))]
evaluate db (CondStepTrueCont l1 st1 jlt1 stt0 e1 jlf1)
  = [StateAfterFact (StateAfter jlt1 (join st1 stt0))]
evaluate db (VarInitCont l5 x2)
  = [StateAfterFact (StateAfter l5 (singleton x2 Bot))]

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
step db fact q_1279
  = case fact of
        SeqFact f@(Seq v_1280 v_1281) -> Q.unions
                                           [q_1279,
                                            foldl'
                                              (\ q_1282 f@(Seq l10 l20) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1282,
                                                       foldl'
                                                         (\ q_1284 f@(StateAfter l10_1283 st10) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1284,
                                                                  foldl'
                                                                    (\ q_1286
                                                                       f@(StateAfter l20_1285 st20)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1286,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (SeqStepCont
                                                                                     l10_1283
                                                                                     l20_1285
                                                                                     st10
                                                                                     st20))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1287 vals ->
                                                                          concatMap
                                                                            (\ v_1288 ->
                                                                               pure StateAfter <*>
                                                                                 mlbs v_1287 l20
                                                                                 <*> pure v_1288)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateAfter db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_1289 vals ->
                                                               concatMap
                                                                 (\ v_1290 ->
                                                                    pure StateAfter <*>
                                                                      mlbs v_1289 l10
                                                                      <*> pure v_1290)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsStateAfter db)),
                                                       foldl'
                                                         (\ q_1292 f@(Cond l20_1291 e2 jlt2 jlf2) ->
                                                            trace ("got: " ++ show f)
                                                              (Q.unions
                                                                 [q_1292,
                                                                  foldl'
                                                                    (\ q_1294
                                                                       f@(StateAfter l10_1293 st2)
                                                                       ->
                                                                       trace ("got: " ++ show f)
                                                                         (Q.unions
                                                                            [q_1294,
                                                                             Q.singleton
                                                                               (traceConclusion
                                                                                  (EvalCondCont
                                                                                     l10_1293
                                                                                     l20_1291
                                                                                     st2
                                                                                     e2
                                                                                     jlt2
                                                                                     jlf2))]))
                                                                    Q.empty
                                                                    (M.foldlWithKey'
                                                                       (\ rest v_1295 vals ->
                                                                          concatMap
                                                                            (\ v_1296 ->
                                                                               pure StateAfter <*>
                                                                                 mlbs v_1295 l10
                                                                                 <*> pure v_1296)
                                                                            vals
                                                                            ++ rest)
                                                                       []
                                                                       (factsStateAfter db))]))
                                                         Q.empty
                                                         (M.foldlWithKey'
                                                            (\ rest v_1297 vals ->
                                                               concatMap
                                                                 (\ (v_1298, v_1299, v_1300) ->
                                                                    pure Cond <*> mlbs v_1297 l20
                                                                      <*> pure v_1298
                                                                      <*> pure v_1299
                                                                      <*> pure v_1300)
                                                                 vals
                                                                 ++ rest)
                                                            []
                                                            (factsCond db))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1301, v_1302) ->
                                                    Seq v_1301 v_1302 : rest)
                                                 []
                                                 (S.singleton (v_1280, v_1281)))]
        CondValFact f@(CondVal v_1303 v_1304) -> Q.unions
                                                   [q_1279,
                                                    foldl'
                                                      (\ q_1305 f@(CondVal l1 _) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_1305,
                                                               foldl'
                                                                 (\ q_1306 f@(StateAfter jlt1 stt0)
                                                                    ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_1306,
                                                                          foldl'
                                                                            (\ q_1308
                                                                               f@(StateAfter l1_1307
                                                                                    st1)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_1308,
                                                                                     foldl'
                                                                                       (\ q_1311
                                                                                          f@(Cond
                                                                                               l1_1307_1309
                                                                                               e1
                                                                                               jlt1_1310
                                                                                               jlf1)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_1311,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (CondStepTrueCont
                                                                                                        l1_1307_1309
                                                                                                        st1
                                                                                                        jlt1_1310
                                                                                                        stt0
                                                                                                        e1
                                                                                                        jlf1))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             v_1312
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ (v_1313,
                                                                                                   v_1314,
                                                                                                   v_1315)
                                                                                                  ->
                                                                                                  pure
                                                                                                    Cond
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_1312
                                                                                                      l1_1307
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_1313
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_1314
                                                                                                      jlt1
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_1315)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsCond
                                                                                             db))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_1316 vals
                                                                                  ->
                                                                                  concatMap
                                                                                    (\ v_1317 ->
                                                                                       pure
                                                                                         StateAfter
                                                                                         <*>
                                                                                         mlbs v_1316
                                                                                           l1
                                                                                         <*>
                                                                                         pure
                                                                                           v_1317)
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateAfter
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_1318 vals ->
                                                                       S.foldl'
                                                                         (\ acc v_1319 ->
                                                                            StateAfter v_1318 v_1319
                                                                              : acc)
                                                                         []
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateAfter db))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_1320 vals ->
                                                            concatMap
                                                              (\ v_1321 ->
                                                                 pure CondVal <*> pure v_1320 <*>
                                                                   mlbs v_1321 BTrue)
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_1303 (S.singleton v_1304))),
                                                    foldl'
                                                      (\ q_1322 f@(CondVal l0 _) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_1322,
                                                               foldl'
                                                                 (\ q_1324
                                                                    f@(StateAfter l0_1323 st0) ->
                                                                    trace ("got: " ++ show f)
                                                                      (Q.unions
                                                                         [q_1324,
                                                                          foldl'
                                                                            (\ q_1325
                                                                               f@(StateAfter jlf0
                                                                                    stf0)
                                                                               ->
                                                                               trace
                                                                                 ("got: " ++ show f)
                                                                                 (Q.unions
                                                                                    [q_1325,
                                                                                     foldl'
                                                                                       (\ q_1328
                                                                                          f@(Cond
                                                                                               l0_1323_1326
                                                                                               e0
                                                                                               jlt0
                                                                                               jlf0_1327)
                                                                                          ->
                                                                                          trace
                                                                                            ("got: "
                                                                                               ++
                                                                                               show
                                                                                                 f)
                                                                                            (Q.unions
                                                                                               [q_1328,
                                                                                                Q.singleton
                                                                                                  (traceConclusion
                                                                                                     (CondStepFalseCont
                                                                                                        l0_1323_1326
                                                                                                        st0
                                                                                                        jlf0_1327
                                                                                                        stf0
                                                                                                        e0
                                                                                                        jlt0))]))
                                                                                       Q.empty
                                                                                       (M.foldlWithKey'
                                                                                          (\ rest
                                                                                             v_1329
                                                                                             vals ->
                                                                                             concatMap
                                                                                               (\ (v_1330,
                                                                                                   v_1331,
                                                                                                   v_1332)
                                                                                                  ->
                                                                                                  pure
                                                                                                    Cond
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_1329
                                                                                                      l0_1323
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_1330
                                                                                                    <*>
                                                                                                    pure
                                                                                                      v_1331
                                                                                                    <*>
                                                                                                    mlbs
                                                                                                      v_1332
                                                                                                      jlf0)
                                                                                               vals
                                                                                               ++
                                                                                               rest)
                                                                                          []
                                                                                          (factsCond
                                                                                             db))]))
                                                                            Q.empty
                                                                            (M.foldlWithKey'
                                                                               (\ rest v_1333 vals
                                                                                  ->
                                                                                  S.foldl'
                                                                                    (\ acc v_1334 ->
                                                                                       StateAfter
                                                                                         v_1333
                                                                                         v_1334
                                                                                         : acc)
                                                                                    []
                                                                                    vals
                                                                                    ++ rest)
                                                                               []
                                                                               (factsStateAfter
                                                                                  db))]))
                                                                 Q.empty
                                                                 (M.foldlWithKey'
                                                                    (\ rest v_1335 vals ->
                                                                       concatMap
                                                                         (\ v_1336 ->
                                                                            pure StateAfter <*>
                                                                              mlbs v_1335 l0
                                                                              <*> pure v_1336)
                                                                         vals
                                                                         ++ rest)
                                                                    []
                                                                    (factsStateAfter db))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_1337 vals ->
                                                            concatMap
                                                              (\ v_1338 ->
                                                                 pure CondVal <*> pure v_1337 <*>
                                                                   mlbs v_1338 BFalse)
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (M.singleton v_1303 (S.singleton v_1304)))]
        CondFact f@(Cond v_1339 v_1340 v_1341 v_1342) -> Q.unions
                                                           [q_1279,
                                                            foldl'
                                                              (\ q_1343 f@(Cond l0 e0 jlt0 jlf0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1343,
                                                                       foldl'
                                                                         (\ q_1345
                                                                            f@(StateAfter l0_1344
                                                                                 st0)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1345,
                                                                                  foldl'
                                                                                    (\ q_1347
                                                                                       f@(StateAfter
                                                                                            jlf0_1346
                                                                                            stf0)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1347,
                                                                                             foldl'
                                                                                               (\ q_1349
                                                                                                  f@(CondVal
                                                                                                       l0_1344_1348
                                                                                                       _)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1349,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (CondStepFalseCont
                                                                                                                l0_1344_1348
                                                                                                                st0
                                                                                                                jlf0_1346
                                                                                                                stf0
                                                                                                                e0
                                                                                                                jlt0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     v_1350
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1351
                                                                                                          ->
                                                                                                          pure
                                                                                                            CondVal
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1350
                                                                                                              l0_1344
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1351
                                                                                                              BFalse)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsCondVal
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1352
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1353
                                                                                               ->
                                                                                               pure
                                                                                                 StateAfter
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1352
                                                                                                   jlf0
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1353)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateAfter
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1354 vals ->
                                                                               concatMap
                                                                                 (\ v_1355 ->
                                                                                    pure StateAfter
                                                                                      <*>
                                                                                      mlbs v_1354 l0
                                                                                      <*>
                                                                                      pure v_1355)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateAfter db)),
                                                                       foldl'
                                                                         (\ q_1357
                                                                            f@(CondVal l0_1356 _) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1357,
                                                                                  foldl'
                                                                                    (\ q_1359
                                                                                       f@(StateAfter
                                                                                            l0_1356_1358
                                                                                            st1)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1359,
                                                                                             foldl'
                                                                                               (\ q_1361
                                                                                                  f@(StateAfter
                                                                                                       jlt0_1360
                                                                                                       stt0)
                                                                                                  ->
                                                                                                  trace
                                                                                                    ("got: "
                                                                                                       ++
                                                                                                       show
                                                                                                         f)
                                                                                                    (Q.unions
                                                                                                       [q_1361,
                                                                                                        Q.singleton
                                                                                                          (traceConclusion
                                                                                                             (CondStepTrueCont
                                                                                                                l0_1356_1358
                                                                                                                st1
                                                                                                                jlt0_1360
                                                                                                                stt0
                                                                                                                e0
                                                                                                                jlf0))]))
                                                                                               Q.empty
                                                                                               (M.foldlWithKey'
                                                                                                  (\ rest
                                                                                                     v_1362
                                                                                                     vals
                                                                                                     ->
                                                                                                     concatMap
                                                                                                       (\ v_1363
                                                                                                          ->
                                                                                                          pure
                                                                                                            StateAfter
                                                                                                            <*>
                                                                                                            mlbs
                                                                                                              v_1362
                                                                                                              jlt0
                                                                                                            <*>
                                                                                                            pure
                                                                                                              v_1363)
                                                                                                       vals
                                                                                                       ++
                                                                                                       rest)
                                                                                                  []
                                                                                                  (factsStateAfter
                                                                                                     db))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1364
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1365
                                                                                               ->
                                                                                               pure
                                                                                                 StateAfter
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1364
                                                                                                   l0_1356
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1365)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateAfter
                                                                                          db))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1366 vals ->
                                                                               concatMap
                                                                                 (\ v_1367 ->
                                                                                    pure CondVal <*>
                                                                                      mlbs v_1366 l0
                                                                                      <*>
                                                                                      mlbs v_1367
                                                                                        BTrue)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCondVal db)),
                                                                       foldl'
                                                                         (\ q_1369
                                                                            f@(Seq l00 l0_1368) ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1369,
                                                                                  foldl'
                                                                                    (\ q_1371
                                                                                       f@(StateAfter
                                                                                            l00_1370
                                                                                            st2)
                                                                                       ->
                                                                                       trace
                                                                                         ("got: " ++
                                                                                            show f)
                                                                                         (Q.unions
                                                                                            [q_1371,
                                                                                             Q.singleton
                                                                                               (traceConclusion
                                                                                                  (EvalCondCont
                                                                                                     l00_1370
                                                                                                     l0_1368
                                                                                                     st2
                                                                                                     e0
                                                                                                     jlt0
                                                                                                     jlf0))]))
                                                                                    Q.empty
                                                                                    (M.foldlWithKey'
                                                                                       (\ rest
                                                                                          v_1372
                                                                                          vals ->
                                                                                          concatMap
                                                                                            (\ v_1373
                                                                                               ->
                                                                                               pure
                                                                                                 StateAfter
                                                                                                 <*>
                                                                                                 mlbs
                                                                                                   v_1372
                                                                                                   l00
                                                                                                 <*>
                                                                                                 pure
                                                                                                   v_1373)
                                                                                            vals
                                                                                            ++ rest)
                                                                                       []
                                                                                       (factsStateAfter
                                                                                          db))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1374, v_1375)
                                                                               ->
                                                                               (pure Seq <*>
                                                                                  pure v_1374
                                                                                  <*>
                                                                                  mlbs v_1375 l0)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsSeq db)),
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (CondInitCont l0 e0 jlt0
                                                                               jlf0))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1376 vals ->
                                                                    S.foldl'
                                                                      (\ acc
                                                                         (v_1377, v_1378, v_1379) ->
                                                                         Cond v_1376 v_1377 v_1378
                                                                           v_1379
                                                                           : acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (M.singleton v_1339
                                                                    (S.singleton
                                                                       (v_1340, v_1341, v_1342))))]
        AssignFact f@(Assign v_1380 v_1381 v_1382) -> Q.unions
                                                        [q_1279,
                                                         foldl'
                                                           (\ q_1383 f@(Assign l2 x0 e3) ->
                                                              trace ("got: " ++ show f)
                                                                (Q.unions
                                                                   [q_1383,
                                                                    foldl'
                                                                      (\ q_1385
                                                                         f@(StateAfter l2_1384 st3)
                                                                         ->
                                                                         trace ("got: " ++ show f)
                                                                           (Q.unions
                                                                              [q_1385,
                                                                               Q.singleton
                                                                                 (traceConclusion
                                                                                    (AssignStepCont
                                                                                       l2_1384
                                                                                       st3
                                                                                       x0
                                                                                       e3))]))
                                                                      Q.empty
                                                                      (M.foldlWithKey'
                                                                         (\ rest v_1386 vals ->
                                                                            concatMap
                                                                              (\ v_1387 ->
                                                                                 pure StateAfter <*>
                                                                                   mlbs v_1386 l2
                                                                                   <*> pure v_1387)
                                                                              vals
                                                                              ++ rest)
                                                                         []
                                                                         (factsStateAfter db)),
                                                                    Q.singleton
                                                                      (traceConclusion
                                                                         (AssignInitCont l2 x0
                                                                            e3))]))
                                                           Q.empty
                                                           (M.foldlWithKey'
                                                              (\ rest (v_1388, v_1389) vals ->
                                                                 S.foldl'
                                                                   (\ acc v_1390 ->
                                                                      Assign v_1388 v_1389 v_1390 :
                                                                        acc)
                                                                   []
                                                                   vals
                                                                   ++ rest)
                                                              []
                                                              (M.singleton (v_1380, v_1381)
                                                                 (S.singleton v_1382)))]
        VarFact f@(Var v_1391 v_1392) -> Q.unions
                                           [q_1279,
                                            foldl'
                                              (\ q_1393 f@(Var l5 x2) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_1393,
                                                       Q.singleton
                                                         (traceConclusion (VarInitCont l5 x2))]))
                                              Q.empty
                                              (S.foldl'
                                                 (\ rest (v_1394, v_1395) ->
                                                    Var v_1394 v_1395 : rest)
                                                 []
                                                 (S.singleton (v_1391, v_1392)))]
        StateAfterFact f@(StateAfter v_1396 v_1397) -> Q.unions
                                                         [q_1279,
                                                          foldl'
                                                            (\ q_1398 f@(StateAfter l10 st10) ->
                                                               trace ("got: " ++ show f)
                                                                 (Q.unions
                                                                    [q_1398,
                                                                     foldl'
                                                                       (\ q_1400
                                                                          f@(Seq l10_1399 l20) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1400,
                                                                                foldl'
                                                                                  (\ q_1402
                                                                                     f@(StateAfter
                                                                                          l20_1401
                                                                                          st20)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1402,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (SeqStepCont
                                                                                                   l10_1399
                                                                                                   l20_1401
                                                                                                   st10
                                                                                                   st20))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1403
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_1404
                                                                                             ->
                                                                                             pure
                                                                                               StateAfter
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1403
                                                                                                 l20
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1404)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateAfter
                                                                                        db))]))
                                                                       Q.empty
                                                                       (foldl'
                                                                          (\ rest (v_1405, v_1406)
                                                                             ->
                                                                             (pure Seq <*>
                                                                                mlbs v_1405 l10
                                                                                <*> pure v_1406)
                                                                               ++ rest)
                                                                          []
                                                                          (factsSeq db)),
                                                                     foldl'
                                                                       (\ q_1408
                                                                          f@(Seq l20 l10_1407) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1408,
                                                                                foldl'
                                                                                  (\ q_1410
                                                                                     f@(StateAfter
                                                                                          l20_1409
                                                                                          st20)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1410,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (SeqStepCont
                                                                                                   l20_1409
                                                                                                   l10_1407
                                                                                                   st20
                                                                                                   st10))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1411
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_1412
                                                                                             ->
                                                                                             pure
                                                                                               StateAfter
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1411
                                                                                                 l20
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1412)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateAfter
                                                                                        db))]))
                                                                       Q.empty
                                                                       (foldl'
                                                                          (\ rest (v_1413, v_1414)
                                                                             ->
                                                                             (pure Seq <*>
                                                                                pure v_1413
                                                                                <*> mlbs v_1414 l10)
                                                                               ++ rest)
                                                                          []
                                                                          (factsSeq db)),
                                                                     foldl'
                                                                       (\ q_1415
                                                                          f@(StateAfter jlf0 stf0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1415,
                                                                                foldl'
                                                                                  (\ q_1418
                                                                                     f@(Cond
                                                                                          l10_1416
                                                                                          e0 jlt0
                                                                                          jlf0_1417)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1418,
                                                                                           foldl'
                                                                                             (\ q_1420
                                                                                                f@(CondVal
                                                                                                     l10_1416_1419
                                                                                                     _)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_1420,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (CondStepFalseCont
                                                                                                              l10_1416_1419
                                                                                                              st10
                                                                                                              jlf0_1417
                                                                                                              stf0
                                                                                                              e0
                                                                                                              jlt0))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   v_1421
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ v_1422
                                                                                                        ->
                                                                                                        pure
                                                                                                          CondVal
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1421
                                                                                                            l10_1416
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1422
                                                                                                            BFalse)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsCondVal
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1423
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_1424,
                                                                                              v_1425,
                                                                                              v_1426)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1423
                                                                                                 l10
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1424
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1425
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1426
                                                                                                 jlf0)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_1427 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_1428 ->
                                                                                  StateAfter v_1427
                                                                                    v_1428
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateAfter db)),
                                                                     foldl'
                                                                       (\ q_1430
                                                                          f@(Cond l0 e0 jlt0
                                                                               l10_1429)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1430,
                                                                                foldl'
                                                                                  (\ q_1432
                                                                                     f@(StateAfter
                                                                                          l0_1431
                                                                                          st0)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1432,
                                                                                           foldl'
                                                                                             (\ q_1434
                                                                                                f@(CondVal
                                                                                                     l0_1431_1433
                                                                                                     _)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_1434,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (CondStepFalseCont
                                                                                                              l0_1431_1433
                                                                                                              st0
                                                                                                              l10_1429
                                                                                                              st10
                                                                                                              e0
                                                                                                              jlt0))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   v_1435
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ v_1436
                                                                                                        ->
                                                                                                        pure
                                                                                                          CondVal
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1435
                                                                                                            l0_1431
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1436
                                                                                                            BFalse)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsCondVal
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1437
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_1438
                                                                                             ->
                                                                                             pure
                                                                                               StateAfter
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1437
                                                                                                 l0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1438)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateAfter
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
                                                                                    <*> pure v_1440
                                                                                    <*> pure v_1441
                                                                                    <*>
                                                                                    mlbs v_1442 l10)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_1443 f@(CondVal l1 _) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1443,
                                                                                foldl'
                                                                                  (\ q_1446
                                                                                     f@(Cond l1_1444
                                                                                          e1
                                                                                          l10_1445
                                                                                          jlf1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1446,
                                                                                           foldl'
                                                                                             (\ q_1448
                                                                                                f@(StateAfter
                                                                                                     l1_1444_1447
                                                                                                     st1)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_1448,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (CondStepTrueCont
                                                                                                              l1_1444_1447
                                                                                                              st1
                                                                                                              l10_1445
                                                                                                              st10
                                                                                                              e1
                                                                                                              jlf1))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   v_1449
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ v_1450
                                                                                                        ->
                                                                                                        pure
                                                                                                          StateAfter
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1449
                                                                                                            l1_1444
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_1450)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsStateAfter
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1451
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_1452,
                                                                                              v_1453,
                                                                                              v_1454)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1451
                                                                                                 l1
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1452
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1453
                                                                                                 l10
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1454)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_1455 vals ->
                                                                             concatMap
                                                                               (\ v_1456 ->
                                                                                  pure CondVal <*>
                                                                                    pure v_1455
                                                                                    <*>
                                                                                    mlbs v_1456
                                                                                      BTrue)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCondVal db)),
                                                                     foldl'
                                                                       (\ q_1458
                                                                          f@(CondVal l10_1457 _) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1458,
                                                                                foldl'
                                                                                  (\ q_1459
                                                                                     f@(StateAfter
                                                                                          jlt1 stt0)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1459,
                                                                                           foldl'
                                                                                             (\ q_1462
                                                                                                f@(Cond
                                                                                                     l10_1457_1460
                                                                                                     e1
                                                                                                     jlt1_1461
                                                                                                     jlf1)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_1462,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (CondStepTrueCont
                                                                                                              l10_1457_1460
                                                                                                              st10
                                                                                                              jlt1_1461
                                                                                                              stt0
                                                                                                              e1
                                                                                                              jlf1))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   v_1463
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ (v_1464,
                                                                                                         v_1465,
                                                                                                         v_1466)
                                                                                                        ->
                                                                                                        pure
                                                                                                          Cond
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1463
                                                                                                            l10_1457
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_1464
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_1465
                                                                                                            jlt1
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_1466)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsCond
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1467
                                                                                        vals ->
                                                                                        S.foldl'
                                                                                          (\ acc
                                                                                             v_1468
                                                                                             ->
                                                                                             StateAfter
                                                                                               v_1467
                                                                                               v_1468
                                                                                               :
                                                                                               acc)
                                                                                          []
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateAfter
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_1469 vals ->
                                                                             concatMap
                                                                               (\ v_1470 ->
                                                                                  pure CondVal <*>
                                                                                    mlbs v_1469 l10
                                                                                    <*>
                                                                                    mlbs v_1470
                                                                                      BTrue)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCondVal db)),
                                                                     foldl'
                                                                       (\ q_1472
                                                                          f@(Seq l10_1471 lcond0) ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1472,
                                                                                foldl'
                                                                                  (\ q_1474
                                                                                     f@(Cond
                                                                                          lcond0_1473
                                                                                          e2 jlt2
                                                                                          jlf2)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_1474,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondCont
                                                                                                   l10_1471
                                                                                                   lcond0_1473
                                                                                                   st10
                                                                                                   e2
                                                                                                   jlt2
                                                                                                   jlf2))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_1475
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_1476,
                                                                                              v_1477,
                                                                                              v_1478)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_1475
                                                                                                 lcond0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1476
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1477
                                                                                               <*>
                                                                                               pure
                                                                                                 v_1478)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (foldl'
                                                                          (\ rest (v_1479, v_1480)
                                                                             ->
                                                                             (pure Seq <*>
                                                                                mlbs v_1479 l10
                                                                                <*> pure v_1480)
                                                                               ++ rest)
                                                                          []
                                                                          (factsSeq db)),
                                                                     foldl'
                                                                       (\ q_1482
                                                                          f@(Assign l10_1481 x0 e3)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_1482,
                                                                                Q.singleton
                                                                                  (traceConclusion
                                                                                     (AssignStepCont
                                                                                        l10_1481
                                                                                        st10
                                                                                        x0
                                                                                        e3))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest (v_1483, v_1484)
                                                                             vals ->
                                                                             concatMap
                                                                               (\ v_1485 ->
                                                                                  pure Assign <*>
                                                                                    mlbs v_1483 l10
                                                                                    <*> pure v_1484
                                                                                    <*> pure v_1485)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsAssign db))]))
                                                            Q.empty
                                                            (M.foldlWithKey'
                                                               (\ rest v_1486 vals ->
                                                                  S.foldl'
                                                                    (\ acc v_1487 ->
                                                                       StateAfter v_1486 v_1487 :
                                                                         acc)
                                                                    []
                                                                    vals
                                                                    ++ rest)
                                                               []
                                                               (M.singleton v_1396
                                                                  (S.singleton v_1397)))]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_1488 db
  = M.foldlWithKey'
      (\ rest v_1490 vals ->
         concatMap
           (\ v_1491 ->
              pure StateAfter <*> mlbs v_1490 v_1488 <*> pure v_1491)
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