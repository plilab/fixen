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

data CondVal = CondVal Natural Bool
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
  = [StateAfterFact (StateAfter l20 (join st10 st20))]
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
                         factsCondVal :: S.HashSet (Natural, Bool),
                         factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr),
                         factsStateAfter :: M.HashMap Natural (S.HashSet State)}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase S.empty S.empty M.empty S.empty M.empty M.empty

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
          CondValFact (CondVal v0 v1) -> if
                                           S.member (v0, v1) (factsCondVal db) then (db, False) else
                                           (db{factsCondVal = S.insert (v0, v1) (factsCondVal db)},
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
step db fact q_936
  = case fact of
        SeqFact f@v_937 -> Q.unions
                             [q_936,
                              foldl'
                                (\ q_938 f@(Seq l10 l20) ->
                                   trace ("got: " ++ show f)
                                     (Q.unions
                                        [q_938,
                                         foldl'
                                           (\ q_940 f@(StateAfter l10_939 st10) ->
                                              trace ("got: " ++ show f)
                                                (Q.unions
                                                   [q_940,
                                                    foldl'
                                                      (\ q_942 f@(StateAfter l20_941 st20) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_942,
                                                               Q.singleton
                                                                 (traceConclusion
                                                                    (SeqStepCont l10_939 l20_941
                                                                       st10
                                                                       st20))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_943 vals ->
                                                            concatMap
                                                              (\ v_944 ->
                                                                 pure StateAfter <*> mlbs v_943 l20
                                                                   <*> pure v_944)
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (factsStateAfter db))]))
                                           Q.empty
                                           (M.foldlWithKey'
                                              (\ rest v_945 vals ->
                                                 concatMap
                                                   (\ v_946 ->
                                                      pure StateAfter <*> mlbs v_945 l10 <*>
                                                        pure v_946)
                                                   vals
                                                   ++ rest)
                                              []
                                              (factsStateAfter db)),
                                         foldl'
                                           (\ q_948 f@(Cond l20_947 e2 jlt2 jlf2) ->
                                              trace ("got: " ++ show f)
                                                (Q.unions
                                                   [q_948,
                                                    foldl'
                                                      (\ q_950 f@(StateAfter l10_949 st2) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_950,
                                                               Q.singleton
                                                                 (traceConclusion
                                                                    (EvalCondCont l10_949 l20_947
                                                                       st2
                                                                       e2
                                                                       jlt2
                                                                       jlf2))]))
                                                      Q.empty
                                                      (M.foldlWithKey'
                                                         (\ rest v_951 vals ->
                                                            concatMap
                                                              (\ v_952 ->
                                                                 pure StateAfter <*> mlbs v_951 l10
                                                                   <*> pure v_952)
                                                              vals
                                                              ++ rest)
                                                         []
                                                         (factsStateAfter db))]))
                                           Q.empty
                                           (M.foldlWithKey'
                                              (\ rest v_953 vals ->
                                                 concatMap
                                                   (\ (v_954, v_955, v_956) ->
                                                      pure Cond <*> mlbs v_953 l20 <*> pure v_954
                                                        <*> pure v_955
                                                        <*> pure v_956)
                                                   vals
                                                   ++ rest)
                                              []
                                              (factsCond db))]))
                                Q.empty
                                ([v_937])]
        CondValFact f@v_957 -> Q.unions
                                 [q_936,
                                  foldl'
                                    (\ q_958 f@(CondVal l1 _) ->
                                       trace ("got: " ++ show f)
                                         (Q.unions
                                            [q_958,
                                             foldl'
                                               (\ q_959 f@(StateAfter jlt1 stt0) ->
                                                  trace ("got: " ++ show f)
                                                    (Q.unions
                                                       [q_959,
                                                        foldl'
                                                          (\ q_961 f@(StateAfter l1_960 st1) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_961,
                                                                   foldl'
                                                                     (\ q_964
                                                                        f@(Cond l1_960_962 e1
                                                                             jlt1_963 jlf1)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_964,
                                                                              Q.singleton
                                                                                (traceConclusion
                                                                                   (CondStepTrueCont
                                                                                      l1_960_962
                                                                                      st1
                                                                                      jlt1_963
                                                                                      stt0
                                                                                      e1
                                                                                      jlf1))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_965 vals ->
                                                                           concatMap
                                                                             (\ (v_966, v_967,
                                                                                 v_968)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  mlbs v_965 l1_960
                                                                                  <*> pure v_966
                                                                                  <*>
                                                                                  mlbs v_967 jlt1
                                                                                  <*> pure v_968)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_969 vals ->
                                                                concatMap
                                                                  (\ v_970 ->
                                                                     pure StateAfter <*>
                                                                       mlbs v_969 l1
                                                                       <*> pure v_970)
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (factsStateAfter db))]))
                                               Q.empty
                                               (M.foldlWithKey'
                                                  (\ rest v_971 vals ->
                                                     S.foldl'
                                                       (\ acc v_972 -> StateAfter v_971 v_972 : acc)
                                                       []
                                                       vals
                                                       ++ rest)
                                                  []
                                                  (factsStateAfter db))]))
                                    Q.empty
                                    ([v_957]),
                                  foldl'
                                    (\ q_973 f@(CondVal l0 _) ->
                                       trace ("got: " ++ show f)
                                         (Q.unions
                                            [q_973,
                                             foldl'
                                               (\ q_975 f@(StateAfter l0_974 st0) ->
                                                  trace ("got: " ++ show f)
                                                    (Q.unions
                                                       [q_975,
                                                        foldl'
                                                          (\ q_976 f@(StateAfter jlf0 stf0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_976,
                                                                   foldl'
                                                                     (\ q_979
                                                                        f@(Cond l0_974_977 e0 jlt0
                                                                             jlf0_978)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_979,
                                                                              Q.singleton
                                                                                (traceConclusion
                                                                                   (CondStepFalseCont
                                                                                      l0_974_977
                                                                                      st0
                                                                                      jlf0_978
                                                                                      stf0
                                                                                      e0
                                                                                      jlt0))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_980 vals ->
                                                                           concatMap
                                                                             (\ (v_981, v_982,
                                                                                 v_983)
                                                                                ->
                                                                                pure Cond <*>
                                                                                  mlbs v_980 l0_974
                                                                                  <*> pure v_981
                                                                                  <*> pure v_982
                                                                                  <*>
                                                                                  mlbs v_983 jlf0)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsCond db))]))
                                                          Q.empty
                                                          (M.foldlWithKey'
                                                             (\ rest v_984 vals ->
                                                                S.foldl'
                                                                  (\ acc v_985 ->
                                                                     StateAfter v_984 v_985 : acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (factsStateAfter db))]))
                                               Q.empty
                                               (M.foldlWithKey'
                                                  (\ rest v_986 vals ->
                                                     concatMap
                                                       (\ v_987 ->
                                                          pure StateAfter <*> mlbs v_986 l0 <*>
                                                            pure v_987)
                                                       vals
                                                       ++ rest)
                                                  []
                                                  (factsStateAfter db))]))
                                    Q.empty
                                    ([v_957])]
        CondFact f@v_988 -> Q.unions
                              [q_936,
                               foldl'
                                 (\ q_989 f@(Cond l0 e0 jlt0 jlf0) ->
                                    trace ("got: " ++ show f)
                                      (Q.unions
                                         [q_989,
                                          foldl'
                                            (\ q_991 f@(StateAfter l0_990 st0) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_991,
                                                     foldl'
                                                       (\ q_993 f@(StateAfter jlf0_992 stf0) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_993,
                                                                foldl'
                                                                  (\ q_995 f@(CondVal l0_990_994 _)
                                                                     ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_995,
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondStepFalseCont
                                                                                   l0_990_994
                                                                                   st0
                                                                                   jlf0_992
                                                                                   stf0
                                                                                   e0
                                                                                   jlt0))]))
                                                                  Q.empty
                                                                  (foldl'
                                                                     (\ rest (v_996, v_997) ->
                                                                        (pure CondVal <*>
                                                                           mlbs v_996 l0_990
                                                                           <*> mlbs v_997 False)
                                                                          ++ rest)
                                                                     []
                                                                     (factsCondVal db))]))
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_998 vals ->
                                                             concatMap
                                                               (\ v_999 ->
                                                                  pure StateAfter <*>
                                                                    mlbs v_998 jlf0
                                                                    <*> pure v_999)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsStateAfter db))]))
                                            Q.empty
                                            (M.foldlWithKey'
                                               (\ rest v_1000 vals ->
                                                  concatMap
                                                    (\ v_1001 ->
                                                       pure StateAfter <*> mlbs v_1000 l0 <*>
                                                         pure v_1001)
                                                    vals
                                                    ++ rest)
                                               []
                                               (factsStateAfter db)),
                                          foldl'
                                            (\ q_1003 f@(CondVal l0_1002 _) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_1003,
                                                     foldl'
                                                       (\ q_1005 f@(StateAfter l0_1002_1004 st1) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_1005,
                                                                foldl'
                                                                  (\ q_1007
                                                                     f@(StateAfter jlt0_1006 stt0)
                                                                     ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_1007,
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondStepTrueCont
                                                                                   l0_1002_1004
                                                                                   st1
                                                                                   jlt0_1006
                                                                                   stt0
                                                                                   e0
                                                                                   jlf0))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_1008 vals ->
                                                                        concatMap
                                                                          (\ v_1009 ->
                                                                             pure StateAfter <*>
                                                                               mlbs v_1008 jlt0
                                                                               <*> pure v_1009)
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (factsStateAfter db))]))
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_1010 vals ->
                                                             concatMap
                                                               (\ v_1011 ->
                                                                  pure StateAfter <*>
                                                                    mlbs v_1010 l0_1002
                                                                    <*> pure v_1011)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsStateAfter db))]))
                                            Q.empty
                                            (foldl'
                                               (\ rest (v_1012, v_1013) ->
                                                  (pure CondVal <*> mlbs v_1012 l0 <*>
                                                     mlbs v_1013 True)
                                                    ++ rest)
                                               []
                                               (factsCondVal db)),
                                          foldl'
                                            (\ q_1015 f@(Seq l00 l0_1014) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_1015,
                                                     foldl'
                                                       (\ q_1017 f@(StateAfter l00_1016 st2) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_1017,
                                                                Q.singleton
                                                                  (traceConclusion
                                                                     (EvalCondCont l00_1016 l0_1014
                                                                        st2
                                                                        e0
                                                                        jlt0
                                                                        jlf0))]))
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_1018 vals ->
                                                             concatMap
                                                               (\ v_1019 ->
                                                                  pure StateAfter <*>
                                                                    mlbs v_1018 l00
                                                                    <*> pure v_1019)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsStateAfter db))]))
                                            Q.empty
                                            (foldl'
                                               (\ rest (v_1020, v_1021) ->
                                                  (pure Seq <*> pure v_1020 <*> mlbs v_1021 l0) ++
                                                    rest)
                                               []
                                               (factsSeq db)),
                                          Q.singleton
                                            (traceConclusion (CondInitCont l0 e0 jlt0 jlf0))]))
                                 Q.empty
                                 ([v_988])]
        AssignFact f@v_1022 -> Q.unions
                                 [q_936,
                                  foldl'
                                    (\ q_1023 f@(Assign l2 x0 e3) ->
                                       trace ("got: " ++ show f)
                                         (Q.unions
                                            [q_1023,
                                             foldl'
                                               (\ q_1025 f@(StateAfter l2_1024 st3) ->
                                                  trace ("got: " ++ show f)
                                                    (Q.unions
                                                       [q_1025,
                                                        Q.singleton
                                                          (traceConclusion
                                                             (AssignStepCont l2_1024 st3 x0 e3))]))
                                               Q.empty
                                               (M.foldlWithKey'
                                                  (\ rest v_1026 vals ->
                                                     concatMap
                                                       (\ v_1027 ->
                                                          pure StateAfter <*> mlbs v_1026 l2 <*>
                                                            pure v_1027)
                                                       vals
                                                       ++ rest)
                                                  []
                                                  (factsStateAfter db)),
                                             Q.singleton
                                               (traceConclusion (AssignInitCont l2 x0 e3))]))
                                    Q.empty
                                    ([v_1022])]
        VarFact f@v_1028 -> Q.unions
                              [q_936,
                               foldl'
                                 (\ q_1029 f@(Var l5 x2) ->
                                    trace ("got: " ++ show f)
                                      (Q.unions
                                         [q_1029,
                                          Q.singleton (traceConclusion (VarInitCont l5 x2))]))
                                 Q.empty
                                 ([v_1028])]
        StateAfterFact f@v_1030 -> Q.unions
                                     [q_936,
                                      foldl'
                                        (\ q_1031 f@(StateAfter l10 st10) ->
                                           trace ("got: " ++ show f)
                                             (Q.unions
                                                [q_1031,
                                                 foldl'
                                                   (\ q_1033 f@(Seq l10_1032 l20) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1033,
                                                            foldl'
                                                              (\ q_1035 f@(StateAfter l20_1034 st20)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1035,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (SeqStepCont l10_1032
                                                                               l20_1034
                                                                               st10
                                                                               st20))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1036 vals ->
                                                                    concatMap
                                                                      (\ v_1037 ->
                                                                         pure StateAfter <*>
                                                                           mlbs v_1036 l20
                                                                           <*> pure v_1037)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (foldl'
                                                      (\ rest (v_1038, v_1039) ->
                                                         (pure Seq <*> mlbs v_1038 l10 <*>
                                                            pure v_1039)
                                                           ++ rest)
                                                      []
                                                      (factsSeq db)),
                                                 foldl'
                                                   (\ q_1041 f@(Seq l20 l10_1040) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1041,
                                                            foldl'
                                                              (\ q_1043 f@(StateAfter l20_1042 st20)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1043,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (SeqStepCont l20_1042
                                                                               l10_1040
                                                                               st20
                                                                               st10))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1044 vals ->
                                                                    concatMap
                                                                      (\ v_1045 ->
                                                                         pure StateAfter <*>
                                                                           mlbs v_1044 l20
                                                                           <*> pure v_1045)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (foldl'
                                                      (\ rest (v_1046, v_1047) ->
                                                         (pure Seq <*> pure v_1046 <*>
                                                            mlbs v_1047 l10)
                                                           ++ rest)
                                                      []
                                                      (factsSeq db)),
                                                 foldl'
                                                   (\ q_1048 f@(StateAfter jlf0 stf0) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1048,
                                                            foldl'
                                                              (\ q_1051
                                                                 f@(Cond l10_1049 e0 jlt0 jlf0_1050)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1051,
                                                                       foldl'
                                                                         (\ q_1053
                                                                            f@(CondVal l10_1049_1052
                                                                                 _)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1053,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (CondStepFalseCont
                                                                                          l10_1049_1052
                                                                                          st10
                                                                                          jlf0_1050
                                                                                          stf0
                                                                                          e0
                                                                                          jlt0))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1054, v_1055)
                                                                               ->
                                                                               (pure CondVal <*>
                                                                                  mlbs v_1054
                                                                                    l10_1049
                                                                                  <*>
                                                                                  mlbs v_1055 False)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCondVal db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1056 vals ->
                                                                    concatMap
                                                                      (\ (v_1057, v_1058, v_1059) ->
                                                                         pure Cond <*>
                                                                           mlbs v_1056 l10
                                                                           <*> pure v_1057
                                                                           <*> pure v_1058
                                                                           <*> mlbs v_1059 jlf0)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsCond db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_1060 vals ->
                                                         S.foldl'
                                                           (\ acc v_1061 ->
                                                              StateAfter v_1060 v_1061 : acc)
                                                           []
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsStateAfter db)),
                                                 foldl'
                                                   (\ q_1063 f@(Cond l0 e0 jlt0 l10_1062) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1063,
                                                            foldl'
                                                              (\ q_1065 f@(StateAfter l0_1064 st0)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1065,
                                                                       foldl'
                                                                         (\ q_1067
                                                                            f@(CondVal l0_1064_1066
                                                                                 _)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1067,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (CondStepFalseCont
                                                                                          l0_1064_1066
                                                                                          st0
                                                                                          l10_1062
                                                                                          st10
                                                                                          e0
                                                                                          jlt0))]))
                                                                         Q.empty
                                                                         (foldl'
                                                                            (\ rest (v_1068, v_1069)
                                                                               ->
                                                                               (pure CondVal <*>
                                                                                  mlbs v_1068
                                                                                    l0_1064
                                                                                  <*>
                                                                                  mlbs v_1069 False)
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCondVal db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1070 vals ->
                                                                    concatMap
                                                                      (\ v_1071 ->
                                                                         pure StateAfter <*>
                                                                           mlbs v_1070 l0
                                                                           <*> pure v_1071)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_1072 vals ->
                                                         concatMap
                                                           (\ (v_1073, v_1074, v_1075) ->
                                                              pure Cond <*> pure v_1072 <*>
                                                                pure v_1073
                                                                <*> pure v_1074
                                                                <*> mlbs v_1075 l10)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsCond db)),
                                                 foldl'
                                                   (\ q_1077 f@(Cond l1 e1 l10_1076 jlf1) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1077,
                                                            foldl'
                                                              (\ q_1079 f@(CondVal l1_1078 _) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1079,
                                                                       foldl'
                                                                         (\ q_1081
                                                                            f@(StateAfter
                                                                                 l1_1078_1080 st1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1081,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (CondStepTrueCont
                                                                                          l1_1078_1080
                                                                                          st1
                                                                                          l10_1076
                                                                                          st10
                                                                                          e1
                                                                                          jlf1))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1082 vals ->
                                                                               concatMap
                                                                                 (\ v_1083 ->
                                                                                    pure StateAfter
                                                                                      <*>
                                                                                      mlbs v_1082
                                                                                        l1_1078
                                                                                      <*>
                                                                                      pure v_1083)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsStateAfter db))]))
                                                              Q.empty
                                                              (foldl'
                                                                 (\ rest (v_1084, v_1085) ->
                                                                    (pure CondVal <*> mlbs v_1084 l1
                                                                       <*> mlbs v_1085 True)
                                                                      ++ rest)
                                                                 []
                                                                 (factsCondVal db))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest v_1086 vals ->
                                                         concatMap
                                                           (\ (v_1087, v_1088, v_1089) ->
                                                              pure Cond <*> pure v_1086 <*>
                                                                pure v_1087
                                                                <*> mlbs v_1088 l10
                                                                <*> pure v_1089)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsCond db)),
                                                 foldl'
                                                   (\ q_1091 f@(CondVal l10_1090 _) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1091,
                                                            foldl'
                                                              (\ q_1092 f@(StateAfter jlt1 stt0) ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1092,
                                                                       foldl'
                                                                         (\ q_1095
                                                                            f@(Cond l10_1090_1093 e1
                                                                                 jlt1_1094 jlf1)
                                                                            ->
                                                                            trace
                                                                              ("got: " ++ show f)
                                                                              (Q.unions
                                                                                 [q_1095,
                                                                                  Q.singleton
                                                                                    (traceConclusion
                                                                                       (CondStepTrueCont
                                                                                          l10_1090_1093
                                                                                          st10
                                                                                          jlt1_1094
                                                                                          stt0
                                                                                          e1
                                                                                          jlf1))]))
                                                                         Q.empty
                                                                         (M.foldlWithKey'
                                                                            (\ rest v_1096 vals ->
                                                                               concatMap
                                                                                 (\ (v_1097, v_1098,
                                                                                     v_1099)
                                                                                    ->
                                                                                    pure Cond <*>
                                                                                      mlbs v_1096
                                                                                        l10_1090
                                                                                      <*>
                                                                                      pure v_1097
                                                                                      <*>
                                                                                      mlbs v_1098
                                                                                        jlt1
                                                                                      <*>
                                                                                      pure v_1099)
                                                                                 vals
                                                                                 ++ rest)
                                                                            []
                                                                            (factsCond db))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1100 vals ->
                                                                    S.foldl'
                                                                      (\ acc v_1101 ->
                                                                         StateAfter v_1100 v_1101 :
                                                                           acc)
                                                                      []
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsStateAfter db))]))
                                                   Q.empty
                                                   (foldl'
                                                      (\ rest (v_1102, v_1103) ->
                                                         (pure CondVal <*> mlbs v_1102 l10 <*>
                                                            mlbs v_1103 True)
                                                           ++ rest)
                                                      []
                                                      (factsCondVal db)),
                                                 foldl'
                                                   (\ q_1105 f@(Seq l10_1104 lcond0) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1105,
                                                            foldl'
                                                              (\ q_1107
                                                                 f@(Cond lcond0_1106 e2 jlt2 jlf2)
                                                                 ->
                                                                 trace ("got: " ++ show f)
                                                                   (Q.unions
                                                                      [q_1107,
                                                                       Q.singleton
                                                                         (traceConclusion
                                                                            (EvalCondCont l10_1104
                                                                               lcond0_1106
                                                                               st10
                                                                               e2
                                                                               jlt2
                                                                               jlf2))]))
                                                              Q.empty
                                                              (M.foldlWithKey'
                                                                 (\ rest v_1108 vals ->
                                                                    concatMap
                                                                      (\ (v_1109, v_1110, v_1111) ->
                                                                         pure Cond <*>
                                                                           mlbs v_1108 lcond0
                                                                           <*> pure v_1109
                                                                           <*> pure v_1110
                                                                           <*> pure v_1111)
                                                                      vals
                                                                      ++ rest)
                                                                 []
                                                                 (factsCond db))]))
                                                   Q.empty
                                                   (foldl'
                                                      (\ rest (v_1112, v_1113) ->
                                                         (pure Seq <*> mlbs v_1112 l10 <*>
                                                            pure v_1113)
                                                           ++ rest)
                                                      []
                                                      (factsSeq db)),
                                                 foldl'
                                                   (\ q_1115 f@(Assign l10_1114 x0 e3) ->
                                                      trace ("got: " ++ show f)
                                                        (Q.unions
                                                           [q_1115,
                                                            Q.singleton
                                                              (traceConclusion
                                                                 (AssignStepCont l10_1114 st10 x0
                                                                    e3))]))
                                                   Q.empty
                                                   (M.foldlWithKey'
                                                      (\ rest (v_1116, v_1117) vals ->
                                                         concatMap
                                                           (\ v_1118 ->
                                                              pure Assign <*> mlbs v_1116 l10 <*>
                                                                pure v_1117
                                                                <*> pure v_1118)
                                                           vals
                                                           ++ rest)
                                                      []
                                                      (factsAssign db))]))
                                        Q.empty
                                        ([v_1030])]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_1119 db
  = M.foldlWithKey'
      (\ rest v_1121 vals ->
         concatMap
           (\ v_1122 ->
              pure StateAfter <*> mlbs v_1121 v_1119 <*> pure v_1122)
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
                           "db" -> print db
                           "pq" -> print $ vsep (pretty . show <$> Q.toDescList pq'')
                           "" -> return ()
                           _ -> putStrLn "unknown command"
                       if null ln then return $ go db' pq'' else respondCmds
              in unsafePerformIO respondCmds