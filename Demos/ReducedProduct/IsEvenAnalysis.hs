{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC
  -Wno-unused-binds -Wno-unused-matches -Wno-unused-imports -Wno-missing-signatures -Wno-missing-export-lists#-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness#-}
{-# LANGUAGE Strict #-}
module ReducedProduct.IsEvenAnalysis where
import ReducedProduct.IsEven
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

data StateBefore = StateBefore Natural State
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
                  | AssignStepCont Natural String Expr State Natural State
                  | EvalCondFalseCont Natural State Expr Natural Natural State
                  | CondInitCont Natural Expr Natural Natural
                  | EvalCondTrueCont Natural State Expr Natural Natural State
                  | VarInitCont Natural String
                      deriving (Show, Eq)

evaluate :: DataBase -> Continuation -> [Fact]
evaluate _ (Initial f) = [f]
evaluate db (AssignInitCont l2 x2 e4)
  = [StateBeforeFact (StateBefore l2 (empty 0))]
evaluate db
  (AssignStepCont lAssign0 x0 e2 stAssign0 lAfter0 stAfter0)
  = [StateBeforeFact
       (StateBefore lAfter0
          (join (insert x0 (eval e2 stAssign0) stAssign0) stAfter0))]
evaluate db (EvalCondFalseCont lcond0 stc0 e0 jlt0 jlf0 stf0)
  = [StateBeforeFact
       (StateBefore jlf0 (join (narrowConditionalFalse e0 stc0) stf0))]
evaluate db (CondInitCont l1 e3 jlt2 jlf2)
  = [StateBeforeFact (StateBefore l1 (empty 0))]
evaluate db (EvalCondTrueCont lcond1 stc1 e1 jlt1 jlf1 stt0)
  = [StateBeforeFact
       (StateBefore jlt1 (join (narrowConditional e1 stc1) stt0))]
evaluate db (VarInitCont l0 x1)
  = [StateBeforeFact (StateBefore l0 (empty 0))]

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsStateBefore ::
                         M.HashMap Natural (S.HashSet State),
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
step db fact q_143
  = case fact of
        SeqFact f@(Seq v_144 v_145) -> Q.unions
                                         [q_143,
                                          foldl'
                                            (\ q_146 f@(Seq lAssign0 lAfter0) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_146,
                                                     foldl'
                                                       (\ q_148 f@(StateBefore lAfter0_147 stAfter0)
                                                          ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_148,
                                                                foldl'
                                                                  (\ q_150
                                                                     f@(StateBefore lAssign0_149
                                                                          stAssign0)
                                                                     ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_150,
                                                                           foldl'
                                                                             (\ q_152
                                                                                f@(Assign
                                                                                     lAssign0_149_151
                                                                                     x0 e2)
                                                                                ->
                                                                                trace
                                                                                  ("got: " ++
                                                                                     show f)
                                                                                  (Q.unions
                                                                                     [q_152,
                                                                                      Q.singleton
                                                                                        (traceConclusion
                                                                                           (AssignStepCont
                                                                                              lAssign0_149_151
                                                                                              x0
                                                                                              e2
                                                                                              stAssign0
                                                                                              lAfter0_147
                                                                                              stAfter0))]))
                                                                             Q.empty
                                                                             (M.foldlWithKey'
                                                                                (\ rest
                                                                                   (v_153, v_154)
                                                                                   vals ->
                                                                                   concatMap
                                                                                     (\ v_155 ->
                                                                                        pure Assign
                                                                                          <*>
                                                                                          mlbs v_153
                                                                                            lAssign0_149
                                                                                          <*>
                                                                                          pure v_154
                                                                                          <*>
                                                                                          pure
                                                                                            v_155)
                                                                                     vals
                                                                                     ++ rest)
                                                                                []
                                                                                (factsAssign db))]))
                                                                  Q.empty
                                                                  (M.foldlWithKey'
                                                                     (\ rest v_156 vals ->
                                                                        concatMap
                                                                          (\ v_157 ->
                                                                             pure StateBefore <*>
                                                                               mlbs v_156 lAssign0
                                                                               <*> pure v_157)
                                                                          vals
                                                                          ++ rest)
                                                                     []
                                                                     (factsStateBefore db))]))
                                                       Q.empty
                                                       (M.foldlWithKey'
                                                          (\ rest v_158 vals ->
                                                             concatMap
                                                               (\ v_159 ->
                                                                  pure StateBefore <*>
                                                                    mlbs v_158 lAfter0
                                                                    <*> pure v_159)
                                                               vals
                                                               ++ rest)
                                                          []
                                                          (factsStateBefore db))]))
                                            Q.empty
                                            (S.foldl'
                                               (\ rest (v_160, v_161) -> Seq v_160 v_161 : rest)
                                               []
                                               (S.singleton (v_144, v_145)))]
        CondFact f@(Cond v_162 v_163 v_164 v_165) -> Q.unions
                                                       [q_143,
                                                        foldl'
                                                          (\ q_166 f@(Cond lcond0 e0 jlt0 jlf0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_166,
                                                                   foldl'
                                                                     (\ q_168
                                                                        f@(StateBefore jlf0_167
                                                                             stf0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_168,
                                                                              foldl'
                                                                                (\ q_170
                                                                                   f@(StateBefore
                                                                                        lcond0_169
                                                                                        stc0)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_170,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondFalseCont
                                                                                                 lcond0_169
                                                                                                 stc0
                                                                                                 e0
                                                                                                 jlt0
                                                                                                 jlf0_167
                                                                                                 stf0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_171
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_172 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_171
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_172)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_173 vals ->
                                                                           concatMap
                                                                             (\ v_174 ->
                                                                                pure StateBefore <*>
                                                                                  mlbs v_173 jlf0
                                                                                  <*> pure v_174)
                                                                             vals
                                                                             ++ rest)
                                                                        []
                                                                        (factsStateBefore db)),
                                                                   foldl'
                                                                     (\ q_176
                                                                        f@(StateBefore jlt0_175
                                                                             stt0)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_176,
                                                                              foldl'
                                                                                (\ q_178
                                                                                   f@(StateBefore
                                                                                        lcond0_177
                                                                                        stc1)
                                                                                   ->
                                                                                   trace
                                                                                     ("got: " ++
                                                                                        show f)
                                                                                     (Q.unions
                                                                                        [q_178,
                                                                                         Q.singleton
                                                                                           (traceConclusion
                                                                                              (EvalCondTrueCont
                                                                                                 lcond0_177
                                                                                                 stc1
                                                                                                 e0
                                                                                                 jlt0_175
                                                                                                 jlf0
                                                                                                 stt0))]))
                                                                                Q.empty
                                                                                (M.foldlWithKey'
                                                                                   (\ rest v_179
                                                                                      vals ->
                                                                                      concatMap
                                                                                        (\ v_180 ->
                                                                                           pure
                                                                                             StateBefore
                                                                                             <*>
                                                                                             mlbs
                                                                                               v_179
                                                                                               lcond0
                                                                                             <*>
                                                                                             pure
                                                                                               v_180)
                                                                                        vals
                                                                                        ++ rest)
                                                                                   []
                                                                                   (factsStateBefore
                                                                                      db))]))
                                                                     Q.empty
                                                                     (M.foldlWithKey'
                                                                        (\ rest v_181 vals ->
                                                                           concatMap
                                                                             (\ v_182 ->
                                                                                pure StateBefore <*>
                                                                                  mlbs v_181 jlt0
                                                                                  <*> pure v_182)
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
                                                             (\ rest v_183 vals ->
                                                                S.foldl'
                                                                  (\ acc (v_184, v_185, v_186) ->
                                                                     Cond v_183 v_184 v_185 v_186 :
                                                                       acc)
                                                                  []
                                                                  vals
                                                                  ++ rest)
                                                             []
                                                             (M.singleton v_162
                                                                (S.singleton
                                                                   (v_163, v_164, v_165))))]
        AssignFact f@(Assign v_187 v_188 v_189) -> Q.unions
                                                     [q_143,
                                                      foldl'
                                                        (\ q_190 f@(Assign lAssign0 x0 e2) ->
                                                           trace ("got: " ++ show f)
                                                             (Q.unions
                                                                [q_190,
                                                                 foldl'
                                                                   (\ q_191
                                                                      f@(StateBefore lAfter0
                                                                           stAfter0)
                                                                      ->
                                                                      trace ("got: " ++ show f)
                                                                        (Q.unions
                                                                           [q_191,
                                                                            foldl'
                                                                              (\ q_193
                                                                                 f@(StateBefore
                                                                                      lAssign0_192
                                                                                      stAssign0)
                                                                                 ->
                                                                                 trace
                                                                                   ("got: " ++
                                                                                      show f)
                                                                                   (Q.unions
                                                                                      [q_193,
                                                                                       foldl'
                                                                                         (\ q_196
                                                                                            f@(Seq
                                                                                                 lAssign0_192_194
                                                                                                 lAfter0_195)
                                                                                            ->
                                                                                            trace
                                                                                              ("got: "
                                                                                                 ++
                                                                                                 show
                                                                                                   f)
                                                                                              (Q.unions
                                                                                                 [q_196,
                                                                                                  Q.singleton
                                                                                                    (traceConclusion
                                                                                                       (AssignStepCont
                                                                                                          lAssign0_192_194
                                                                                                          x0
                                                                                                          e2
                                                                                                          stAssign0
                                                                                                          lAfter0_195
                                                                                                          stAfter0))]))
                                                                                         Q.empty
                                                                                         (foldl'
                                                                                            (\ rest
                                                                                               (v_197,
                                                                                                v_198)
                                                                                               ->
                                                                                               (pure
                                                                                                  Seq
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_197
                                                                                                    lAssign0_192
                                                                                                  <*>
                                                                                                  mlbs
                                                                                                    v_198
                                                                                                    lAfter0)
                                                                                                 ++
                                                                                                 rest)
                                                                                            []
                                                                                            (factsSeq
                                                                                               db))]))
                                                                              Q.empty
                                                                              (M.foldlWithKey'
                                                                                 (\ rest v_199 vals
                                                                                    ->
                                                                                    concatMap
                                                                                      (\ v_200 ->
                                                                                         pure
                                                                                           StateBefore
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_199
                                                                                             lAssign0
                                                                                           <*>
                                                                                           pure
                                                                                             v_200)
                                                                                      vals
                                                                                      ++ rest)
                                                                                 []
                                                                                 (factsStateBefore
                                                                                    db))]))
                                                                   Q.empty
                                                                   (M.foldlWithKey'
                                                                      (\ rest v_201 vals ->
                                                                         S.foldl'
                                                                           (\ acc v_202 ->
                                                                              StateBefore v_201
                                                                                v_202
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
                                                           (\ rest (v_203, v_204) vals ->
                                                              S.foldl'
                                                                (\ acc v_205 ->
                                                                   Assign v_203 v_204 v_205 : acc)
                                                                []
                                                                vals
                                                                ++ rest)
                                                           []
                                                           (M.singleton (v_187, v_188)
                                                              (S.singleton v_189)))]
        VarFact f@(Var v_206 v_207) -> Q.unions
                                         [q_143,
                                          foldl'
                                            (\ q_208 f@(Var l0 x1) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_208,
                                                     Q.singleton
                                                       (traceConclusion (VarInitCont l0 x1))]))
                                            Q.empty
                                            (S.foldl'
                                               (\ rest (v_209, v_210) -> Var v_209 v_210 : rest)
                                               []
                                               (S.singleton (v_206, v_207)))]
        StateBeforeFact f@(StateBefore v_211 v_212) -> Q.unions
                                                         [q_143,
                                                          foldl'
                                                            (\ q_213 f@(StateBefore jlf0 stf0) ->
                                                               trace ("got: " ++ show f)
                                                                 (Q.unions
                                                                    [q_213,
                                                                     foldl'
                                                                       (\ q_215
                                                                          f@(Cond lcond0 e0 jlt0
                                                                               jlf0_214)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_215,
                                                                                foldl'
                                                                                  (\ q_217
                                                                                     f@(StateBefore
                                                                                          lcond0_216
                                                                                          stc0)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_217,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondFalseCont
                                                                                                   lcond0_216
                                                                                                   stc0
                                                                                                   e0
                                                                                                   jlt0
                                                                                                   jlf0_214
                                                                                                   stf0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_218
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_219
                                                                                             ->
                                                                                             pure
                                                                                               StateBefore
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_218
                                                                                                 lcond0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_219)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateBefore
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_220 vals ->
                                                                             concatMap
                                                                               (\ (v_221, v_222,
                                                                                   v_223)
                                                                                  ->
                                                                                  pure Cond <*>
                                                                                    pure v_220
                                                                                    <*> pure v_221
                                                                                    <*> pure v_222
                                                                                    <*>
                                                                                    mlbs v_223 jlf0)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_225
                                                                          f@(Cond jlf0_224 e0 jlt0
                                                                               lcond0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_225,
                                                                                foldl'
                                                                                  (\ q_227
                                                                                     f@(StateBefore
                                                                                          lcond0_226
                                                                                          stc0)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_227,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondFalseCont
                                                                                                   jlf0_224
                                                                                                   stf0
                                                                                                   e0
                                                                                                   jlt0
                                                                                                   lcond0_226
                                                                                                   stc0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_228
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_229
                                                                                             ->
                                                                                             pure
                                                                                               StateBefore
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_228
                                                                                                 lcond0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_229)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateBefore
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_230 vals ->
                                                                             concatMap
                                                                               (\ (v_231, v_232,
                                                                                   v_233)
                                                                                  ->
                                                                                  pure Cond <*>
                                                                                    mlbs v_230 jlf0
                                                                                    <*> pure v_231
                                                                                    <*> pure v_232
                                                                                    <*> pure v_233)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_235
                                                                          f@(Cond lcond1 e1 jlf0_234
                                                                               jlf1)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_235,
                                                                                foldl'
                                                                                  (\ q_237
                                                                                     f@(StateBefore
                                                                                          lcond1_236
                                                                                          stc1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_237,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondTrueCont
                                                                                                   lcond1_236
                                                                                                   stc1
                                                                                                   e1
                                                                                                   jlf0_234
                                                                                                   jlf1
                                                                                                   stf0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_238
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_239
                                                                                             ->
                                                                                             pure
                                                                                               StateBefore
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_238
                                                                                                 lcond1
                                                                                               <*>
                                                                                               pure
                                                                                                 v_239)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsStateBefore
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_240 vals ->
                                                                             concatMap
                                                                               (\ (v_241, v_242,
                                                                                   v_243)
                                                                                  ->
                                                                                  pure Cond <*>
                                                                                    pure v_240
                                                                                    <*> pure v_241
                                                                                    <*>
                                                                                    mlbs v_242 jlf0
                                                                                    <*> pure v_243)
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsCond db)),
                                                                     foldl'
                                                                       (\ q_244
                                                                          f@(StateBefore jlt1 stt0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_244,
                                                                                foldl'
                                                                                  (\ q_247
                                                                                     f@(Cond
                                                                                          jlf0_245
                                                                                          e1
                                                                                          jlt1_246
                                                                                          jlf1)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_247,
                                                                                           Q.singleton
                                                                                             (traceConclusion
                                                                                                (EvalCondTrueCont
                                                                                                   jlf0_245
                                                                                                   stf0
                                                                                                   e1
                                                                                                   jlt1_246
                                                                                                   jlf1
                                                                                                   stt0))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest v_248
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ (v_249,
                                                                                              v_250,
                                                                                              v_251)
                                                                                             ->
                                                                                             pure
                                                                                               Cond
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_248
                                                                                                 jlf0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_249
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_250
                                                                                                 jlt1
                                                                                               <*>
                                                                                               pure
                                                                                                 v_251)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsCond
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_252 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_253 ->
                                                                                  StateBefore v_252
                                                                                    v_253
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db)),
                                                                     foldl'
                                                                       (\ q_254
                                                                          f@(StateBefore lAssign0
                                                                               stAssign0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_254,
                                                                                foldl'
                                                                                  (\ q_256
                                                                                     f@(Assign
                                                                                          lAssign0_255
                                                                                          x0 e2)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_256,
                                                                                           foldl'
                                                                                             (\ q_259
                                                                                                f@(Seq
                                                                                                     lAssign0_255_257
                                                                                                     jlf0_258)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_259,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (AssignStepCont
                                                                                                              lAssign0_255_257
                                                                                                              x0
                                                                                                              e2
                                                                                                              stAssign0
                                                                                                              jlf0_258
                                                                                                              stf0))]))
                                                                                             Q.empty
                                                                                             (foldl'
                                                                                                (\ rest
                                                                                                   (v_260,
                                                                                                    v_261)
                                                                                                   ->
                                                                                                   (pure
                                                                                                      Seq
                                                                                                      <*>
                                                                                                      mlbs
                                                                                                        v_260
                                                                                                        lAssign0_255
                                                                                                      <*>
                                                                                                      mlbs
                                                                                                        v_261
                                                                                                        jlf0)
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsSeq
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (M.foldlWithKey'
                                                                                     (\ rest
                                                                                        (v_262,
                                                                                         v_263)
                                                                                        vals ->
                                                                                        concatMap
                                                                                          (\ v_264
                                                                                             ->
                                                                                             pure
                                                                                               Assign
                                                                                               <*>
                                                                                               mlbs
                                                                                                 v_262
                                                                                                 lAssign0
                                                                                               <*>
                                                                                               pure
                                                                                                 v_263
                                                                                               <*>
                                                                                               pure
                                                                                                 v_264)
                                                                                          vals
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsAssign
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_265 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_266 ->
                                                                                  StateBefore v_265
                                                                                    v_266
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db)),
                                                                     foldl'
                                                                       (\ q_267
                                                                          f@(StateBefore lAfter0
                                                                               stAfter0)
                                                                          ->
                                                                          trace ("got: " ++ show f)
                                                                            (Q.unions
                                                                               [q_267,
                                                                                foldl'
                                                                                  (\ q_270
                                                                                     f@(Seq jlf0_268
                                                                                          lAfter0_269)
                                                                                     ->
                                                                                     trace
                                                                                       ("got: " ++
                                                                                          show f)
                                                                                       (Q.unions
                                                                                          [q_270,
                                                                                           foldl'
                                                                                             (\ q_272
                                                                                                f@(Assign
                                                                                                     jlf0_268_271
                                                                                                     x0
                                                                                                     e2)
                                                                                                ->
                                                                                                trace
                                                                                                  ("got: "
                                                                                                     ++
                                                                                                     show
                                                                                                       f)
                                                                                                  (Q.unions
                                                                                                     [q_272,
                                                                                                      Q.singleton
                                                                                                        (traceConclusion
                                                                                                           (AssignStepCont
                                                                                                              jlf0_268_271
                                                                                                              x0
                                                                                                              e2
                                                                                                              stf0
                                                                                                              lAfter0_269
                                                                                                              stAfter0))]))
                                                                                             Q.empty
                                                                                             (M.foldlWithKey'
                                                                                                (\ rest
                                                                                                   (v_273,
                                                                                                    v_274)
                                                                                                   vals
                                                                                                   ->
                                                                                                   concatMap
                                                                                                     (\ v_275
                                                                                                        ->
                                                                                                        pure
                                                                                                          Assign
                                                                                                          <*>
                                                                                                          mlbs
                                                                                                            v_273
                                                                                                            jlf0_268
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_274
                                                                                                          <*>
                                                                                                          pure
                                                                                                            v_275)
                                                                                                     vals
                                                                                                     ++
                                                                                                     rest)
                                                                                                []
                                                                                                (factsAssign
                                                                                                   db))]))
                                                                                  Q.empty
                                                                                  (foldl'
                                                                                     (\ rest
                                                                                        (v_276,
                                                                                         v_277)
                                                                                        ->
                                                                                        (pure Seq
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_276
                                                                                             jlf0
                                                                                           <*>
                                                                                           mlbs
                                                                                             v_277
                                                                                             lAfter0)
                                                                                          ++ rest)
                                                                                     []
                                                                                     (factsSeq
                                                                                        db))]))
                                                                       Q.empty
                                                                       (M.foldlWithKey'
                                                                          (\ rest v_278 vals ->
                                                                             S.foldl'
                                                                               (\ acc v_279 ->
                                                                                  StateBefore v_278
                                                                                    v_279
                                                                                    : acc)
                                                                               []
                                                                               vals
                                                                               ++ rest)
                                                                          []
                                                                          (factsStateBefore db))]))
                                                            Q.empty
                                                            (M.foldlWithKey'
                                                               (\ rest v_280 vals ->
                                                                  S.foldl'
                                                                    (\ acc v_281 ->
                                                                       StateBefore v_280 v_281 :
                                                                         acc)
                                                                    []
                                                                    vals
                                                                    ++ rest)
                                                               []
                                                               (M.singleton v_211
                                                                  (S.singleton v_212)))]

stateBefore :: DataBase -> [StateBefore]
stateBefore db
  = M.foldlWithKey'
      (\ rest v_283 vals ->
         S.foldl' (\ acc v_284 -> StateBefore v_283 v_284 : acc) [] vals ++
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