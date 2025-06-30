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

evaluate :: Continuation -> Fact
evaluate (Initial f) = f
evaluate (CondStepFalseCont l0 st0 jlf0 stf0 e0 jlt0)
  = StateAfterFact (StateAfter jlf0 (join st0 stf0))
evaluate (AssignInitCont l4 x1 e5)
  = StateAfterFact (StateAfter l4 (empty 0))
evaluate (AssignStepCont l2 st3 x0 e3)
  = StateAfterFact (StateAfter l2 (insert x0 (eval e3 st3) st3))
evaluate (CondInitCont l3 e4 jlt3 jlf3)
  = StateAfterFact (StateAfter l3 (empty 0))
evaluate (EvalCondCont l00 lcond0 st2 e2 jlt2 jlf2)
  = CondValFact (CondVal lcond0 (evaluateConditional e2 st2))
evaluate (SeqStepCont l10 l20 st10 st20)
  = StateAfterFact (StateAfter l20 (join st10 st20))
evaluate (CondStepTrueCont l1 st1 jlt1 stt0 e1 jlf1)
  = StateAfterFact (StateAfter jlt1 (join st1 stt0))
evaluate (VarInitCont l5 x2)
  = StateAfterFact (StateAfter l5 (singleton x2 Bot))

instance Ord Continuation where
        (<=) _ (Initial _) = True
        (<=) _ _ = False

data DataBase = DataBase{factsSeq :: S.HashSet Seq,
                         factsCondVal :: S.HashSet CondVal, factsCond :: S.HashSet Cond,
                         factsAssign :: S.HashSet Assign, factsVar :: S.HashSet Var,
                         factsStateAfter :: S.HashSet StateAfter}
                  deriving (Show, Eq)

emptyDB :: DataBase
emptyDB = DataBase S.empty S.empty S.empty S.empty S.empty S.empty

insertDB :: Fact -> DataBase -> (DataBase, Bool)
insertDB fact db
  = let update hset vnew
          = if not (S.null hset) && any (`subsumes` vnew) hset then
              (hset, False) else
              let hset' = S.filter (not . (vnew `strictlySubsumes`)) hset in
                (S.insert vnew hset', True)
      in
      case fact of
          SeqFact v -> first (\ hset -> db{factsSeq = hset})
                         (update (factsSeq db) v)
          CondValFact v -> first (\ hset -> db{factsCondVal = hset})
                             (update (factsCondVal db) v)
          CondFact v -> first (\ hset -> db{factsCond = hset})
                          (update (factsCond db) v)
          AssignFact v -> first (\ hset -> db{factsAssign = hset})
                            (update (factsAssign db) v)
          VarFact v -> first (\ hset -> db{factsVar = hset})
                         (update (factsVar db) v)
          StateAfterFact v -> first (\ hset -> db{factsStateAfter = hset})
                                (update (factsStateAfter db) v)

type Queue = Q.MaxQueue Continuation

step :: DataBase -> Fact -> Queue -> Queue
step db fact q_184
  = case fact of
        SeqFact f@v_185 -> Q.unions
                             [q_184,
                              foldl'
                                (\ q_186 f@(Seq l10 l20) ->
                                   trace ("got: " ++ show f)
                                     (Q.unions
                                        [q_186,
                                         foldl'
                                           (\ q_188 f@(StateAfter l10_187 st10) ->
                                              trace ("got: " ++ show f)
                                                (Q.unions
                                                   [q_188,
                                                    foldl'
                                                      (\ q_190 f@(StateAfter l20_189 st20) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_190,
                                                               Q.singleton
                                                                 (traceConclusion
                                                                    (SeqStepCont l10_187 l20_189
                                                                       st10
                                                                       st20))]))
                                                      Q.empty
                                                      (foldl'
                                                         (\ rest (StateAfter v_191 v_192) ->
                                                            (pure StateAfter <*> mlbs v_191 l20 <*>
                                                               pure v_192)
                                                              ++ rest)
                                                         []
                                                         (S.toList (factsStateAfter db)))]))
                                           Q.empty
                                           (foldl'
                                              (\ rest (StateAfter v_193 v_194) ->
                                                 (pure StateAfter <*> mlbs v_193 l10 <*> pure v_194)
                                                   ++ rest)
                                              []
                                              (S.toList (factsStateAfter db))),
                                         foldl'
                                           (\ q_196 f@(Cond l20_195 e2 jlt2 jlf2) ->
                                              trace ("got: " ++ show f)
                                                (Q.unions
                                                   [q_196,
                                                    foldl'
                                                      (\ q_198 f@(StateAfter l10_197 st2) ->
                                                         trace ("got: " ++ show f)
                                                           (Q.unions
                                                              [q_198,
                                                               Q.singleton
                                                                 (traceConclusion
                                                                    (EvalCondCont l10_197 l20_195
                                                                       st2
                                                                       e2
                                                                       jlt2
                                                                       jlf2))]))
                                                      Q.empty
                                                      (foldl'
                                                         (\ rest (StateAfter v_199 v_200) ->
                                                            (pure StateAfter <*> mlbs v_199 l10 <*>
                                                               pure v_200)
                                                              ++ rest)
                                                         []
                                                         (S.toList (factsStateAfter db)))]))
                                           Q.empty
                                           (foldl'
                                              (\ rest (Cond v_201 v_202 v_203 v_204) ->
                                                 (pure Cond <*> mlbs v_201 l20 <*> pure v_202 <*>
                                                    pure v_203
                                                    <*> pure v_204)
                                                   ++ rest)
                                              []
                                              (S.toList (factsCond db)))]))
                                Q.empty
                                ([v_185])]
        CondValFact f@v_205 -> Q.unions
                                 [q_184,
                                  foldl'
                                    (\ q_206 f@(CondVal l1) ->
                                       trace ("got: " ++ show f)
                                         (Q.unions
                                            [q_206,
                                             foldl'
                                               (\ q_207 f@(StateAfter jlt1 stt0) ->
                                                  trace ("got: " ++ show f)
                                                    (Q.unions
                                                       [q_207,
                                                        foldl'
                                                          (\ q_209 f@(StateAfter l1_208 st1) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_209,
                                                                   foldl'
                                                                     (\ q_212
                                                                        f@(Cond l1_208_210 e1
                                                                             jlt1_211 jlf1)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_212,
                                                                              Q.singleton
                                                                                (traceConclusion
                                                                                   (CondStepTrueCont
                                                                                      l1_208_210
                                                                                      st1
                                                                                      jlt1_211
                                                                                      stt0
                                                                                      e1
                                                                                      jlf1))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest
                                                                           (Cond v_213 v_214 v_215
                                                                              v_216)
                                                                           ->
                                                                           (pure Cond <*>
                                                                              mlbs v_213 l1_208
                                                                              <*> pure v_214
                                                                              <*> mlbs v_215 jlt1
                                                                              <*> pure v_216)
                                                                             ++ rest)
                                                                        []
                                                                        (S.toList
                                                                           (factsCond db)))]))
                                                          Q.empty
                                                          (foldl'
                                                             (\ rest (StateAfter v_217 v_218) ->
                                                                (pure StateAfter <*> mlbs v_217 l1
                                                                   <*> pure v_218)
                                                                  ++ rest)
                                                             []
                                                             (S.toList (factsStateAfter db)))]))
                                               Q.empty
                                               (S.toList (factsStateAfter db))]))
                                    Q.empty
                                    (foldl'
                                       (\ rest (CondVal v_219 v_220) ->
                                          (pure CondVal <*> pure v_219 <*> mlbs v_220 True) ++ rest)
                                       []
                                       [v_205]),
                                  foldl'
                                    (\ q_221 f@(CondVal l0) ->
                                       trace ("got: " ++ show f)
                                         (Q.unions
                                            [q_221,
                                             foldl'
                                               (\ q_223 f@(StateAfter l0_222 st0) ->
                                                  trace ("got: " ++ show f)
                                                    (Q.unions
                                                       [q_223,
                                                        foldl'
                                                          (\ q_224 f@(StateAfter jlf0 stf0) ->
                                                             trace ("got: " ++ show f)
                                                               (Q.unions
                                                                  [q_224,
                                                                   foldl'
                                                                     (\ q_227
                                                                        f@(Cond l0_222_225 e0 jlt0
                                                                             jlf0_226)
                                                                        ->
                                                                        trace ("got: " ++ show f)
                                                                          (Q.unions
                                                                             [q_227,
                                                                              Q.singleton
                                                                                (traceConclusion
                                                                                   (CondStepFalseCont
                                                                                      l0_222_225
                                                                                      st0
                                                                                      jlf0_226
                                                                                      stf0
                                                                                      e0
                                                                                      jlt0))]))
                                                                     Q.empty
                                                                     (foldl'
                                                                        (\ rest
                                                                           (Cond v_228 v_229 v_230
                                                                              v_231)
                                                                           ->
                                                                           (pure Cond <*>
                                                                              mlbs v_228 l0_222
                                                                              <*> pure v_229
                                                                              <*> pure v_230
                                                                              <*> mlbs v_231 jlf0)
                                                                             ++ rest)
                                                                        []
                                                                        (S.toList
                                                                           (factsCond db)))]))
                                                          Q.empty
                                                          (S.toList (factsStateAfter db))]))
                                               Q.empty
                                               (foldl'
                                                  (\ rest (StateAfter v_232 v_233) ->
                                                     (pure StateAfter <*> mlbs v_232 l0 <*>
                                                        pure v_233)
                                                       ++ rest)
                                                  []
                                                  (S.toList (factsStateAfter db)))]))
                                    Q.empty
                                    (foldl'
                                       (\ rest (CondVal v_234 v_235) ->
                                          (pure CondVal <*> pure v_234 <*> mlbs v_235 False) ++
                                            rest)
                                       []
                                       [v_205])]
        CondFact f@v_236 -> Q.unions
                              [q_184,
                               foldl'
                                 (\ q_237 f@(Cond l0 e0 jlt0 jlf0) ->
                                    trace ("got: " ++ show f)
                                      (Q.unions
                                         [q_237,
                                          foldl'
                                            (\ q_239 f@(StateAfter l0_238 st0) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_239,
                                                     foldl'
                                                       (\ q_241 f@(StateAfter jlf0_240 stf0) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_241,
                                                                foldl'
                                                                  (\ q_243 f@(CondVal l0_238_242) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_243,
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondStepFalseCont
                                                                                   l0_238_242
                                                                                   st0
                                                                                   jlf0_240
                                                                                   stf0
                                                                                   e0
                                                                                   jlt0))]))
                                                                  Q.empty
                                                                  (foldl'
                                                                     (\ rest (CondVal v_244 v_245)
                                                                        ->
                                                                        (pure CondVal <*>
                                                                           mlbs v_244 l0_238
                                                                           <*> mlbs v_245 False)
                                                                          ++ rest)
                                                                     []
                                                                     (S.toList
                                                                        (factsCondVal db)))]))
                                                       Q.empty
                                                       (foldl'
                                                          (\ rest (StateAfter v_246 v_247) ->
                                                             (pure StateAfter <*> mlbs v_246 jlf0
                                                                <*> pure v_247)
                                                               ++ rest)
                                                          []
                                                          (S.toList (factsStateAfter db)))]))
                                            Q.empty
                                            (foldl'
                                               (\ rest (StateAfter v_248 v_249) ->
                                                  (pure StateAfter <*> mlbs v_248 l0 <*> pure v_249)
                                                    ++ rest)
                                               []
                                               (S.toList (factsStateAfter db))),
                                          foldl'
                                            (\ q_251 f@(CondVal l0_250) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_251,
                                                     foldl'
                                                       (\ q_253 f@(StateAfter l0_250_252 st1) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_253,
                                                                foldl'
                                                                  (\ q_255
                                                                     f@(StateAfter jlt0_254 stt0) ->
                                                                     trace ("got: " ++ show f)
                                                                       (Q.unions
                                                                          [q_255,
                                                                           Q.singleton
                                                                             (traceConclusion
                                                                                (CondStepTrueCont
                                                                                   l0_250_252
                                                                                   st1
                                                                                   jlt0_254
                                                                                   stt0
                                                                                   e0
                                                                                   jlf0))]))
                                                                  Q.empty
                                                                  (foldl'
                                                                     (\ rest
                                                                        (StateAfter v_256 v_257) ->
                                                                        (pure StateAfter <*>
                                                                           mlbs v_256 jlt0
                                                                           <*> pure v_257)
                                                                          ++ rest)
                                                                     []
                                                                     (S.toList
                                                                        (factsStateAfter db)))]))
                                                       Q.empty
                                                       (foldl'
                                                          (\ rest (StateAfter v_258 v_259) ->
                                                             (pure StateAfter <*> mlbs v_258 l0_250
                                                                <*> pure v_259)
                                                               ++ rest)
                                                          []
                                                          (S.toList (factsStateAfter db)))]))
                                            Q.empty
                                            (foldl'
                                               (\ rest (CondVal v_260 v_261) ->
                                                  (pure CondVal <*> mlbs v_260 l0 <*>
                                                     mlbs v_261 True)
                                                    ++ rest)
                                               []
                                               (S.toList (factsCondVal db))),
                                          foldl'
                                            (\ q_263 f@(Seq l00 l0_262) ->
                                               trace ("got: " ++ show f)
                                                 (Q.unions
                                                    [q_263,
                                                     foldl'
                                                       (\ q_265 f@(StateAfter l00_264 st2) ->
                                                          trace ("got: " ++ show f)
                                                            (Q.unions
                                                               [q_265,
                                                                Q.singleton
                                                                  (traceConclusion
                                                                     (EvalCondCont l00_264 l0_262
                                                                        st2
                                                                        e0
                                                                        jlt0
                                                                        jlf0))]))
                                                       Q.empty
                                                       (foldl'
                                                          (\ rest (StateAfter v_266 v_267) ->
                                                             (pure StateAfter <*> mlbs v_266 l00 <*>
                                                                pure v_267)
                                                               ++ rest)
                                                          []
                                                          (S.toList (factsStateAfter db)))]))
                                            Q.empty
                                            (foldl'
                                               (\ rest (Seq v_268 v_269) ->
                                                  (pure Seq <*> pure v_268 <*> mlbs v_269 l0) ++
                                                    rest)
                                               []
                                               (S.toList (factsSeq db))),
                                          Q.singleton
                                            (traceConclusion (CondInitCont l0 e0 jlt0 jlf0))]))
                                 Q.empty
                                 ([v_236])]
        AssignFact f@v_270 -> Q.unions
                                [q_184,
                                 foldl'
                                   (\ q_271 f@(Assign l2 x0 e3) ->
                                      trace ("got: " ++ show f)
                                        (Q.unions
                                           [q_271,
                                            foldl'
                                              (\ q_273 f@(StateAfter l2_272 st3) ->
                                                 trace ("got: " ++ show f)
                                                   (Q.unions
                                                      [q_273,
                                                       Q.singleton
                                                         (traceConclusion
                                                            (AssignStepCont l2_272 st3 x0 e3))]))
                                              Q.empty
                                              (foldl'
                                                 (\ rest (StateAfter v_274 v_275) ->
                                                    (pure StateAfter <*> mlbs v_274 l2 <*>
                                                       pure v_275)
                                                      ++ rest)
                                                 []
                                                 (S.toList (factsStateAfter db))),
                                            Q.singleton
                                              (traceConclusion (AssignInitCont l2 x0 e3))]))
                                   Q.empty
                                   ([v_270])]
        VarFact f@v_276 -> Q.unions
                             [q_184,
                              foldl'
                                (\ q_277 f@(Var l5 x2) ->
                                   trace ("got: " ++ show f)
                                     (Q.unions
                                        [q_277, Q.singleton (traceConclusion (VarInitCont l5 x2))]))
                                Q.empty
                                ([v_276])]
        StateAfterFact f@v_278 -> Q.unions
                                    [q_184,
                                     foldl'
                                       (\ q_279 f@(StateAfter l10 st10) ->
                                          trace ("got: " ++ show f)
                                            (Q.unions
                                               [q_279,
                                                foldl'
                                                  (\ q_281 f@(Seq l10_280 l20) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_281,
                                                           foldl'
                                                             (\ q_283 f@(StateAfter l20_282 st20) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_283,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (SeqStepCont l10_280
                                                                              l20_282
                                                                              st10
                                                                              st20))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest (StateAfter v_284 v_285) ->
                                                                   (pure StateAfter <*>
                                                                      mlbs v_284 l20
                                                                      <*> pure v_285)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsStateAfter db)))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Seq v_286 v_287) ->
                                                        (pure Seq <*> mlbs v_286 l10 <*> pure v_287)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsSeq db))),
                                                foldl'
                                                  (\ q_289 f@(Seq l20 l10_288) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_289,
                                                           foldl'
                                                             (\ q_291 f@(StateAfter l20_290 st20) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_291,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (SeqStepCont l20_290
                                                                              l10_288
                                                                              st20
                                                                              st10))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest (StateAfter v_292 v_293) ->
                                                                   (pure StateAfter <*>
                                                                      mlbs v_292 l20
                                                                      <*> pure v_293)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsStateAfter db)))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Seq v_294 v_295) ->
                                                        (pure Seq <*> pure v_294 <*> mlbs v_295 l10)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsSeq db))),
                                                foldl'
                                                  (\ q_296 f@(StateAfter jlf0 stf0) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_296,
                                                           foldl'
                                                             (\ q_299
                                                                f@(Cond l10_297 e0 jlt0 jlf0_298) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_299,
                                                                      foldl'
                                                                        (\ q_301
                                                                           f@(CondVal l10_297_300)
                                                                           ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_301,
                                                                                 Q.singleton
                                                                                   (traceConclusion
                                                                                      (CondStepFalseCont
                                                                                         l10_297_300
                                                                                         st10
                                                                                         jlf0_298
                                                                                         stf0
                                                                                         e0
                                                                                         jlt0))]))
                                                                        Q.empty
                                                                        (foldl'
                                                                           (\ rest
                                                                              (CondVal v_302 v_303)
                                                                              ->
                                                                              (pure CondVal <*>
                                                                                 mlbs v_302 l10_297
                                                                                 <*>
                                                                                 mlbs v_303 False)
                                                                                ++ rest)
                                                                           []
                                                                           (S.toList
                                                                              (factsCondVal db)))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest
                                                                   (Cond v_304 v_305 v_306 v_307) ->
                                                                   (pure Cond <*> mlbs v_304 l10 <*>
                                                                      pure v_305
                                                                      <*> pure v_306
                                                                      <*> mlbs v_307 jlf0)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsCond db)))]))
                                                  Q.empty
                                                  (S.toList (factsStateAfter db)),
                                                foldl'
                                                  (\ q_309 f@(Cond l0 e0 jlt0 l10_308) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_309,
                                                           foldl'
                                                             (\ q_311 f@(StateAfter l0_310 st0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_311,
                                                                      foldl'
                                                                        (\ q_313
                                                                           f@(CondVal l0_310_312) ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_313,
                                                                                 Q.singleton
                                                                                   (traceConclusion
                                                                                      (CondStepFalseCont
                                                                                         l0_310_312
                                                                                         st0
                                                                                         l10_308
                                                                                         st10
                                                                                         e0
                                                                                         jlt0))]))
                                                                        Q.empty
                                                                        (foldl'
                                                                           (\ rest
                                                                              (CondVal v_314 v_315)
                                                                              ->
                                                                              (pure CondVal <*>
                                                                                 mlbs v_314 l0_310
                                                                                 <*>
                                                                                 mlbs v_315 False)
                                                                                ++ rest)
                                                                           []
                                                                           (S.toList
                                                                              (factsCondVal db)))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest (StateAfter v_316 v_317) ->
                                                                   (pure StateAfter <*>
                                                                      mlbs v_316 l0
                                                                      <*> pure v_317)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsStateAfter db)))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Cond v_318 v_319 v_320 v_321) ->
                                                        (pure Cond <*> pure v_318 <*> pure v_319 <*>
                                                           pure v_320
                                                           <*> mlbs v_321 l10)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsCond db))),
                                                foldl'
                                                  (\ q_323 f@(Cond l1 e1 l10_322 jlf1) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_323,
                                                           foldl'
                                                             (\ q_325 f@(CondVal l1_324) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_325,
                                                                      foldl'
                                                                        (\ q_327
                                                                           f@(StateAfter l1_324_326
                                                                                st1)
                                                                           ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_327,
                                                                                 Q.singleton
                                                                                   (traceConclusion
                                                                                      (CondStepTrueCont
                                                                                         l1_324_326
                                                                                         st1
                                                                                         l10_322
                                                                                         st10
                                                                                         e1
                                                                                         jlf1))]))
                                                                        Q.empty
                                                                        (foldl'
                                                                           (\ rest
                                                                              (StateAfter v_328
                                                                                 v_329)
                                                                              ->
                                                                              (pure StateAfter <*>
                                                                                 mlbs v_328 l1_324
                                                                                 <*> pure v_329)
                                                                                ++ rest)
                                                                           []
                                                                           (S.toList
                                                                              (factsStateAfter
                                                                                 db)))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest (CondVal v_330 v_331) ->
                                                                   (pure CondVal <*> mlbs v_330 l1
                                                                      <*> mlbs v_331 True)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsCondVal db)))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Cond v_332 v_333 v_334 v_335) ->
                                                        (pure Cond <*> pure v_332 <*> pure v_333 <*>
                                                           mlbs v_334 l10
                                                           <*> pure v_335)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsCond db))),
                                                foldl'
                                                  (\ q_337 f@(CondVal l10_336) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_337,
                                                           foldl'
                                                             (\ q_338 f@(StateAfter jlt1 stt0) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_338,
                                                                      foldl'
                                                                        (\ q_341
                                                                           f@(Cond l10_336_339 e1
                                                                                jlt1_340 jlf1)
                                                                           ->
                                                                           trace ("got: " ++ show f)
                                                                             (Q.unions
                                                                                [q_341,
                                                                                 Q.singleton
                                                                                   (traceConclusion
                                                                                      (CondStepTrueCont
                                                                                         l10_336_339
                                                                                         st10
                                                                                         jlt1_340
                                                                                         stt0
                                                                                         e1
                                                                                         jlf1))]))
                                                                        Q.empty
                                                                        (foldl'
                                                                           (\ rest
                                                                              (Cond v_342 v_343
                                                                                 v_344 v_345)
                                                                              ->
                                                                              (pure Cond <*>
                                                                                 mlbs v_342 l10_336
                                                                                 <*> pure v_343
                                                                                 <*> mlbs v_344 jlt1
                                                                                 <*> pure v_345)
                                                                                ++ rest)
                                                                           []
                                                                           (S.toList
                                                                              (factsCond db)))]))
                                                             Q.empty
                                                             (S.toList (factsStateAfter db))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (CondVal v_346 v_347) ->
                                                        (pure CondVal <*> mlbs v_346 l10 <*>
                                                           mlbs v_347 True)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsCondVal db))),
                                                foldl'
                                                  (\ q_349 f@(Seq l10_348 lcond0) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_349,
                                                           foldl'
                                                             (\ q_351
                                                                f@(Cond lcond0_350 e2 jlt2 jlf2) ->
                                                                trace ("got: " ++ show f)
                                                                  (Q.unions
                                                                     [q_351,
                                                                      Q.singleton
                                                                        (traceConclusion
                                                                           (EvalCondCont l10_348
                                                                              lcond0_350
                                                                              st10
                                                                              e2
                                                                              jlt2
                                                                              jlf2))]))
                                                             Q.empty
                                                             (foldl'
                                                                (\ rest
                                                                   (Cond v_352 v_353 v_354 v_355) ->
                                                                   (pure Cond <*> mlbs v_352 lcond0
                                                                      <*> pure v_353
                                                                      <*> pure v_354
                                                                      <*> pure v_355)
                                                                     ++ rest)
                                                                []
                                                                (S.toList (factsCond db)))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Seq v_356 v_357) ->
                                                        (pure Seq <*> mlbs v_356 l10 <*> pure v_357)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsSeq db))),
                                                foldl'
                                                  (\ q_359 f@(Assign l10_358 x0 e3) ->
                                                     trace ("got: " ++ show f)
                                                       (Q.unions
                                                          [q_359,
                                                           Q.singleton
                                                             (traceConclusion
                                                                (AssignStepCont l10_358 st10 x0
                                                                   e3))]))
                                                  Q.empty
                                                  (foldl'
                                                     (\ rest (Assign v_360 v_361 v_362) ->
                                                        (pure Assign <*> mlbs v_360 l10 <*>
                                                           pure v_361
                                                           <*> pure v_362)
                                                          ++ rest)
                                                     []
                                                     (S.toList (factsAssign db)))]))
                                       Q.empty
                                       ([v_278])]

stateAfter :: Natural -> DataBase -> [StateAfter]
stateAfter v_363 db
  = foldl'
      (\ rest (StateAfter v_365 v_366) ->
         (pure StateAfter <*> mlbs v_365 v_363 <*> pure v_366) ++ rest)
      []
      (S.toList (factsStateAfter db))

traceConclusion :: Show a => a -> a
traceConclusion c = trace ("concluded: " ++ show c) c

compute :: [Fact] -> DataBase
compute = go emptyDB . Q.fromList . map Initial
  where {-# NOINLINE go #-}
        
        go :: DataBase -> Queue -> DataBase
        go db pq
          | Q.null pq = db
          | otherwise =
            let (nextFact, pq') = first evaluate $ Q.deleteFindMax pq
                (db', changed) = insertDB nextFact db
                _ = if not changed then trace ("dropped: " ++ show nextFact) ()
                      else ()
                pq'' = if changed then step db' nextFact pq' else pq'
                respondCmds
                  = do ln <- getLine
                       case ln of
                           "db" -> print db
                           "pq" -> print $ vsep (pretty . show <$> Q.toDescList pq'')
                           "" -> return ()
                           _ -> putStrLn "unknown command"
                       if null ln then return $ go db' pq'' else respondCmds
              in unsafePerformIO respondCmds