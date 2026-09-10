{-# LANGUAGE OverloadedStrings #-}

-- | Solver entry points and the work-queue loop. The order of entailment,
-- contour merging, insertion and activation is explicit in the expression
-- tree and independent of source layout.
module Fixen.CodeGen.Haskell.Solver (codeGenLoopAndSolve, codeGenReSolve) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Bindings (numberedName)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST
import Fixen.IR.RuleForest
import Fixen.Monad

codeGenLoopAndSolve :: CodeGenOptions -> FixenPass CodeGenState Text
codeGenLoopAndSolve options = do
  phases <- fixenGetPhases
  let phased = length phases > 1
      stateType = Hs.typ (if phased then "Interpretation" else "Database")
      stateName = if phased then "i" else "db"
      bind n = Hs.Binding (Hs.pat n)
      evaluation =
        if phased
          then
            [ bind "source_phase" (Hs.call "snd" [Hs.var "p"])
            , Hs.Binding (Hs.PTuple [Hs.pat "f", Hs.pat "target_phase"]) (Hs.call "evaluatePhased" [Hs.var "p"])
            , bind "db" (Hs.call "selectDb" [Hs.var "i", Hs.var "target_phase"])
            ]
          else [bind "f" (Hs.call "evaluate" [Hs.var "p"])]
      phaseValue =
        if phased
          then Hs.call "Just" [Hs.Infix (Hs.call "show" [Hs.var "source_phase"]) (Hs.name "++") (Hs.Infix (Hs.StringLit " -> ") (Hs.name "++") (Hs.call "show" [Hs.var "target_phase"]))]
          else Hs.var "Nothing"
      loopArgs = if phased then ["q'", "i"] else ["q'", "db"]
      rejected = traceCall "debugSolverRejected" [phaseValue, Hs.var "f"] (Hs.call "loop" (Hs.var <$> loopArgs))
      newFacts = Hs.call "filter" [Hs.Lambda (Hs.pat "candidate" :| []) (Hs.call "not" [Hs.call "|=" [Hs.var "db", Hs.var "candidate"]]), Hs.call "maximalContour" [Hs.var "c"]]
      insertions =
        [ bind "c" (Hs.call "mergeContour" [Hs.var "f", Hs.var "db"])
        , bind "new_facts" newFacts
        , bind "new_db" (Hs.call "foldl'" [Hs.var "insertToDb", Hs.var "db", Hs.var "new_facts"])
        ]
          ++ [bind "new_int" (Hs.call "replaceDb" [Hs.var "i", Hs.var "new_db", Hs.var "target_phase"]) | phased]
      stepArgs = if phased then ["new_int", "new_facts", "target_phase", "q'"] else ["new_db", "new_facts", "q'"]
      continuation = Hs.call "loop" [Hs.call "stepAll" (Hs.var <$> stepArgs), Hs.var (if phased then "new_int" else "new_db")]
      accepted = Hs.Let (NonEmpty.fromList insertions) (traceCall "debugSolverAccepted" [phaseValue, Hs.var "f", Hs.var "new_facts"] continuation)
      process = Hs.Let (NonEmpty.fromList evaluation) (Hs.If (Hs.call "|=" [Hs.var "db", Hs.var "f"]) rejected accepted)
      body =
        Hs.Case
          (Hs.call "Q.maxView" [Hs.var "q"])
          ((Hs.PCon (Hs.name "Just") [Hs.PTuple [Hs.pat "p", Hs.pat "q'"]], process) :| [(Hs.PCon (Hs.name "Nothing") [], Hs.var stateName)])
  pure $
    Hs.renderDecls
      [ Hs.Signature (Hs.name "loop") (Hs.functionType [Hs.typ "Queue", stateType] stateType)
      , Hs.Function (Hs.name "loop") [Hs.pat "q", Hs.pat stateName] body
      , Hs.Signature (Hs.name "solve") (Hs.functionType [Hs.TyList (Hs.typ "Fact")] stateType)
      , Hs.Function (Hs.name "solve") [] (Hs.call "reSolve" [Hs.var (if phased then "emptyInterpretation" else "emptyDb")])
      ]
  where
    traceCall function args continuation
      | codeGenDebug options = Hs.call function (args ++ [continuation])
      | otherwise = continuation

codeGenReSolve :: NonEmpty RuleForest -> FixenPass CodeGenState Text
codeGenReSolve forests = do
  let phased = NonEmpty.length forests > 1
      stateType = Hs.typ (if phased then "Interpretation" else "Database")
      stateName = if phased then "i" else "db"
      initialFact = tag (NonEmpty.length forests - 1) (Hs.call "Init" [Hs.var "fact"])
      initial = Hs.call "map" [Hs.Lambda (Hs.pat "fact" :| []) initialFact, Hs.var "f"]
      seeds = [seed phase leaf | (phase, forest) <- zip [0 ..] (NonEmpty.toList forests), leaf <- _ruleForestLeaves forest]
      queue = Hs.call "Q.fromList" [Hs.call "concat" [Hs.List (initial : seeds)]]
      body = Hs.Let (Hs.Binding (Hs.pat "q") queue :| []) (Hs.call "loop" [Hs.var "q", Hs.var stateName])
      tag phase fact
        | phased = Hs.Tuple [fact, Hs.Var (numberedName "Phase" phase)]
        | otherwise = fact
      seed phase leaf =
        let conclusion = _ruleLeafConclusion leaf
            fact = Hs.call (simpleIdentifier (relationLikeName conclusion)) (lowerExpr <$> relationLikeArgs conclusion)
            guards = Hs.guardStmt . lowerExpr . conditionExpr <$> _ruleLeafCondition leaf
         in Hs.Do guards (Hs.List [tag phase (Hs.call "Init" [fact])])
  pure $
    Hs.renderDecls
      [ Hs.Signature (Hs.name "reSolve") (Hs.functionType [stateType, Hs.TyList (Hs.typ "Fact")] stateType)
      , Hs.Function (Hs.name "reSolve") [Hs.pat stateName, Hs.pat "f"] body
      ]
