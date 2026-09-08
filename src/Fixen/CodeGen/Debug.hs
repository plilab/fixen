{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.CodeGen.Debug
-- Description : Code generation for solver debug traces
--
-- @since 26.8
module Fixen.CodeGen.Debug (
  codeGenDebugImport,
  codeGenDebugDefinitions,
  codeGenRuleActivation,
  codeGenSolverAcceptance,
  codeGenSolverRejection,
) where

import Data.Text (Text, pack)
import Data.Text qualified as Text
import Fixen.CodeGen.Common

-- | Generates the import required by runtime debug traces.
codeGenDebugImport :: CodeGenOptions -> Text
codeGenDebugImport options
  | codeGenDebug options =
      "\nimport Debug.Trace (trace, traceM)"
  | otherwise = ""

-- | Generates runtime helpers used by instrumented solver code.
codeGenDebugDefinitions :: CodeGenOptions -> Text
codeGenDebugDefinitions options
  | codeGenDebug options =
      """
      debugRuleActivation
        :: Applicative f
        => Fact
        -> Maybe Int
        -> String
        -> RuleInstance
        -> f ()
      debugRuleActivation premise phase_number rule_name rule_instance =
        traceM $
          "\\ESC[33m[Fixen] [Step]\\ESC[0m"
          ++ maybe "" (\\n -> " [Phase " ++ show n ++ "]") phase_number
          ++ " \\ESC[32mPremise\\ESC[0m "
          ++ show premise
          ++ ", \\ESC[32mRule\\ESC[0m \\ESC[31m"
          ++ rule_name
          ++ "\\ESC[0m activated, candidate: "
          ++ show (evaluate rule_instance)

      debugSolverRejected :: Fact -> a -> a
      debugSolverRejected candidate =
        trace $
          "\\ESC[33m[Fixen] [Solver]\\ESC[0m \\ESC[31mSubsumed\\ESC[0m candidate "
          ++ show candidate

      debugSolverAccepted :: Fact -> [Fact] -> a -> a
      debugSolverAccepted candidate accepted_facts =
        trace $
          "\\ESC[33m[Fixen] [Solver]\\ESC[0m \\ESC[32mProcessed\\ESC[0m candidate "
          ++ show candidate
          ++ "; \\ESC[32mInserted Facts\\ESC[0m: "
          ++ show accepted_facts
      """
  | otherwise = ""

-- | Generates a call to the runtime rule-activation logger.
codeGenRuleActivation
  :: CodeGenOptions
  -> Maybe Int
  -> Text
  -> Text
  -> Text
codeGenRuleActivation options phase_number indentation rule_name
  | codeGenDebug options =
      Text.concat
        [ indentation
        , "debugRuleActivation fact "
        , pack $ "(" ++ show phase_number ++ ") "
        , haskellStringLiteral rule_name
        , " rule_instance"
        ]
  | otherwise = ""

-- | Wraps a solver continuation with an accepted-fact debug trace.
codeGenSolverAcceptance
  :: CodeGenOptions
  -> Text
  -> Text
  -> Text
  -> Text
codeGenSolverAcceptance options candidate accepted_facts continuation
  | codeGenDebug options =
      Text.concat
        [ "debugSolverAccepted "
        , candidate
        , " "
        , accepted_facts
        , " $ "
        , continuation
        ]
  | otherwise = continuation

-- | Wraps a solver continuation with a rejected-fact debug trace.
codeGenSolverRejection
  :: CodeGenOptions
  -> Text
  -> Text
  -> Text
codeGenSolverRejection options candidate continuation
  | codeGenDebug options =
      Text.concat
        [ "debugSolverRejected "
        , candidate
        , " $ "
        , continuation
        ]
  | otherwise = continuation

-- | Renders text as an escaped Haskell String literal.
haskellStringLiteral :: Text -> Text
haskellStringLiteral = Text.pack . show . Text.unpack
