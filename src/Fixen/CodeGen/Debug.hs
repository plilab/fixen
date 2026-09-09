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
      """\n
      import Debug.Trace (trace, traceM)
      import Data.Text (Text)
      """
  | otherwise = ""

codeGenDebugColors :: CodeGenOptions -> Text
codeGenDebugColors options =
  """
  debugColors :: Bool
  debugColors = 
  """
    <> (pack . show $ debugColor options)
    <> """\n
       red, green, yellow, reset :: [Char]
       red = "\\ESC[31m"
       green = "\\ESC[32m"
       yellow = "\\ESC[33m"
       reset = "\\ESC[0m"

       applyColor :: [Char] -> [Char] -> [Char]
       applyColor color t =
         let color' = if debugColors then color else ""
             reset' = if debugColors then reset else ""
          in color' ++ t ++ reset'

       applyRed :: [Char] -> [Char]
       applyRed = applyColor red

       applyYellow :: [Char] -> [Char]
       applyYellow = applyColor yellow

       applyGreen :: [Char] -> [Char]
       applyGreen = applyColor green

       """

-- | Generates runtime helpers used by instrumented solver code.
codeGenDebugDefinitions :: CodeGenOptions -> Text
codeGenDebugDefinitions options
  | codeGenDebug options =
      codeGenDebugColors options
        <> """
           debugRuleActivation
             :: Applicative f
             => Fact
             -> Maybe Int
             -> String
             -> RuleInstance
             -> f ()
           debugRuleActivation premise phase_number rule_name rule_instance =
             traceM $
               applyYellow ("[Fixen] [Step] " ++ maybe "" (\\n -> "[Phase " ++ show n ++ "] ") phase_number)
               ++ applyGreen "Premise "
               ++ show premise
               ++ ", "
               ++ applyGreen "Rule "
               ++ applyRed rule_name
               ++ " activated, candidate: "
               ++ show (evaluate rule_instance)

           debugSolverRejected :: Fact -> a -> a
           debugSolverRejected candidate =
             trace $
               applyYellow "[Fixen] [Solver]"
               ++ applyRed " Subsumed"
               ++ " candidate "
               ++ show candidate

           debugSolverAccepted :: Fact -> [Fact] -> a -> a
           debugSolverAccepted candidate accepted_facts =
             trace $
               applyYellow "[Fixen] [Solver]"
               ++ applyGreen " Processed"
               ++ " candidate "
               ++ show candidate
               ++ "\\n"
               ++ applyYellow "[Fixen] [Solver]"
               ++ applyGreen " Inserted Facts"
               ++ ": "
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
