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
) where

import Data.Text (Text, pack)
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
               ++ case evaluate rule_instance of
                    [fact] -> " activated, candidate: " ++ show fact
                    facts -> " activated, candidates: " ++ show facts

           debugSolverPhase :: Maybe String -> String
           debugSolverPhase =
             maybe "" (\\phase_name -> " [" ++ phase_name ++ "]")

           debugSolverRejected :: Maybe String -> Fact -> a -> a
           debugSolverRejected phase_name candidate =
             trace $
               applyYellow ("[Fixen] [Solver]" ++ debugSolverPhase phase_name)
               ++ applyRed " Subsumed"
               ++ " candidate "
               ++ show candidate

           debugSolverAccepted :: Maybe String -> Fact -> [Fact] -> a -> a
           debugSolverAccepted phase_name candidate accepted_facts =
             trace $
               applyYellow ("[Fixen] [Solver]" ++ debugSolverPhase phase_name)
               ++ applyGreen " Processed"
               ++ " candidate "
               ++ show candidate
               ++ "\\n"
               ++ applyYellow ("[Fixen] [Solver]" ++ debugSolverPhase phase_name)
               ++ applyGreen " Inserted Facts"
               ++ ": "
               ++ show accepted_facts
           """
  | otherwise = ""
