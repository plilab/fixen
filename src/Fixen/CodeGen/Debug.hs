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
) where

import Data.Text (Text, pack)
import Data.Text qualified as Text
import Fixen.CodeGen.Common

-- | Generates the import required by runtime debug traces.
codeGenDebugImport :: CodeGenOptions -> Text
codeGenDebugImport options
  | codeGenDebug options =
      "\nimport Debug.Trace (traceM)"
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
          "[Fixen] [Step]"
          ++ maybe "" (\\n -> " [Phase " ++ show n ++ "]") phase_number
          ++ " premise "
          ++ show premise
          ++ ", rule "
          ++ rule_name
          ++ " activated, candidate "
          ++ show (evaluate rule_instance)
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

-- | Renders text as an escaped Haskell String literal.
haskellStringLiteral :: Text -> Text
haskellStringLiteral = Text.pack . show . Text.unpack
