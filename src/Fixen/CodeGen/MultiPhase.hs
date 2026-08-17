{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.CodeGen.MultiPhase
-- Description : Code Generation for Supporting Definitions for Multi-Phase Fixen Programs
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities the definitions
-- that are required for multi-phase Fixen programs.
--
-- The 'codeGenMultiPhase' function generates:
--
-- 1. The @Interpretation@ definition ('codeGenInterpretationDef')
-- 2. The @emptyInterpretation@ definition ('codeGenEmptyInterpretation')
-- 3. The @Phases@ definition ('codeGenPhasesDef')
-- 4. The @nextPhase@ function definition ('codeGenNextPhaseDef')
-- 5. The @selectDb@ function definition ('codeGenSelectDbDef')
-- 6. The @||=@ function definition ('codeGenInterpretationEntailment')
-- 7. The @replaceDb@ function definition ('codeGenReplaceDb')
-- 8. The @insertToInterpretation@ function definition
--    ('codeGenInsertToInterpretation')
-- 9. The @evaluatePhased@ function definition ('codeGenEvaluatePhased')
--
-- @since 26.7
module Fixen.CodeGen.MultiPhase where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.Monad
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Generates the Haskell source code for dealing with multi-phase Fixen
-- programs.
--
-- @since 26.7
codeGenMultiPhase :: FixenPass CodeGenState Text
codeGenMultiPhase = do
  num_phases <- NonEmpty.length <$> fixenGetPhases
  return $
    if num_phases == 1
      then ""
      else
        Text.intercalate
          "\n\n"
          [ codeGenInterpretationDef num_phases
          , codeGenEmptyInterpretation num_phases
          , codeGenPhasesDef num_phases
          , codeGenNextPhaseDef num_phases
          , codeGenSelectDbDef num_phases
          , codeGenInterpretationEntailment
          , codeGenReplaceDb num_phases
          , codeGenInsertToInterpretation
          , codeGenEvaluatePhased
          ]

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- ** Interpretation Type

-- | Generates the @Interpretation@ type alias.
--
-- @since 26.7
codeGenInterpretationDef :: Int -> Text
codeGenInterpretationDef n =
  Text.concat
    [ "----- MULTI-PHASE FIXEN PROGRAM DEFINITIONS -----\n"
    , "type Interpretation = "
    , parenthesize $ Text.intercalate ", " (replicate n "Database")
    ]

-- ** Empty Interpretations

-- | Generates the @emptyInterpretation@ definition.
--
-- @since 26.7
codeGenEmptyInterpretation :: Int -> Text
codeGenEmptyInterpretation n =
  Text.concat
    [ "emptyInterpretation :: Interpretation\n"
    , "emptyInterpretation = "
    , parenthesize $ Text.intercalate ", " $ replicate n "emptyDb"
    ]

-- ** Phases

-- | Generates the @Phases@ type.
--
-- @since 26.7
codeGenPhasesDef :: Int -> Text
codeGenPhasesDef n =
  Text.concat
    [ "data Phase = "
    , ( Text.intercalate
          "\n           | "
          (Text.append "Phase" . Text.show <$> [0 .. n - 1])
      )
    , "\n deriving (Eq, Show, Ord)"
    ]

-- | Generates the @nextPhase@ function
--
-- @since 26.7
codeGenNextPhaseDef :: Int -> Text
codeGenNextPhaseDef n =
  Text.append "nextPhase :: Phase -> Phase\n" $
    Text.intercalate "\n" $
      fmap
        ( \i ->
            Text.concat
              [ "nextPhase Phase"
              , Text.show i
              , " = Phase"
              , Text.show (mod (i + 1) n)
              ]
        )
        [0 .. n - 1]

-- ** Selecting Databases

-- | Generates the @selectDb@ function.
--
-- @since 26.7
codeGenSelectDbDef :: Int -> Text
codeGenSelectDbDef n =
  Text.append "selectDb :: Interpretation -> Phase -> Database\n" $
    Text.intercalate "\n" $
      (`Text.append` " = db") ∘ selectLhs n <$> [0 .. n - 1]
  where
    -- selectDb (_, _, ..., _, db, _, ..., _) Phase{i} = ...
    selectLhs n' i =
      Text.concat
        [ "selectDb ("
        , Text.intercalate ", " $
            (replicate i "_") ++ ("db" : replicate (n' - i - 1) "_")
        , ") Phase"
        , Text.show i
        ]

-- ** Intepretation Entailment

-- | Generates the @||=@ function.
--
-- @since 26.7
codeGenInterpretationEntailment :: Text
codeGenInterpretationEntailment =
  """
  (||=) :: Interpretation -> Fact -> Phase -> Bool
  (i ||= f) p = selectDb i p |= f

  infix 1 ||=
  """

-- ** Database Replacement

-- | Generates the @replaceDb@ function.
--
-- @since 26.7
codeGenReplaceDb :: Int -> Text
codeGenReplaceDb n =
  Text.append "replaceDb :: Interpretation -> Database -> Phase -> Interpretation\n" $
    Text.intercalate "\n" $
      replaceCase n <$> [0 .. n - 1]
  where
    replaceCase n' i =
      Text.concat
        [ "replaceDb "
        , replaceLhsTup n' i
        , " db' Phase"
        , Text.show i
        , " = "
        , replaceRhs n' i
        ]

    -- (db1, db2, ..., dbi-1, _, dbi+1, ..., dbn)
    replaceLhsTup n' i =
      let components = [if i == i' then "_" else db i' | i' <- [0 .. n' - 1]]
       in parenthesize $ Text.intercalate ", " components

    -- (db1, db2, ..., dbi-1, db', dbi+1, ..., dbn)
    replaceRhs n' i =
      let components = [if i == i' then "db'" else db i' | i' <- [0 .. n' - 1]]
       in parenthesize $ Text.intercalate ", " components

    db :: Int -> Text
    db = Text.append "db" ∘ Text.show

-- ** Fact Insertion into Intepretations

-- | Generates the @insertToInterpretation@ function.
--
-- @since 26.7
codeGenInsertToInterpretation :: Text
codeGenInsertToInterpretation =
  """
  insertToInterpretation :: Interpretation -> Fact -> Phase -> Interpretation
  insertToInterpretation i f p = do
    let db = selectDb i p
        db' = insertToDb db f
     in replaceDb i db' p
  """

-- ** Phased Rule-Instance Evaluation

-- | Generates the @evaluatePhased@ function.
--
-- @since 26.7
codeGenEvaluatePhased :: Text
codeGenEvaluatePhased =
  """
  evaluatePhased :: (RuleInstance, Phase) -> (Fact, Phase)
  evaluatePhased (r, p) = (evaluate r, nextPhase p)
  """
