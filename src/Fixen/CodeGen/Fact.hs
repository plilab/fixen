{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.CodeGen.Fact
-- Description : Code Generation for Facts
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities for facts.
--
-- @since 0.0.1
module Fixen.CodeGen.Fact where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.Fields
import Fixen.IR.RelationRepresentation

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Generates the @Fact@ datatype in the Haskell program:
--
-- @
-- data Fact = ...
--   deriving (Show, Eq)
-- @
--
-- @since 0.0.1
codeGenFacts :: RelationRepresentation -> Text
codeGenFacts r =
  let header :: Text = "----- FACTS -----\ndata Fact = "
      facts = Map.toList r
   in Text.concat
        [ header
        , Text.intercalate "\n          | " (codeGenFact <$> facts)
        , "\n  deriving (Show, Eq)"
        ]

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Generates a single alternative in the @Fact@ type. These are individual
-- relations in the Fixen program.
--
-- @since 0.0.1
codeGenFact
  :: (Text, RelationRepresentationInfo)
  -- ^ The lhs is the name of the relation, the rhs is the information about the
  -- relation
  --
  -- @since 0.0.1
  -> Text
codeGenFact (t, r) =
  let fact_rep = r ^. fact --  _factRepresentation r
      fact_ty = fact_rep ^. types <&> snd
      ty_code = codeGenTypeAsAtomic <$> fact_ty
   in Text.intercalate " " (t : ty_code)
