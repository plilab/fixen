{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
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
-- @since 26.7
module Fixen.CodeGen.Fact where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.Fields
import Fixen.IR.AST
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
-- @since 26.7
codeGenFacts :: RelationRepresentation -> Text
codeGenFacts r =
  let header :: Text = "----- FACTS -----\ndata Fact = "
      facts = Map.toList r
   in Text.concat
        [ header
        , Text.intercalate "\n          | " (codeGenFact <$> facts)
        , "\n  deriving (Show, Eq)\n\n"
        , codeGenFactLeq r
        , "\n\n"
        , codeGenMaximalContour
        ]

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Generates a single alternative in the @Fact@ type. These are individual
-- relations in the Fixen program.
--
-- @since 26.7
codeGenFact
  -- | The lhs is the name of the relation, the rhs is the information about the
  -- relation
  --
  -- @since 26.7
  :: (Text, RelationRepresentationInfo)
  -> Text
codeGenFact (t, r) =
  let fact_rep = r ^. fact
      fact_ty = fact_rep ^. types <&> snd
      ty_code = codeGenTypeAsAtomic <$> fact_ty
   in Text.intercalate " " (t : ty_code)

codeGenFactLeq :: RelationRepresentation -> Text
codeGenFactLeq r =
  let header = "factLeq :: Fact -> Fact -> Bool"
      facts = Map.toList r
      fact_cases = codeGenFactLeqCase <$> facts
   in if length fact_cases == 1
        then Text.intercalate "\n" $ header : fact_cases
        else Text.intercalate "\n" $ (header : fact_cases) ++ ["factLeq _ _ = False"]

codeGenFactLeqCase :: (Text, RelationRepresentationInfo) -> Text
codeGenFactLeqCase (t, r) =
  let fact_rep = r ^. fact
      fact_ty = fact_rep ^. types <&> fst
      fact_leqs =
        ( \case
            Match -> MkIdentifierSimple (-1) "=="
            Meet l _ -> l
            LatticeMeet l _ _ -> l
        )
          <$> fact_ty
      pattern_lhs = parenthesize $ Text.intercalate " " $ t : (v <$> [0 .. length fact_ty - 1])
      pattern_rhs = parenthesize $ Text.intercalate " " $ t : (v' <$> [0 .. length fact_ty - 1])
      body_atoms =
        ( \i ->
            ExprApp
              (-1)
              ( ExprApp
                  (-1)
                  (ExprVar (-1) (fact_leqs !! i))
                  (ExprVar (-1) (MkIdentifierSimple (-1) (v i)))
              )
              (ExprVar (-1) (MkIdentifierSimple (-1) (v' i)))
        )
          <$> [0 .. length fact_ty - 1]
      body = if null body_atoms then "True" else Text.intercalate " && " $ asAtomic <$> body_atoms
   in Text.intercalate " " ["factLeq", pattern_lhs, pattern_rhs, "=", body]
  where
    v i = Text.concat ["v", Text.show i]
    v' i = Text.concat ["v'", Text.show i]

codeGenMaximalContour :: Text
codeGenMaximalContour =
  """
  maximalContour :: [Fact] -> [Fact]
  maximalContour [] = []
  maximalContour [x] = [x]
  maximalContour (x : xs) =
    if any (factLeq x) xs 
    then maximalContour xs
    else x : maximalContour (filter (\\x' -> not (factLeq x' x)) xs)
  """
