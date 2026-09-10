{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fact declarations and semantic comparisons. Generated expressions use the
-- Haskell output AST; the fixed maximal-contour algorithm is a source template.
module Fixen.CodeGen.Fact where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Common
import Fixen.CodeGen.Haskell.Bindings (numberedName)
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.RelationRepresentation

codeGenFacts :: RelationRepresentation -> Text
codeGenFacts layouts =
  Text.intercalate
    "\n\n"
    [ "----- FACTS -----\n"
        <> Hs.renderDecls
          [Hs.Data (Hs.name "Fact") (NonEmpty.fromList (factConstructor <$> Map.toList layouts)) (Hs.name <$> ["Show", "Eq"])]
    , codeGenFactLeq layouts
    , codeGenMaximalContour
    ]

factConstructor :: (Text, RelationRepresentationInfo) -> Hs.Constructor
factConstructor (rel, layout) =
  Hs.Constructor (Hs.name rel) (lowerType . snd <$> _factTypes (_factRepresentation layout))

codeGenFact :: (Text, RelationRepresentationInfo) -> Text
codeGenFact = Hs.renderInline . Hs.prettyConstructor . factConstructor

codeGenFactLeq :: RelationRepresentation -> Text
codeGenFactLeq layouts =
  Hs.renderDecls $
    Hs.Signature (Hs.name "factLeq") (Hs.functionType [Hs.typ "Fact", Hs.typ "Fact"] (Hs.typ "Bool"))
      : (factLeqCase <$> Map.toList layouts)
      ++ [Hs.Function (Hs.name "factLeq") [Hs.PWildcard, Hs.PWildcard] (Hs.var "False") | Map.size layouts /= 1]

codeGenFactLeqCase :: (Text, RelationRepresentationInfo) -> Text
codeGenFactLeqCase = Hs.renderDecls . pure . factLeqCase

factLeqCase :: (Text, RelationRepresentationInfo) -> Hs.Decl
factLeqCase (rel, layout) =
  let fields = fst <$> _factTypes (_factRepresentation layout)
      left = [numberedName "v" i | i <- [0 .. length fields - 1]]
      right = [numberedName "v'" i | i <- [0 .. length fields - 1]]
      comparisons = zipWith3 (\q a b -> Hs.apps (Hs.Var (comparisonName q)) [Hs.Var a, Hs.Var b]) fields left right
   in Hs.Function
        (Hs.name "factLeq")
        [Hs.PCon (Hs.name rel) (Hs.PVar <$> left), Hs.PCon (Hs.name rel) (Hs.PVar <$> right)]
        (Hs.andExpr comparisons)

comparisonName :: QueryType -> Hs.Name
comparisonName Match = Hs.name "=="
comparisonName (Meet leq _) = lowerName leq
comparisonName (LatticeMeet leq _ _) = lowerName leq

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
