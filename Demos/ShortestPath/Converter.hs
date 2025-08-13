{-# LANGUAGE OverloadedStrings #-}

{- |
The expected JSON shape is a nested object-of-objects:

@
{
  "<u>": {
    "<v1>": { "weight": <natural> },
    "<v2>": { "weight": <natural> }
  },
  "<u2>": {
    "<v3>": { "weight": <natural> }
  }
}
@

* Outer keys are **source vertex IDs** (JSON strings).
* Inner keys are **destination vertex IDs** (JSON strings).
* Each edge payload is an object with a single key, @\"weight\"@, whose
  value is a non-negative integer (parsed as 'Natural').

Example:

@
{
  "0": { "1": {"weight": 2}, "2": {"weight": 1} },
  "1": { "2": {"weight": 1} },
  "2": { "0": {"weight": 1} }
}
@
-}

module ShortestPath.Converter
  ( Weight (..)
  , Graph
  , loadGraph
  , convertToKleen
  , convertToHandwritten
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Numeric.Natural

import qualified ShortestPath.ShortestPath as SP
import qualified ShortestPath.Dist as D
import qualified ShortestPath.HandwrittenDijkstra as HW

newtype Weight = Weight { weight :: Natural } deriving (Show)

instance FromJSON Weight where
  parseJSON = withObject "Weight" $ \o ->
    Weight <$> o .: "weight"

-- Graph type: node -> (node -> weight)
type Graph = M.Map String (M.Map String Weight)

loadGraph :: FilePath -> IO (Either String Graph)
loadGraph fp = do
  -- Read the contents of the JSON file
  bs <- B.readFile fp
  return $ eitherDecode bs

convertToKleen :: Graph -> [SP.Fact]
convertToKleen g =
  let edgeFacts =
        [ SP.EdgeFact (SP.Edge u v (D.DistNat w))
        | (u, nbrs) <- M.toList g
        , (v, Weight w) <- M.toList nbrs
        ]
  in edgeFacts ++ [SP.StartFact (SP.Start "0")]

convertToHandwritten :: Graph -> M.Map HW.Vertex [(HW.Vertex, HW.Dist)]
convertToHandwritten =
  M.map (map toAdj . M.toList)
  where
    toAdj :: (String, Weight) -> (HW.Vertex, HW.Dist)
    toAdj (v, Weight w) = (v, HW.Dist (fromIntegral w))
