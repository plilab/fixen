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
  , convertToNoPriorityKleen
  , convertToHandwritten
  ) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Numeric.Natural

import qualified ShortestPath.ShortestPath as SP
import qualified ShortestPath.ShortestPathNoPriority as SPNP
import qualified ShortestPath.Dist as D
import qualified ShortestPath.HandwrittenCommon as HC

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
  in SP.StartFact (SP.Start "0"):edgeFacts

convertToNoPriorityKleen :: Graph -> [SPNP.Fact]
convertToNoPriorityKleen g =
  let edgeFacts =
        [ SPNP.EdgeFact (SPNP.Edge u v (D.DistNat w))
        | (u, nbrs) <- M.toList g
        , (v, Weight w) <- M.toList nbrs
        ]
  in SPNP.StartFact (SPNP.Start "0"):edgeFacts

convertToHandwritten :: Graph -> M.Map HC.Vertex [(HC.Vertex, HC.Dist)]
convertToHandwritten =
  M.map (map toAdj . M.toList)
  where
    toAdj :: (String, Weight) -> (HC.Vertex, HC.Dist)
    toAdj (v, Weight w) = (v, HC.Dist (fromIntegral w))
