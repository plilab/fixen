{-# LANGUAGE OverloadedStrings #-}

-- |
-- The expected JSON shape is a nested object-of-objects:
--
-- @
-- {
--   "<u>": {
--     "<v1>": { "weight": <natural> },
--     "<v2>": { "weight": <natural> }
--   },
--   "<u2>": {
--     "<v3>": { "weight": <natural> }
--   }
-- }
-- @
--
-- * Outer keys are **source vertex IDs** (JSON strings).
-- * Inner keys are **destination vertex IDs** (JSON strings).
-- * Each edge payload is an object with a single key, @\"weight\"@, whose
--   value is a non-negative integer (parsed as 'Natural').
--
-- Example:
--
-- @
-- {
--   "0": { "1": {"weight": 2}, "2": {"weight": 1} },
--   "1": { "2": {"weight": 1} },
--   "2": { "0": {"weight": 1} }
-- }
-- @
module ShortestPath.Converter (
  Weight (..),
  Graph,
  loadGraph,
  convertToHand,
  convertToFixenWithPriorities,
  convertToFixenNoPriorities,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as H
import Data.Map.Strict qualified as M
import Numeric.Natural
import ShortestPath.FixenNoPriorities qualified as NoPriorities
import ShortestPath.FixenWithPriorities qualified as WithPriorities

newtype Weight = Weight {weight :: Natural} deriving (Show)

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

convertToHand :: Graph -> H.HashMap String [(String, Natural)]
convertToHand =
  H.fromList . M.toList . M.map (map toAdj . M.toList)
  where
    toAdj :: (String, Weight) -> (String, Natural)
    toAdj (v, Weight w) = (v, fromIntegral w)

convertToFixenWithPriorities :: Graph -> WithPriorities.Database
convertToFixenWithPriorities g =
  let edgeFacts =
        [ WithPriorities.Edge u v w
        | (u, nbrs) <- M.toList g
        , (v, Weight w) <- M.toList nbrs
        ]
   in WithPriorities.solve edgeFacts

convertToFixenNoPriorities :: Graph -> NoPriorities.Database
convertToFixenNoPriorities g =
  let edgeFacts =
        [ NoPriorities.Edge u v w
        | (u, nbrs) <- M.toList g
        , (v, Weight w) <- M.toList nbrs
        ]
   in NoPriorities.solve edgeFacts
