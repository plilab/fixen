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
module Pareto.Converter (
  Weight (..),
  Graph,
  loadGraph,
  convertToHand,
  convertToFixenNoPriorities,
  convertToFixenWithPriorities,
) where

import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.HashMap.Strict qualified as H
import Data.Map.Strict qualified as M
import Numeric.Natural
import Pareto.FixenNoPriorities qualified as NoPriorities
import Pareto.FixenWithPriorities qualified as WithPriorities

data Weight = Weight {distance :: Natural, time :: Natural} deriving (Show)

instance FromJSON Weight where
  parseJSON = withObject "Weight" $ \o ->
    Weight <$> o .: "distance" <*> o .: "time"

-- Graph type: node -> (node -> weight)
type Graph = M.Map String (M.Map String Weight)

loadGraph :: FilePath -> IO (Either String Graph)
loadGraph fp = do
  -- Read the contents of the JSON file
  bs <- B.readFile fp
  return $ eitherDecode bs

convertToHand :: Graph -> H.HashMap String [(String, Natural, Natural)]
convertToHand =
  H.fromList . M.toList . M.map (map toAdj . M.toList)
  where
    toAdj :: (String, Weight) -> (String, Natural, Natural)
    toAdj (v, Weight w x) = (v, fromIntegral w, fromIntegral x)

convertToFixenWithPriorities :: Graph -> WithPriorities.Database
convertToFixenWithPriorities g =
  let edgeFacts =
        [ WithPriorities.Edge u v w x
        | (u, nbrs) <- M.toList g
        , (v, Weight w x) <- M.toList nbrs
        ]
   in WithPriorities.solve edgeFacts

convertToFixenNoPriorities :: Graph -> NoPriorities.Database
convertToFixenNoPriorities g =
  let edgeFacts =
        [ NoPriorities.Edge u v w x
        | (u, nbrs) <- M.toList g
        , (v, Weight w x) <- M.toList nbrs
        ]
   in NoPriorities.solve edgeFacts
