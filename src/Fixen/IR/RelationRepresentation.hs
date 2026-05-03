module Fixen.IR.RelationRepresentation where

import Data.IntMap.Strict
import Data.Map.Strict
import Data.Text
import Fixen.IR.AST

type RelationRepresentation = Map Text RelationRepresentationInfo

data RelationRepresentationInfo = RelationRepresentationInfo
  { _databaseRepresentation :: Database
  , _factRepresentation :: Fact
  }
  deriving (Show, Eq)

data Database = Database
  { _databaseTypes :: [(QueryType, Type)]
  , _extractionMap :: IntMap Int -- how to take a fact out of the DB. To insert, reverse the arrows.
  }
  deriving (Show, Eq)

data QueryType
  = Match
  | -- | mlbs function
    Meet Identifier Identifier
  deriving (Show, Eq)

data Fact = Fact
  { _factTypes :: [Type]
  , _insertionMap :: IntMap Int -- how to take a fact and insert into DB. To take out, reverse the arrows.
  }
  deriving (Show, Eq)
