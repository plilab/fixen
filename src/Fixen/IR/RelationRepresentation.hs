-- |
-- Module      : Fixen.IR.RelationRepresentation
-- Description : Definitions for Relation Representations
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module contains definitions for working with relation representations.
--
-- Fixen-generated facts are exactly as declared by the user (sort of), so that
-- the interface for working with them is unsurprising. However, internally,
-- the fact database has a different layout than what is declared in the
-- relation declaration. This obviously affects how insertions, queries,
-- matches, etc., are handled too.
--
-- A 'RelationRepresentation' thus captures how relations should be stored in
-- the fact database, and we use this information during code generation to set
-- up the database and interfaces nicely.
--
-- A small caveat is that the types of the arguments to facts (and the database
-- elements) are not __exactly__ as described by the user; partially ordered
-- types do __not__ create, say, @newtype@s and @instance PartialOrd ...@. As
-- such, the types of the arguments are given by their __underlying type__, i.e.,
-- the @type = ...@ of a @partial ord@ declaration. Of course, if the argument
-- is discrete, the type is written as-is.
--
-- @since 0.0.1
module Fixen.IR.RelationRepresentation where

import Data.IntMap.Strict
import Data.Map.Strict
import Data.Text
import Fixen.Fields
import Fixen.IR.AST

--------------------------------------------------------------------------------

-- * Main Types

--------------------------------------------------------------------------------

-- | A 'RelationRepresentation' is just a map from relation names to
-- 'RelationRepresentationInfo's.
--
-- @since 0.0.1
type RelationRepresentation = Map Text RelationRepresentationInfo

-- | A 'RelationRepresentationInfo' captures information about how
-- facts ('Fact') are represented in the database ('Database').
--
-- @since 0.0.1
data RelationRepresentationInfo = RelationRepresentationInfo
  { _databaseRepresentation :: Database
  -- ^ The database layout
  --
  -- @since 0.0.1
  , _factRepresentation :: Fact
  -- ^ The fact layout
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasDatabase RelationRepresentationInfo Database where
  database = lens _databaseRepresentation (\s i -> s {_databaseRepresentation = i})

instance HasFact RelationRepresentationInfo Fact where
  fact = lens _factRepresentation (\s i -> s {_factRepresentation = i})

-- TODO: Seriously look at how _extractionMap and _insertionMap is used in
-- 'codeGen' and rethink the naming.

-- | Describes how a relation is stored in the fact database.
--
-- /Invariants for '_databaseTypes'/:
--
-- * If the \(i^\text{th}\) element has 'StoreType' as 'StoredAsHashSet', then
--   for all \(j > i \), the \(j^\text{th}\) element will also have 'StoreType'
--   as 'StoredAsHashSet'.
-- * The last element of the list is guaranteed to have 'StoredAsHashSet' as
--   the 'StoreType'.
-- * If the \(i^\text{th}\) element has 'QueryType' as 'Meet', then
--   for all \(j > i \), the \(j^\text{th}\) element will also have 'QueryType'
--   as 'Meet'.
-- * If an element has 'QueryType' as 'Meet', then its 'StoreType' will be
--   'StoredAsHashSet'.
-- * The length of the list is equal to the arity of the relation.
--
-- @since 0.0.1
data Database = Database
  { _databaseTypes :: [(QueryType, StoreType, Type)]
  -- ^ The database types. Note that the 'Type' in each
  -- tuple is the __underlying__ type.
  --
  -- @since 0.0.1
  , _extractionMap :: IntMap Int
  -- ^ A map from fact argument positions to database positions.
  -- For instance, if the \(i^\text{th}\) fact argument corresponds to the
  -- \(j^\text{th}\) argument in the database, then the map has the entry \(i \to j\).
  --
  -- This is the inverse of '_insertionMap'.
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasTypes Database [(QueryType, StoreType, Type)] where
  types = lens _databaseTypes (\s i -> s {_databaseTypes = i})

instance HasMap Database (IntMap Int) where
  map = lens _extractionMap (\s i -> s {_extractionMap = i})

-- | Describes how a fact is laid out.
--
-- /Invariants for '_factTypes'/:
--
-- * If the \(i^\text{th}\) element has 'QueryType' as 'Meet', then
--   for all \(j > i \), the \(j^\text{th}\) element will also have 'QueryType'
--   as 'Meet'.
-- * The length of the list is equal to the arity of the relation.
--
-- @since 0.0.1
data Fact = Fact
  { _factTypes :: [(QueryType, Type)]
  -- ^ The fact types. Note that the 'Type' in each tuple is the __underlying__
  -- type.
  --
  -- @since 0.0.1
  , _insertionMap :: IntMap Int
  -- ^ A map from database positions to fact positions.
  -- For instance, if the \(i^\text{th}\) fact argument corresponds to the
  -- \(j^\text{th}\) argument in the database, then the map has the entry \(j \to i\).
  --
  -- This is the inverse of '_extractionMap'.
  --
  -- @since 0.0.1
  }
  deriving (Show, Eq)

instance HasTypes Fact [(QueryType, Type)] where
  types = lens _factTypes (\s i -> s {_factTypes = i})

instance HasMap Fact (IntMap Int) where
  map = lens _insertionMap (\s i -> s {_insertionMap = i})

-- | Describes how a type is stored in the database.
--
-- @since 0.0.1
data StoreType = StoredAsHashMap | StoredAsHashSet | StoredAsSingleton
  deriving (Show, Eq)

-- | Describes how you would match against an argument.
--
-- @since 0.0.1
data QueryType
  = -- | Discrete; therefore just a simple match, i.e., HashMap lookup or
    -- equality comparisons
    --
    -- @since 0.0.1
    Match
  | -- | Partially ordered, i.e., set lookup guarded by leq, or
    -- matched via mlbs function
    --
    -- @since 0.0.1
    Meet
      Identifier
      -- ^ The leq function
      Identifier
      -- ^ The mlbs function
  | -- | Partially ordered in a lattice, i.e., set lookup guarded by leq, or
    -- matched via meet function
    --
    -- @since 0.0.1
    LatticeMeet
      Identifier
      -- ^ The leq function
      Identifier
      -- ^ The join function
      Identifier
      -- ^ The meet function
      Identifier
      -- ^ The bot function
  deriving (Show, Eq)
