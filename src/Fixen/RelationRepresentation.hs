-- |
-- Module      : Fixen.RelationRepresentation
-- Description : Generating Relation Representations
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module defines how 'RelationRepresentation's are generated.
--
-- The idea behind this phase is to decide how facts should be stored. For
-- example, if we know that an argument to a relation is discrete, i.e., uses
-- the trivial partial order defined by <= = ==, then it makes more sense to
-- store that argument as HashMap keys. Furthermore, if a relation argument is
-- never matched, then it should be stored later in the nested HashMaps. For
-- this reason, we use this compiler pass to decide how facts should be laid
-- out in the database so that Fixen-generated programs have better runtime
-- performance.
--
-- @since 0.0.1
module Fixen.RelationRepresentation where

import Control.Lens
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Fixen.Fields
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.Monad
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | The state used for obtaining 'RelationRepresentation's.
--
-- @since 0.0.1
type RelationRepresentationState σ = WithSymbolEnv σ

-- | Obtains the representations for 'RelationDeclaration's in the 'Program'.
--
-- @since 0.0.1
getRelationRepresentation :: RelationRepresentationState σ => FixenPass σ RelationRepresentation
getRelationRepresentation = do
  rel_map <- fixenGetRelationInfo
  mapM getRepresentation rel_map

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Gets the \"ideal\" representation of a relation. Facts of the relation are
-- stored as:
--
-- * Discrete arguments that are matched in some rule are shifted furthest to
--   the left
-- * Discrete arguments that are never matched in any rule follow after
-- * Partially ordered arguments are pushed all the way to the right.
--
-- Note that this sorting is stable; e.g., if two arguments have the same order
-- then their original relative order is preserved.
--
-- There are several edge cases:
--
-- 1. If there are no partially ordered arguments, the rightmost argument is
--    stored in a HashSet.
-- 2. If there are multiple partially ordered arguments, all the arguments
--    are squeezed into tuples, and one HashSet stores those tuples.
-- 3. If the relation has no arguments at all, the code generator will use
--    a simple 'Bool' flag (this is indicated by the empty type lists in
--    'RelationRepresentation').
--
-- This is best illustrated via an example. Let's say we have a relation
-- @R: Discrete1, PartiallyOrdered1, PartiallyOrdered2, Discrete2@.
-- Further suppose that only @Discrete2@ is matched in rules. Then, the fact
-- database for this relation looks like:
--
-- @HashMap Discrete2 (HashMap Discrete1 (HashSet (PartiallyOrdered1, PartiallyOrdered2)))@
--
-- @since 0.0.1
getRepresentation :: RelationRepresentationState σ => RelationInfo -> FixenPass σ RelationRepresentationInfo
getRepresentation RelationInfo {_relationDeclaration = rel_declaration, _relationArgMatchInfo = rel_arg_match_info} = do
  let rel_args = rel_declaration ^. args
  rel_underlying_types <- mapM getUnderlyingType rel_args
  rel_kinds <- mapM fixenGetRelationParamKind rel_args
  rel_query_types <- mapM getMeetMechanism rel_args
  let sorting_metric i j
        -- handle the kinds first
        | i_k == Discrete ∧ j_k == PartiallyOrdered = LT
        | i_k == PartiallyOrdered ∧ j_k == Discrete = GT
        | i_k == PartiallyOrdered ∧ j_k == PartiallyOrdered = EQ
        -- now, both of them are discrete. matched variables come first
        | i_m == Matched ∧ j_m == Unmatched = LT
        | i_m == Unmatched ∧ j_m == Matched = GT
        -- both have the same match info, and are both discrete. treat as eq.
        | otherwise = EQ
        where
          i_k = rel_kinds !! i
          j_k = rel_kinds !! j
          i_m = rel_arg_match_info !! i
          j_m = rel_arg_match_info !! j
  let db_indices = sortBy sorting_metric [0 .. length rel_args - 1]
      insertion_map = IntMap.fromList $ zip db_indices [0 .. length rel_args - 1]
      extraction_map = IntMap.fromList $ zip [0 .. length rel_args - 1] db_indices
      q_types = (rel_query_types !!) <$> db_indices
      u_types = (rel_underlying_types !!) <$> db_indices
      rel_store_types = getStoreType q_types
      db_args = zip3 q_types rel_store_types u_types
  return
    RelationRepresentationInfo
      { _databaseRepresentation =
          Database
            { _databaseTypes = db_args
            , _extractionMap = extraction_map
            }
      , _factRepresentation =
          Fact
            { _factTypes = zip rel_query_types rel_underlying_types
            , _insertionMap = insertion_map
            }
      }
  where
    getStoreType [] = []
    getStoreType [Match] = [StoredAsHashSet]
    getStoreType (Meet _ _ : xs) = replicate (length xs + 1) StoredAsHashSet
    getStoreType (_ : xs) = StoredAsHashMap : getStoreType xs

-- | Determines how a 'Type' is queried in the database.
--
-- @since 0.0.1
getMeetMechanism :: RelationRepresentationState σ => Type -> FixenPass σ QueryType
getMeetMechanism t = do
  let n = calculateRepresentativeFromType t
  p_ord <- fixenGetPartialOrdInfo
  case p_ord ^. at n of
    Nothing -> return Match
    Just p_ord_decl -> return $ Meet (partialOrdDeclarationLeq p_ord_decl) (partialOrdDeclarationMlbs p_ord_decl)
