module Fixen.RelationRepresentation where

import Control.Lens
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Map.Strict qualified as Map
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.IR.RelationRepresentation
import Fixen.Monad

type RelationRepresentationState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

getDatabaseRepresentation :: FixenPass RelationRepresentationState RelationRepresentation
getDatabaseRepresentation = do
  rel_map <- fixenGetRelationInfo
  mapM getRepresentation rel_map

getRepresentation :: RelationInfo -> FixenPass RelationRepresentationState RelationRepresentationInfo
getRepresentation RelationInfo {_relationDeclaration = rel_declaration, _relationArgMatchInfo = rel_arg_match_info} = do
  let rel_args = relationParams rel_declaration
  rel_underlying_types <- mapM getUnderlyingType rel_args
  rel_kinds <- mapM getKind rel_args
  rel_query_types <- mapM getMeetMechanism rel_args
  let sorting_metric i j
        -- handle the kinds first
        | i_k == Discrete && j_k == PartiallyOrdered = LT
        | i_k == PartiallyOrdered && j_k == Discrete = EQ
        | i_k == PartiallyOrdered && j_k == PartiallyOrdered = GT
        -- now, both of them are discrete. matched variables come first
        | i_m == Matched && j_m == Unmatched = LT
        | i_m == Unmatched && j_m == Matched = GT
        -- both have the same match info, and are both discrete. treat as eq.
        | otherwise = EQ
        where
          i_k = rel_kinds !! i
          j_k = rel_kinds !! j
          i_m = rel_arg_match_info !! i
          j_m = rel_arg_match_info !! j
  let db_indices = sortBy sorting_metric [0 .. length rel_args - 1]
      insertion_map = IntMap.fromList $ zip [0 .. length rel_args - 1] db_indices
      extraction_map = IntMap.fromList $ zip db_indices [0 .. length rel_args - 1]
      db_args = (\i -> (rel_query_types !! i, rel_underlying_types !! i)) <$> db_indices
  return
    RelationRepresentationInfo
      { _databaseRepresentation =
          Database
            { _databaseTypes = db_args
            , _extractionMap = extraction_map
            }
      , _factRepresentation =
          Fact
            { _factTypes = rel_underlying_types
            , _insertionMap = insertion_map
            }
      }

getMeetMechanism :: Type -> FixenPass RelationRepresentationState QueryType
getMeetMechanism t = do
  let name = calculateRepresentativeFromType t
  p_ord <- fixenGetPartialOrdInfo
  case p_ord ^. at name of
    Nothing -> return Match
    Just p_ord_decl -> return $ Meet (partialOrdDeclarationMlbs p_ord_decl)

getUnderlyingType :: Type -> FixenPass RelationRepresentationState Type
getUnderlyingType t = do
  let name = calculateRepresentativeFromType t
  p_ord <- fixenGetPartialOrdInfo
  case p_ord ^. at name of
    Nothing -> return t
    Just p_ord_dec -> return $ partialOrdDeclarationType p_ord_dec

getKind :: Type -> FixenPass RelationRepresentationState Kind
getKind t = do
  let name = calculateRepresentativeFromType t
  pk_info <- fixenGetRelationParamKindInfo
  -- guaranteed to succeed
  return $ pk_info Map.! name
