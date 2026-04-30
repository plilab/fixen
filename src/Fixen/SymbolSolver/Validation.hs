module Fixen.SymbolSolver.Validation where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.IR.Core qualified as Core
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Prelude

type SymbolRule a = a -> SymbolEnv -> FixenPass SymbolState [Report String]

type SymbolValidator a = a -> SymbolEnv -> FixenPass SymbolState Bool

type GenericRepresentativeRule a = HasNodeId a => Representative -> SymbolRule a

validate :: [SymbolRule a] -> a -> SymbolEnv -> FixenPass SymbolState Bool
validate rules subject env = do
  reports <- sequence $ (\f -> f subject env) <$> rules
  mapM_ accumR (concat reports)
  return $ not $ null reports

validateNamed :: (HasNodeId a, Named a n, IdentifierLike n) => GenericRepresentativeRule a -> SymbolRule a
validateNamed r i env = r (simpleIdentifier $ nameOf i) i env

-- Validation rules.
validateAgainstRelation :: String -> GenericRepresentativeRule a
validateAgainstRelation where_msg repr i env = do
  let -- check if this identifier is a relation name
      rel_info_maybe =
        env
          ^. infoMap
            . relationInfoMap
            . at repr
  case rel_info_maybe of
    -- not a relation name, all is good
    Nothing -> return []
    -- is a relation name, throw an error on the relation
    Just rel_info -> do
      let rel_decl = rel_info ^. relationDeclaration
      pos <- fixenGetPosition i
      pos' <- fixenGetPosition rel_decl
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos', This "rel declaration"), (pos, Where where_msg)]
            relationValidationErrorNotes
        ]

relationValidationErrorNotes :: [Note String]
relationValidationErrorNotes = [Note "relations cannot share names with\n  1. other declarations in the program (i.e., rel and partial ord), and\n  2. types/constructors used in the program"]

queryValidationErrorNotes :: [Note String]
queryValidationErrorNotes =
  [Note "queries cannot share names with \"external\" terms used in the program (these include other queries)"]

validateAgainstPartialOrd :: String -> [Note String] -> GenericRepresentativeRule a
validateAgainstPartialOrd this_msg notes repr i env = do
  let p_ord_maybe =
        env
          ^. infoMap
            . partialOrdInfoMap
            . at repr
  case p_ord_maybe of
    Nothing -> return []
    Just p_ord -> do
      pos <- fixenGetPosition p_ord
      pos' <- fixenGetPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos', This this_msg), (pos, Where "partial ord declaration with the same name")]
            notes
        ]

validateAgainstExtern :: String -> [Note String] -> GenericRepresentativeRule a
validateAgainstExtern this_msg notes repr i env = do
  let extern_maybe =
        env
          ^. infoMap
            . externInfoMap
            . at repr
  case extern_maybe of
    Nothing -> return []
    Just e_id -> do
      pos <- fixenGetPosition e_id
      pos' <- fixenGetPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos', This this_msg), (pos, Where "use of an external symbol with the same name")]
            notes
        ]

warnNameShadowingAgainstBoundVar :: String -> GenericRepresentativeRule a
warnNameShadowingAgainstBoundVar where_msg repr i env = do
  let matching_bvs =
        env
          ^. infoMap . ruleInfoMap
          & IntMap.toList
          <&> snd
          <&> (^. Fixen.Monad.ruleBoundVars)
          <&> Map.toList
          & concat
          <&> (\(r, inf) -> (r, _localVarVar inf))
          & filter (\(r, _) -> r == repr)
  -- liftIO $ print matching_bvs
  forM matching_bvs $ \(_, s) -> do
    pos <- fixenGetPosition i
    pos' <- fixenGetPosition s
    return $
      Warn
        Nothing
        "name shadowing"
        [(pos', This "rule parameter"), (pos, Where where_msg)]
        [Hint "change the name of the rule parameter"]

warnNameShadowingAgainstExtern :: GenericRepresentativeRule a
warnNameShadowingAgainstExtern repr i env = do
  let extern_maybe =
        env
          ^. infoMap
            . externInfoMap
            . at repr
  case extern_maybe of
    Nothing -> return []
    Just e_id -> do
      pos <- fixenGetPosition e_id
      pos' <- fixenGetPosition i
      return
        [ Warn
            Nothing
            "name shadowing"
            [(pos', This "rule parameter"), (pos, Where "use of an external symbol with the same name")]
            [Hint "change the name of the rule parameter"]
        ]

validateAgainstQuery :: String -> GenericRepresentativeRule a
validateAgainstQuery where_msg repr i env = do
  let q_maybe =
        env
          ^. infoMap
            . queryInfoMap
            . at repr
  case q_maybe of
    Nothing -> return []
    Just q -> do
      pos <- fixenGetPosition q
      pos' <- fixenGetPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos, This "query declaration"), (pos', Where where_msg)]
            queryValidationErrorNotes
        ]

validateAgainstPreludeCapitalized :: String -> String -> GenericRepresentativeRule a
validateAgainstPreludeCapitalized this_msg decl_type repr i _ =
  if repr `Set.member` preludeTermsCons
    then do
      pos <- fixenGetPosition i
      return
        [ Warn
            Nothing
            "potential name clash with Prelude symbols"
            [(pos, This this_msg)]
            [Hint $ "hide this name from the Prelude import, or change the name of this " ++ decl_type]
        ]
    else return []

validateAgainstFixenCapitalized :: String -> String -> GenericRepresentativeRule a
validateAgainstFixenCapitalized this_msg decl_type repr i _ =
  if repr `Set.member` fixenTypesCons
    then do
      pos <- fixenGetPosition i
      return
        [ Err
            Nothing
            "name clash with Fixen-generated symbol"
            [(pos, This this_msg)]
            [Note $ "change the name of this " ++ decl_type]
        ]
    else return []

validateAgainstPreludeLowercase :: String -> String -> GenericRepresentativeRule a
validateAgainstPreludeLowercase this_msg decl_type repr i _ =
  if repr `Set.member` preludeTerms
    then do
      pos <- fixenGetPosition i
      return
        [ Warn
            Nothing
            "potential name clash with Prelude symbols"
            [(pos, This this_msg)]
            [Hint $ "hide this name from the Prelude import, or change the name of this " ++ decl_type]
        ]
    else return []

validateAgainstFixenLowercase :: String -> String -> GenericRepresentativeRule a
validateAgainstFixenLowercase this_msg decl_type repr i _ =
  if repr `Set.member` fixenTerms
    then do
      pos <- fixenGetPosition i
      return
        [ Err
            Nothing
            "name clash with Fixen-generated symbol"
            [(pos, This this_msg)]
            [Note $ "change the name of this " ++ decl_type]
        ]
    else return []

relationExistsAndHasRightArity :: Core.Relation SimpleIdentifier [b] -> String -> SymbolEnv -> FixenPass SymbolState [Report String]
relationExistsAndHasRightArity rel containing_name env = do
  let rel_repr = simpleIdentifier $ nameOf rel
  -- find relation in env
  case env ^. infoMap . relationInfoMap . at rel_repr of
    Nothing -> do
      rel_pos <- fixenGetPosition rel
      return
        [ Err
            Nothing
            "unknown relation"
            [(rel_pos, This "relation name not found")]
            []
        ]
    Just rel_info -> do
      let rel_decl = rel_info ^. relationDeclaration
          rel_decl_arity = length (relationParams rel_decl)
          rel_arity = length (relationParams rel)
          fmt 0 = "no arguments"
          fmt 1 = "1 argument"
          fmt n = show n ++ " arguments"
      if rel_arity /= rel_decl_arity
        then do
          rel_pos <- fixenGetPosition rel
          rel_decl_pos <- fixenGetPosition rel_decl
          return
            [ Err
                Nothing
                "wrong arity"
                [
                  ( rel_pos
                  , This $
                      concat
                        [ containing_name
                        , " with "
                        , fmt rel_arity
                        ]
                  )
                ,
                  ( rel_decl_pos
                  , Where $
                      concat
                        [ "rel declared with "
                        , fmt rel_decl_arity
                        ]
                  )
                ]
                [ Note $
                    concat
                      [ "number of arguments to relation in "
                      , containing_name
                      , " must match the arity of the relation"
                      ]
                ]
            ]
        else return []
