module Fixen.SymbolSolver.Validation where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.IR.Core qualified as Core
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Prelude
import Prelude.Unicode

type SymbolRule a = a -> SymbolEnv -> FixenPass SymbolState [Report String]

type SymbolValidator a = a -> SymbolEnv -> FixenPass SymbolState Bool

type GenericRepresentativeRule a = HasNodeId a => Name -> SymbolRule a

validate :: [SymbolRule a] -> a -> SymbolEnv -> FixenPass SymbolState Bool
validate rules subject env = do
  reports <-
    rules
      <&> (\f -> f subject env)
      & sequence
  let all_reports = concat reports
  mapM_ accumR all_reports
  return $ (¬) (null all_reports)

validateNamed :: (HasNodeId a, Named a n, IdentifierLike n) => GenericRepresentativeRule a -> SymbolRule a
validateNamed r i env = r (simpleIdentifier $ nameOf i) i env

-- Validation rules.
validateAgainstRelation :: String -> GenericRepresentativeRule a
validateAgainstRelation where_msg repr i env = do
  case env ^. relationMap ∘ at repr of
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
  case env ^. partialOrdMap ∘ at repr of
    Nothing -> return []
    Just p_ord -> do
      pos <- fixenGetPosition p_ord
      pos' <- fixenGetPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [ (pos', This this_msg)
            , (pos, Where "partial ord declaration with the same name")
            ]
            notes
        ]

validateAgainstExtern :: String -> [Note String] -> GenericRepresentativeRule a
validateAgainstExtern this_msg notes repr i env = do
  case env ^. externMap ∘ at repr of
    Nothing -> return []
    Just e_id -> do
      pos <- fixenGetPosition e_id
      pos' <- fixenGetPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [ (pos', This this_msg)
            , (pos, Where "use of an external symbol with the same name")
            ]
            notes
        ]

warnNameShadowingAgainstBoundVar :: String -> GenericRepresentativeRule a
warnNameShadowingAgainstBoundVar where_msg repr i env = do
  let matching_bvs =
        env
          ^. ruleMap
          & IntMap.toList
          <&> snd
          <&> (^. Fixen.Monad.ruleBoundVars)
          <&> Map.toList
          & concat
          & filter (\(_, l) -> any (not . isUsedInAssumption) (_localVarUsage l))
          <&> second _localVarVar
          & filter ((== repr) ∘ fst)
  -- checking against rules
  ls1 <- forM matching_bvs $ \(_, s) -> do
    pos <- fixenGetPosition i
    pos' <- fixenGetPosition s
    return $
      Warn
        Nothing
        "name shadowing"
        [(pos', This "rule parameter"), (pos, Where where_msg)]
        [Hint "change the name of the rule parameter"]
  -- checking against priorities
  let matching_bvs2 =
        env ^. priorityMap
          & IntMap.toList
          <&> snd
          <&> (^. priorityLocalVars)
          <&> Map.toList
          & concat
          & filter ((== repr) . fst)
  ls2 <- forM matching_bvs2 $ \(_, (_, s)) -> do
    pos <- fixenGetPosition i
    pos' <- fixenGetPosition s
    return $
      Warn
        Nothing
        "name shadowing"
        [(pos', This "rule parameter"), (pos, Where where_msg)]
        [Hint "change the name of the rule parameter"]
  return $ ls1 ++ ls2

warnNameShadowingAgainstExtern :: String -> GenericRepresentativeRule a
warnNameShadowingAgainstExtern decl_name repr i env = do
  against_extern <- case env ^. externMap ∘ at repr of
    Nothing -> return []
    Just e_id -> do
      pos <- fixenGetPosition e_id
      pos' <- fixenGetPosition i
      return
        [ Warn
            Nothing
            "name shadowing"
            [ (pos', This decl_name)
            , (pos, Where "use of an external symbol with the same name")
            ]
            [Hint $ "change the name of this " ++ decl_name]
        ]
  against_prelude <-
    if repr `Set.member` preludeTerms
      then do
        pos' <- fixenGetPosition i
        return
          [ Warn
              Nothing
              "name shadowing of prelude terms"
              [(pos', This decl_name)]
              [Hint $ "change the name of this " ++ decl_name]
          ]
      else return []
  return $ against_extern ++ against_prelude

validateAgainstQuery :: String -> GenericRepresentativeRule a
validateAgainstQuery where_msg repr i env = do
  case env ^. queryMap ∘ at repr of
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
  case env ^. relationMap ∘ at (simpleIdentifier (nameOf rel)) of
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
      if rel_arity ≠ rel_decl_arity
        then do
          rel_pos <- fixenGetPosition rel
          rel_decl_pos <- fixenGetPosition rel_decl
          return
            [ Err
                Nothing
                "wrong arity"
                [ (rel_pos, This $ concat [containing_name, " with ", fmt rel_arity])
                , (rel_decl_pos, Where $ concat ["rel declared with ", fmt rel_decl_arity])
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

warnUnusedRuleParameters :: SymbolEnv -> FixenPass SymbolState ()
warnUnusedRuleParameters env = do
  unused_rule_params <- getUnusedRuleParams
  when (not (null unused_rule_params)) $ do
    pos <- mapM fixenGetPosition unused_rule_params
    accumWarn
      Nothing
      "unused rule parameters"
      ((,This "rule parameter") <$> pos)
      [Hint "replace these with holes `_`"]
  where
    getUnusedRuleParams = do
      let rule_info = env ^. ruleMap & IntMap.toList
      ls <- mapM getUnusedRuleParamsOfRule rule_info
      return $ concat ls
    getUnusedRuleParamsOfRule :: (NodeId, RuleInfo) -> FixenPass SymbolState [SimpleIdentifier]
    getUnusedRuleParamsOfRule (rule_node_id, rule_info) = do
      let bvs = rule_info ^. Fixen.Monad.ruleBoundVars
          potentially_unused_bvs =
            Map.filter
              ( \lv_info ->
                  let usage = lv_info ^. localVarUsage
                   in (all isUsedInAssumption usage) && length usage < 2
              )
              bvs
          priorities = env ^. priorityMap & IntMap.toList <&> snd <&> _priorityLocalVars <&> Map.toList & concat <&> (\(k, (v, _)) -> (k, v))
          unused_bvs = Map.filterKeys (\k -> all (\(k', n) -> k /= k' || n /= rule_node_id) priorities) potentially_unused_bvs
      return $ unused_bvs <&> _localVarVar & foldr (:) []
