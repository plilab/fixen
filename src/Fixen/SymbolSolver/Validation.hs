-- |
-- Module      : Fixen.SymbolSolver.Validation
-- Description : Common validation rules for symbol solving
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides validation rules for various syntactic categories in
-- Fixen programs.
--
-- @since 26.7
module Fixen.SymbolSolver.Validation where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Prelude
import Fixen.Utils

--------------------------------------------------------------------------------

-- * The Main Types

--------------------------------------------------------------------------------

-- | A rule for a syntactic category.
--
-- @since 26.7
type SymbolRule σ α = α -> SymbolEnv -> FixenPass σ [Report String]

-- | A symbol validator, i.e., a function that checks the validity of a symbol
-- with respect to a 'SymbolEnv'.
--
-- @since 26.7
type SymbolValidator σ α = α -> SymbolEnv -> FixenPass σ Bool

-- | A 'SymbolRule' that uses the name of the symbol for validation. We also
-- require these to have 'NodeId's for source position tracking.
--
-- @since 26.7
type NamedSymbolRule σ α = HasNodeId α NodeId => Name -> SymbolRule σ α

--------------------------------------------------------------------------------

-- * Validation Functions

--------------------------------------------------------------------------------

-- | The main validation function. This function receives a list of rules and
-- checks a symbol against them, with respect to the 'SymbolEnv'.
--
-- Usage pattern: To create a symbol validator, simply call 'validate' on a
-- list of 'SymbolRule's that apply to that symbol. For instance,
--
-- @
-- validateMySymbol :: SymbolValidator MySymbol
-- validateMySymbol = validate mySymbolRules where
--   mySymbolRules = [ rule1, rule2, ..., ruleN ]
-- @
--
-- @since 26.7
validate
  :: SymbolState σ
  => [SymbolRule σ α]
  -- ^ The list of rules for this symbol
  --
  -- @since 26.7
  -> SymbolValidator σ α
validate ruls subject env = do
  reports <- ruls <&> (\f -> f subject env) & sequence
  let all_reports = concat reports
  mapM_ accumR all_reports
  return $ (¬) (null all_reports)

-- Turns a 'NamedSymbolRule' into a regular 'SymbolRule'.
--
-- @since 26.7
validateNamed :: (HasNodeId a NodeId, HasName a n, IdentifierLike n) => NamedSymbolRule σ a -> SymbolRule σ a
validateNamed r i env = r (simpleIdentifier $ i ^. name) i env

--------------------------------------------------------------------------------

-- * Common Validation Rules

-- $commonValidationRules
--
-- These validation rules throw __errors__ whenever the rules are violated.

--------------------------------------------------------------------------------

-- ** Relations

-- | Reject duplicate named parameters within one relation declaration.
-- Unnamed parameters are ignored. The relation name is accepted to match the
-- shape expected by 'validateNamed', but the check itself only inspects the
-- parameters.
--
-- @since 0.0.1
validateUniqueParamNames
  :: SymbolState σ
  => String
  -> Name
  -> SymbolRule σ RelationDeclaration
validateUniqueParamNames whereMsg _relationName rel _env = do
  let parameterNames = rel ^.. args . each . name . _Just
      duplicates = filter (\n -> length (filter (≅ n) parameterNames) > 1) parameterNames
  case duplicates of
    [] -> return []
    _ -> do
      positions <- mapM getPosition duplicates
      return
        [ Err
            Nothing
            "duplicate parameter names"
            ((, This whereMsg) <$> positions)
            [ Note "relation parameter names must be unique"
            , Hint "rename one of these parameters"
            ]
        ]

-- | Validates a symbol against a relation. This rule prevents the symbol from
-- sharing the same name as a relation symbol in the 'SymbolEnv'.
--
-- @since 26.7
validateAgainstRelation
  :: SymbolState σ
  => String
  -- ^ The error message attached to the symbol being validated (in the case of
  -- a validation error)
  --
  -- @since 26.7
  -> NamedSymbolRule σ a
validateAgainstRelation where_msg repr i env = do
  case env ^. relationInfos . at repr of
    -- not a relation name, all is good
    Nothing -> return []
    -- is a relation name, throw an error on the relation
    Just rel_info -> do
      let rel_decl = rel_info ^. declaration
      pos <- getPosition i
      pos' <- getPosition rel_decl
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos', This "rel declaration"), (pos, Where where_msg)]
            relationValidationErrorNotes
        ]

-- | Checks that a relation-like symbol exists as a relation declaration in the
-- program and has the right arity.
--
-- @since 26.7
relationExistsAndHasRightArity
  :: SymbolState σ
  => RelationLike b
  -- ^ The relation
  --
  -- @since 26.7
  -> String
  -- ^ The name of the thing that contains the occurrence of this relation symbol
  --
  -- @since 26.7
  -> SymbolEnv
  -> FixenPass σ [Report String]
relationExistsAndHasRightArity rel containing_name env = do
  case env ^. relationInfos . at (simpleIdentifier (rel ^. name)) of
    Nothing -> do
      rel_pos <- getPosition rel
      return
        [ Err
            Nothing
            "unknown relation"
            [(rel_pos, This "relation name not found")]
            []
        ]
    Just rel_info -> do
      let rel_decl = rel_info ^. declaration
          rel_decl_arity = length (rel_decl ^. args)
          rel_arity = length (rel ^. args)
          fmt 0 = "no arguments"
          fmt 1 = "1 argument"
          fmt n = show n ++ " arguments"
      if rel_arity ≠ rel_decl_arity
        then do
          rel_pos <- getPosition rel
          rel_decl_pos <- getPosition rel_decl
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

-- ** Partial Order Declarations

-- | Validates a symbol against a partial order declaration. This rule prevents
-- the symbol from sharing the same name as a partial order declaration.
--
-- @since 26.7
validateAgainstPartialOrd
  :: SymbolState σ
  => String
  -- ^ The error message attached to the symbol being validated (in the case of
  -- a validation error)
  --
  -- @since 26.7
  -> [Note String]
  -- ^ Notes to attach to the error message in the case of a validation error
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstPartialOrd this_msg notes repr i env = do
  case env ^. partialOrdInfos . at repr of
    Nothing -> return []
    Just p_ord -> do
      pos <- getPosition p_ord
      pos' <- getPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [ (pos', This this_msg)
            , (pos, Where "partial ord declaration with the same name")
            ]
            notes
        ]

-- ** Lattice Declarations

-- | Validates a symbol against a lattice declaration. This rule prevents
-- the symbol from sharing the same name as a lattice declaration.
--
-- @since 26.7
validateAgainstLattice
  :: SymbolState σ
  => String
  -- ^ The error message attached to the symbol being validated (in the case of
  -- a validation error)
  --
  -- @since 26.7
  -> [Note String]
  -- ^ Notes to attach to the error message in the case of a validation error
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstLattice this_msg notes repr i env = do
  case env ^. latticeInfos . at repr of
    Nothing -> return []
    Just p_ord -> do
      pos <- getPosition p_ord
      pos' <- getPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [ (pos', This this_msg)
            , (pos, Where "lat declaration with the same name")
            ]
            notes
        ]

-- ** External Symbols

-- $externalSymbols
--
-- Symbols in a Fixen program are considered \"external\" whenever they are not
-- declared in the Fixen program.

-- | Validates a symbol against external symbols. This rule prevents a symbol
-- from sharing the same name as an external symbol.
--
-- @since 26.7
validateAgainstExtern
  :: SymbolState σ
  => String
  -- ^ The error message to attach to the symbol (in the case of a validation
  -- error)
  --
  -- @since 26.7
  -> [Note String]
  -- ^ Notes to attach to the error message in the case of a validation error
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstExtern this_msg notes repr i env = do
  case env ^. externInfos . at repr of
    Nothing -> return []
    Just e_id -> do
      pos <- getPositionFromNodeId e_id
      pos' <- getPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [ (pos', This this_msg)
            , (pos, Where "use of an external symbol with the same name")
            ]
            notes
        ]

-- | Validates a symbol against those that are generated by Fixen. At
-- the moment, this function does not check against __all__ Fixen-generated
-- symbols (since relations, etc., also generate new Haskell definitions)
--
-- @since 26.7
validateAgainstFixenCapitalized
  :: SymbolState σ
  => String
  -- ^ The error message to attach to this symbol (in the case of a validation
  -- error)
  --
  -- @since 26.7
  -> String
  -- ^ The name of the type of this symbol (relation declaration, query, etc.)
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstFixenCapitalized this_msg decl_type repr i _ =
  if repr ∈ fixenTypesCons
    then do
      pos <- getPosition i
      return
        [ Err
            Nothing
            "name clash with Fixen-generated symbol"
            [(pos, This this_msg)]
            [Note $ "change the name of this " ++ decl_type]
        ]
    else return []

-- | Validates a symbol against those that are generated by Fixen. At
-- the moment, this function does not check against __all__ Fixen-generated
-- symbols (since relations, etc., also generate new Haskell definitions)
--
-- @since 26.7
validateAgainstFixenLowercase
  :: SymbolState σ
  => String
  -- ^ The error message to attach to this symbol (in the case of a validation
  -- error)
  --
  -- @since 26.7
  -> String
  -- ^ The name of the type of this symbol (relation declaration, query, etc.)
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstFixenLowercase this_msg decl_type repr i _ =
  if repr ∈ fixenTerms
    then do
      pos <- getPosition i
      return
        [ Err
            Nothing
            "name clash with Fixen-generated symbol"
            [(pos, This this_msg)]
            [Note $ "change the name of this " ++ decl_type]
        ]
    else return []

-- ** Queries

-- | Validates a symbol against a query declaration. This rule prevents a symbol
-- from sharing the same name as a query declaration.
--
-- @since 26.7
validateAgainstQuery
  :: SymbolState σ
  => String
  -- ^ The error message to attach to the symbol, in the case of a validation
  -- error
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
validateAgainstQuery where_msg repr i env = do
  case env ^. queryInfos . at repr of
    Nothing -> return []
    Just q -> do
      pos <- getPosition q
      pos' <- getPosition i
      return
        [ Err
            Nothing
            "duplicate names!"
            [(pos, This "query declaration"), (pos', Where where_msg)]
            queryValidationErrorNotes
        ]

--------------------------------------------------------------------------------

-- * Common Warnings

-- $commonWarnings
--
-- These validation rules throw __warnings__ whenever the rules are violated.

--------------------------------------------------------------------------------

-- | Emits warnings whenever there are unused rule parameters.
--
-- A rule parameter is considered unused if:
--
-- 1. It is not used in any condition or conclusion, __and__
-- 2. All instantiations of the parameter (in priority declarations) are unused
--
-- /Precondition/: Requires that everything in the program has been inserted
-- into the 'SymbolEnv'.
--
-- @since 26.7
warnUnusedRuleParameters
  :: SymbolState σ
  => SymbolEnv
  -- ^ The 'SymbolEnv' to check
  --
  -- @since 26.7
  -> FixenPass σ ()
warnUnusedRuleParameters env = do
  unused_rule_params <- getUnusedRuleParams
  when ((¬) (null unused_rule_params)) $ do
    pos <- mapM getPosition unused_rule_params
    accumWarn
      Nothing
      "unused rule parameters"
      ((,This "rule parameter") <$> pos)
      [Hint "replace these with holes `_`"]
  where
    getUnusedRuleParams = do
      let rule_info = env ^. ruleInfos & IntMap.toList
      ls <- mapM getUnusedRuleParamsOfRule rule_info
      return $ concat ls

    getUnusedRuleParamsOfRule (rule_node_id, rule_info) = do
      let bvs = rule_info ^. args
          potentially_unused_bvs =
            Map.filter
              ( \lv_info ->
                  let usage = lv_info ^. usageInfo
                   in (all isUsedInAssumption usage) ∧ length usage < 2
              )
              bvs
          varsUsedInPriorities =
            values (env ^. priorityInfos)
              ^.. each . args
              <&> Map.toList
              & concat
              <&> (\(k, (v, _)) -> (k, v))
          unused_bvs =
            Map.filterKeys
              ( \k ->
                  all
                    (\(k', n) -> k ≠ k' ∨ n ≠ rule_node_id)
                    varsUsedInPriorities
              )
              potentially_unused_bvs
      return $ unused_bvs ^.. each . var

-- | Emits warnings whenever an external symbol is shadowed by some kind of
-- bound variable (in rules and priorities).
--
-- @since 26.7
warnNameShadowingAgainstBoundVar
  :: SymbolState σ
  => String
  -- ^ The error message to attach to the variable
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
warnNameShadowingAgainstBoundVar where_msg repr i env = do
  let matching_bvs =
        values (env ^. ruleInfos)
          ^.. each . args
          <&> Map.toList
          & concat
          & filter (\(_, l) -> any ((¬) ∘ isUsedInAssumption) (l ^. usageInfo))
          <&> second (^. var)
          & filter ((== repr) ∘ fst)
  -- checking against rules
  ls1 <- forM matching_bvs $ \(_, s) -> do
    pos <- getPosition i
    pos' <- getPosition s
    return $
      Warn
        Nothing
        "name shadowing"
        [(pos', This "rule parameter"), (pos, Where where_msg)]
        [Hint "change the name of the rule parameter"]
  -- checking against priorities
  let matching_bvs2 =
        values (env ^. priorityInfos)
          ^.. each . args
          <&> Map.toList
          & concat
          & filter ((== repr) ∘ fst)
  ls2 <- forM matching_bvs2 $ \(_, (_, s)) -> do
    pos <- getPosition i
    pos' <- getPosition s
    return $
      Warn
        Nothing
        "name shadowing"
        [(pos', This "rule parameter"), (pos, Where where_msg)]
        [Hint "change the name of the rule parameter"]
  return $ ls1 ++ ls2

-- | Warns whenever a rule parameter or priority local variable shadows an
-- external symbol.
--
-- @since 26.7
warnNameShadowingAgainstExtern
  :: SymbolState σ
  => String
  -- ^ The name of the thing (e.g., rule parameter, local variable)
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
warnNameShadowingAgainstExtern decl_name repr i env = do
  against_extern <- case env ^. externInfos . at repr of
    Nothing -> return []
    Just e_id -> do
      pos <- getPositionFromNodeId e_id
      pos' <- getPosition i
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
    if repr ∈ preludeTerms
      then do
        pos' <- getPosition i
        return
          [ Warn
              Nothing
              "name shadowing of prelude terms"
              [(pos', This decl_name)]
              [Hint $ "change the name of this " ++ decl_name]
          ]
      else return []
  return $ against_extern ++ against_prelude

-- | Warns whenever something has the same name as a type or constructor in
-- Haskell's Prelude.
--
-- @since 26.7
warnAgainstPreludeCapitalized
  :: SymbolState σ
  => String
  -- ^ The error message to attach to the thing being validated
  --
  -- @since 26.7
  -> String
  -- ^ The type of the thing being validated (e.g., relation, variable, etc.)
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
warnAgainstPreludeCapitalized this_msg decl_type repr i _ =
  if repr ∈ preludeTermsCons
    then do
      pos <- getPosition i
      return
        [ Warn
            Nothing
            "potential name clash with Prelude symbols"
            [(pos, This this_msg)]
            [Hint $ "hide this name from the Prelude import, or change the name of this " ++ decl_type]
        ]
    else return []

-- | Warns whenever something has the same name as a term in
-- Haskell's Prelude.
--
-- @since 26.7
warnAgainstPreludeLowercase
  :: SymbolState σ
  => String
  -- ^ The error message to attach to the thing being validated
  --
  -- @since 26.7
  -> String
  -- ^ The type of the thing being validated (e.g., relation, variable, etc.)
  --
  -- @since 26.7
  -> NamedSymbolRule σ α
warnAgainstPreludeLowercase this_msg decl_type repr i _ =
  if repr ∈ preludeTerms
    then do
      pos <- getPosition i
      return
        [ Warn
            Nothing
            "potential name clash with Prelude symbols"
            [(pos, This this_msg)]
            [Hint $ "hide this name from the Prelude import, or change the name of this " ++ decl_type]
        ]
    else return []

--------------------------------------------------------------------------------

-- * Miscellaneous

--------------------------------------------------------------------------------

-- | Standard notes for whenever a relation has a bad name.
--
-- @since 26.7
relationValidationErrorNotes :: [Note String]
relationValidationErrorNotes = [Note "relations cannot share names with\n  1. other declarations in the program (i.e., rel and partial ord), and\n  2. types/constructors used in the program"]

-- | Standard notes for whenever a query has a bad name.
--
-- @since 26.7
queryValidationErrorNotes :: [Note String]
queryValidationErrorNotes =
  [Note "queries cannot share names with \"external\" terms used in the program (these include other queries)"]
