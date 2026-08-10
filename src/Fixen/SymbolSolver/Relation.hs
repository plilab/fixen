-- |
-- Module      : Fixen.SymbolSolver.Relation
-- Description : Symbol solving for relation declarations
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides facilities for solving relation declarations.
--
-- @since 26.7
module Fixen.SymbolSolver.Relation where

import Control.Lens
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern
import Fixen.SymbolSolver.Validation
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Main Entry Point

-- $mainEntryPoint
--
-- You should only need 'initEnvWithRelation'.

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with a 'RelationDeclaration'.
--
-- /Precondition/: The 'SymbolEnv' must have been initialized with
-- 'PartialOrdDeclaration's.
--
-- @since 26.7
initEnvWithRelation :: SymbolState σ => SymbolEnv -> RelationDeclaration -> FixenPass σ SymbolEnv
initEnvWithRelation env r = do
  _ <- validateRelation r env
  env
    & insertRelationParamTypesAsDiscrete
    & insertRelation
    & foldMWith initEnvWithExternSymbol non_p_ord_params
  where
    rel_name = simpleIdentifier $ r ^. name
    rel_params = r ^. args

    insertRelation e =
      let new_rel_info =
            RelationInfo
              { _relationDeclaration = r
              , _relationArgMatchInfo = replicate (length rel_params) Unmatched
              }
       in case e ^. relationInfos . at rel_name of
            Just _ -> e
            Nothing -> e & relationInfos . at rel_name ?~ new_rel_info

    insertRelationParamTypesAsDiscrete e =
      let info = calculateRepresentativeFromType <$> rel_params
       in Prelude.foldl' softInsertTypeAsDiscrete e info

    softInsertTypeAsDiscrete :: SymbolEnv -> Name -> SymbolEnv
    softInsertTypeAsDiscrete e p =
      -- check if the discreteness annotation is already there.
      -- if it is, don't re-insert; it might be partially ordered!
      case e ^. kindInfos . at p of
        Just _ -> e
        Nothing ->
          e & kindInfos . at p ?~ Discrete

    non_p_ord_params =
      let -- filter out all the args that are partial ords, since their DB representation
          -- will not have partial ords (only the underlying type). If the underlying type is
          -- the same name (or references it), they are already in the extern list.
          non_p_ord_args = filter notAPartialOrd rel_params
       in getAllTypeNamesList non_p_ord_args

    notAPartialOrd (TypeName _ (IdentifierSimpleIdentifier (SimpleIdentifier _ s))) =
      let partial_ords = env ^. partialOrdInfos
          lattice_decls = env ^. latticeInfos
       in s ∉ partial_ords ∧ s ∉ lattice_decls
    notAPartialOrd _ = True

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Validates a 'RelationDeclaration'. The rules are:
--
-- * __Against Other Relations__: The relation being declared must not have
--   the same name as another relation declaration.
-- * __Against Partial Ord Declarations__: The relation being declared must not
--   have the same name as a partial ord declaration.
-- * __Against Extern Symbols__: The relation being declared must not have the
--   same name as an extern symbol. Note that a symbol is considered as extern
--   if it is not declared (in the appropriate context) in the Fixen program.
-- * __Against Fixen-generated Symbols__: The relation being declared must not
--   have the same name as a Fixen-generated symbol.
-- * __Against Prelude Symbols__: A warning is generated whenever the relation
--   has the same name as a type or term in Prelude.
--
-- @since 26.7
validateRelation :: SymbolState σ => SymbolValidator σ RelationDeclaration
validateRelation = validate r
  where
    r =
      [ againstOtherRelations
      , againstPartialOrd
      , againstLattice
      , againstExtern
      , againstPrelude
      , againstFixenCapitalized
      ]
    againstOtherRelations =
      validateNamed
        (validateAgainstRelation "another rel with the same name")
    againstPartialOrd =
      validateNamed
        ( validateAgainstPartialOrd
            "rel declaration"
            relationValidationErrorNotes
        )
    againstLattice =
      validateNamed
        ( validateAgainstLattice
            "rel declaration"
            relationValidationErrorNotes
        )
    againstExtern =
      validateNamed
        ( validateAgainstExtern "rel declaration" relationValidationErrorNotes
        )
    againstPrelude =
      validateNamed (warnAgainstPreludeCapitalized "rel declaration" "rel declaration")
    againstFixenCapitalized =
      validateNamed (validateAgainstFixenCapitalized "rel declaration" "rel declaration")
