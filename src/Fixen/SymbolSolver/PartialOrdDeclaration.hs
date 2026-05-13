-- |
--     Module      : Fixen.SymbolSolver.PartialOrdDeclaration
--     Description : Symbol solving for partial ord declarations
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides facilities for solving partial ord declaration
--     symbols.
--
-- @since 0.0.1
module Fixen.SymbolSolver.PartialOrdDeclaration where

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
-- You should only need 'initEnvWithPartialOrd'.

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with a 'PartialOrdDeclaration'. The types, leq
-- and mlbs symbols are added as extern symbols to the environment (see
-- "Fixen.SymbolSolver.Extern").
--
-- @since 0.0.1
initEnvWithPartialOrd
  :: SymbolState σ
  => SymbolEnv
  -- ^ The 'SymbolEnv'
  -> PartialOrdDeclaration
  -- ^ The 'PartialOrdDeclaration'
  -> FixenPass σ SymbolEnv
initEnvWithPartialOrd env p = do
  _ <- validatePartialOrd p env
  env
    & insertPartialOrdInfo
    & insertRelationParamKindInfo
    & foldMWith initEnvWithExternSymbol extern_symbols
  where
    repr = simpleIdentifier $ p ^. name
    extern_symbols = (⋃) [type_symb, leq_symb, mlb_symb]
    type_symb = getAllTypeNames $ partialOrdDeclarationType p
    leq_symb = getSimpleIdentifierFromIdentifier $ partialOrdDeclarationLeq p
    mlb_symb = getSimpleIdentifierFromIdentifier $ partialOrdDeclarationMlbs p
    insertPartialOrdInfo e =
      -- do not insert if already exists
      case e ^. partialOrdInfos . at repr of
        Just _ -> e
        Nothing -> e & partialOrdInfos . at repr ?~ p
    -- unconditionally insert the partial order (i.e., join it with
    -- whatever is inside, just in case it was intiialized as being
    -- discrete)
    insertRelationParamKindInfo = kindInfos . at repr ?~ PartiallyOrdered

--------------------------------------------------------------------------------

-- * Helpers

--------------------------------------------------------------------------------

-- | Performs validation on any 'PartialOrdDeclaration' inserted. The rules are
--
-- * __Against Other 'PartialOrdDeclarations'__: If there is already a partial
--   ord declaration with the same name, an error is thrown.
-- * __Against Relation Declarations__: If there is a relation declaration of
--   the same name, an error is thrown.
--
-- Note that the extern symbols created by the 'PartialOrdDeclaration', i.e.,
-- its types, leq and mlbs symbols, are validated as they are being inserted
-- by 'initEnvWithExternSymbol'.
--
-- @since 0.0.1
validatePartialOrd :: SymbolState σ => SymbolValidator σ PartialOrdDeclaration
validatePartialOrd = validate r
  where
    r =
      [ againstOtherPartialOrds
      , againstRelations
      ]

    againstOtherPartialOrds =
      validateNamed
        ( validateAgainstPartialOrd
            "a partial ord declaration"
            [Note "partial ord declarations must have distinct names"]
        )

    againstRelations =
      validateNamed
        (validateAgainstRelation "a partial ord declaration with the same name")
