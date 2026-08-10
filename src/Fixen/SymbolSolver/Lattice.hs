module Fixen.SymbolSolver.Lattice where

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
-- You should only need 'initEnvWithLattice'.

--------------------------------------------------------------------------------

-- | Initializes a 'SymbolEnv' with a 'LatticeDeclaration'. The types, leq, join
-- and meet symbols are added as extern symbols to the environment (see
-- "Fixen.SymbolSolver.Extern").
--
-- @since 26.7
initEnvWithLattice
  :: SymbolState σ
  => SymbolEnv
  -- ^ The 'SymbolEnv'
  -> LatticeDeclaration
  -- ^ The 'LatticeDeclaration'
  -> FixenPass σ SymbolEnv
initEnvWithLattice env p = do
  _ <- validateLattice p env
  env
    & insertLatticeInfo
    & insertRelationParamKindInfo
    & foldMWith initEnvWithExternSymbol extern_symbols
  where
    repr = simpleIdentifier $ p ^. name
    extern_symbols = (⋃) [type_symb, leq_symb, join_symb, meet_symb]
    type_symb = getAllTypeNames $ latticeDeclarationType p
    leq_symb = getSimpleIdentifierFromIdentifier $ latticeDeclarationLeq p
    join_symb = getSimpleIdentifierFromIdentifier $ latticeDeclarationJoin p
    meet_symb = getSimpleIdentifierFromIdentifier $ latticeDeclarationMeet p
    insertLatticeInfo e =
      -- do not insert if already exists
      case e ^. latticeInfos . at repr of
        Just _ -> e
        Nothing -> e & latticeInfos . at repr ?~ p
    -- unconditionally insert the partial order (i.e., join it with
    -- whatever is inside, just in case it was intiialized as being
    -- discrete)
    insertRelationParamKindInfo = kindInfos . at repr ?~ Lattice

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
-- @since 26.7
validateLattice :: SymbolState σ => SymbolValidator σ LatticeDeclaration
validateLattice = validate r
  where
    r =
      [ againstOtherPartialOrds
      , againstOtherLattices
      , againstRelations
      ]

    againstOtherPartialOrds =
      validateNamed
        ( validateAgainstPartialOrd
            "a partial ord declaration"
            [Note "lat/partial ord declarations must have distinct names"]
        )

    againstOtherLattices =
      validateNamed
        ( validateAgainstLattice
            "a lat declaration"
            [Note "lat ord declarations must have distinct names"]
        )

    againstRelations =
      validateNamed
        (validateAgainstRelation "a partial ord declaration with the same name")
