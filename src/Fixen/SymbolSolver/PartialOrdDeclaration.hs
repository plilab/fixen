module Fixen.SymbolSolver.PartialOrdDeclaration where

import Control.Lens
import Data.Set qualified as Set
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Extern
import Fixen.SymbolSolver.Validation

initEnvWithPartialOrd :: SymbolEnv -> PartialOrdDeclaration -> FixenPass SymbolState SymbolEnv
initEnvWithPartialOrd env p = do
  validatePartialOrd p env
  env
    & insertPartialOrdInfo
    & insertRelationParamKindInfo
    & foldMWith initEnvWithExternSymbol extern_symbols
  where
    repr = simpleIdentifier $ nameOf p
    extern_symbols = Set.unions [type_symb, leq_symb, mlb_symb]
    type_symb = getAllTypeNames $ partialOrdDeclarationType p
    leq_symb = getSimpleIdentifierFromIdentifier $ partialOrdDeclarationLeq p
    mlb_symb = getSimpleIdentifierFromIdentifier $ partialOrdDeclarationMlbs p
    insertPartialOrdInfo e =
      -- do not insert if already exists
      case e ^. infoMap . partialOrdInfoMap . at repr of
        Just _ -> e
        Nothing ->
          e
            & infoMap
              . partialOrdInfoMap
              . at repr
              ?~ p
    insertRelationParamKindInfo =
      -- unconditionally insert the partial order (i.e., join it with
      -- whatever is inside, just in case it was intiialized as being
      -- discrete)
      infoMap
        . relationParamKindInfoMap
        . at repr
        ?~ PartiallyOrdered

validatePartialOrd :: SymbolValidator PartialOrdDeclaration
validatePartialOrd = validate rules
  where
    rules =
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
