module Fixen.SymbolSolver.Query where

import Control.Lens
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Validation

-- | Invariant: relations must have been inserted in the environment
initEnvWithQuery :: SymbolEnv -> Query -> FixenPass SymbolState SymbolEnv
initEnvWithQuery env q = do
  _ <- validateQuery q env
  let q_repr = simpleIdentifier $ nameOf q
  -- perform the insertion if it doesn't already exist
  case env ^. infoMap . queryInfoMap . at q_repr of
    Just _ -> return env
    Nothing -> return $ env & infoMap . queryInfoMap . at q_repr ?~ q

validateQuery :: SymbolValidator Query
validateQuery = validate rules
  where
    rules =
      [ againstOtherQueries
      , againstExtern
      , matchRelationArity
      , againstPrelude
      , againstFixen
      ]

    againstOtherQueries =
      validateNamed
        ( validateAgainstQuery
            "another query declaration with the same name"
        )

    againstExtern =
      validateNamed
        (validateAgainstExtern "query declaration" queryValidationErrorNotes)

    againstPrelude =
      validateNamed
        (validateAgainstPreludeLowercase "query declaration" "query declaration")

    againstFixen =
      validateNamed
        (validateAgainstFixenLowercase "query declaration" "query declaration")

    matchRelationArity :: SymbolRule Query
    matchRelationArity q env =
      let rel = queryRel q
       in relationExistsAndHasRightArity rel "query" env
