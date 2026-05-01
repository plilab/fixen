module Fixen.SymbolSolver.Extern where

import Control.Lens
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Validation
import Prelude.Unicode

initEnvWithExternSymbol :: SymbolEnv -> SimpleIdentifier -> FixenPass SymbolState SymbolEnv
initEnvWithExternSymbol env i = do
  let symb_id = simpleIdentifier i
      node_id = getNodeId i
  -- check if it already exists; if it does, ignore it.
  case env ^. externMap ∘ at symb_id of
    Just _ -> return env
    Nothing -> do
      -- validate if this extern symbol is okay to insert
      _ <- validateExternSymbol i env
      return $
        env
          & externMap
            ∘ at symb_id
            ?~ node_id

validateExternSymbol :: SymbolValidator SimpleIdentifier
validateExternSymbol = validate rules
  where
    rules =
      [ againstRelations
      , againstQueries
      , againstFixenUpperCase
      , againstFixenLowerCase
      , againstRuleParams
      ]
    againstRelations :: SymbolRule SimpleIdentifier
    againstRelations i env =
      validateAgainstRelation
        "an existing use of the same name"
        (simpleIdentifier i)
        i
        env
    againstQueries i env =
      validateAgainstQuery
        "an existing use of the same name"
        (simpleIdentifier i)
        i
        env
    againstFixenUpperCase i env =
      validateAgainstFixenCapitalized
        "external symbol"
        "external symbol"
        (simpleIdentifier i)
        i
        env
    againstFixenLowerCase i env =
      validateAgainstFixenLowercase
        "external symbol"
        "external symbol"
        (simpleIdentifier i)
        i
        env
    againstRuleParams i env =
      warnNameShadowingAgainstBoundVar
        "use of the same name"
        (simpleIdentifier i)
        i
        env
