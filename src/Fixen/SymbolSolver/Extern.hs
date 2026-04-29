module Fixen.SymbolSolver.Extern where

import Control.Lens
import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad
import Fixen.SymbolSolver.Common
import Fixen.SymbolSolver.Validation

initEnvWithExternSymbol :: SymbolEnv -> SimpleIdentifier -> FixenPass SymbolState SymbolEnv
initEnvWithExternSymbol env i = do
  -- Proceed with the insertion
  let symb_id = simpleIdentifier i
      node_id = getNodeId i
  -- check if it already exists; if it does, ignore it.
  case env ^. infoMap . externInfoMap . at symb_id of
    Just _ -> return env
    Nothing -> do
      -- validate if this extern symbol is okay to insert
      validateExternSymbol i env
      return $
        env
          & infoMap
            . externInfoMap
            . at symb_id
            ?~ node_id

validateExternSymbol :: SymbolValidator SimpleIdentifier
validateExternSymbol = validate rules
  where
    rules = [againstRelations, againstQueries]
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
