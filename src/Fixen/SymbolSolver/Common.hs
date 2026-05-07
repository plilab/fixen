module Fixen.SymbolSolver.Common where

import Control.Monad
import Data.List.NonEmpty
import Data.Set qualified as Set

-- import Fixen.Data.NodeId
import Fixen.IR.AST
import Fixen.Monad

type SymbolState = PositionEnv :*: NodeId :*: FixenErrors

foldMWith :: (Foldable t, Monad m) => (b -> a -> m b) -> t a -> b -> m b
foldMWith f = flip (foldM f)

getAllTypeNames :: Type -> Set.Set SimpleIdentifier
getAllTypeNames (TypeName _ (IdentifierSimpleIdentifier n)) = Set.singleton n
getAllTypeNames (TypeApp _ lhs rhs) = getAllTypeNames lhs <> getAllTypeNames rhs
getAllTypeNames (TypeList _ t) = getAllTypeNames t
getAllTypeNames (TypeTuple _ hd tl) = (getAllTypeNames hd) <> (getAllTypeNamesList (Data.List.NonEmpty.toList tl))
getAllTypeNames _ = Set.empty

getAllTypeNamesList :: [Type] -> Set.Set SimpleIdentifier
getAllTypeNamesList = Set.unions . fmap getAllTypeNames -- Prelude.foldl' (\m t -> m <> (getAllTypeNames t)) Set.empty

getAllExprNames :: Expr -> Set.Set SimpleIdentifier
getAllExprNames (ExprVar _ (IdentifierSimpleIdentifier n)) = Set.singleton n
getAllExprNames (ExprApp _ lhs rhs) = getAllExprNames lhs <> getAllExprNames rhs
getAllExprNames (ExprList _ t) = getAllExprNamesList t
getAllExprNames (ExprTuple _ hd tl) = (getAllExprNames hd) <> (getAllExprNamesList (Data.List.NonEmpty.toList tl))
getAllExprNames _ = Set.empty

getAllExprNamesList :: [Expr] -> Set.Set SimpleIdentifier
getAllExprNamesList = Set.unions . fmap getAllExprNames -- Prelude.foldl' (\m t -> m <> (getAllExprNames t)) Set.empty

getSimpleIdentifierFromIdentifier :: Identifier -> Set.Set SimpleIdentifier
getSimpleIdentifierFromIdentifier (IdentifierSimpleIdentifier i) = Set.singleton i
getSimpleIdentifierFromIdentifier _ = Set.empty
