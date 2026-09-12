-- |
-- Module      : Fixen.SymbolSolver.Common
-- Description : Dumping ground for utilities of the symbol solver
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides miscellaneous definitions used by the symbol solver.
--
-- @since 26.7
module Fixen.SymbolSolver.Common where

import Control.Lens
import Data.List.NonEmpty
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Fixen.Fields (latticeInfos, partialOrdInfos)
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Utils

--------------------------------------------------------------------------------

-- * Symbol-solving states

--------------------------------------------------------------------------------

-- | The state carried by the symbol solver.
--
--   This must be a product of at least three components:
--
--   * 'PositionEnv' — maps 'NodeId' values to source positions
--   * 'FixenErrors' — accumulated error diagnostics
--
-- @since 26.7
type SymbolState σ = (WithPositionEnv σ, WithErrors σ) -- PositionEnv :*: NodeId :*: FixenErrors

-- | The missing operation and its declaration name, if an ordered type has
-- no refinement operation. Discrete types do not require one.
missingRefinement :: SymbolEnv -> Type -> Maybe (String, SimpleIdentifier)
missingRefinement env t
  | Just p <- env ^. partialOrdInfos . at representative
  , isNothing (partialOrdDeclarationMlbs p) =
      Just ("mlbs", partialOrdDeclarationName p)
  | Just l <- env ^. latticeInfos . at representative
  , isNothing (latticeDeclarationMeet l) =
      Just ("meet", latticeDeclarationName l)
  | otherwise = Nothing
  where
    representative = calculateRepresentativeFromType t

--------------------------------------------------------------------------------

-- * Getting all 'SimpleIdentifier's

--------------------------------------------------------------------------------

-- | Obtains all simple identifiers within a type. This excludes any
-- fully qualified names.
--
-- @since 26.7
getAllTypeNames :: Type -> Set.Set SimpleIdentifier
getAllTypeNames (TypeName _ (IdentifierSimpleIdentifier n)) = Set.singleton n
getAllTypeNames (TypeApp _ lhs rhs) = getAllTypeNames lhs ∪ getAllTypeNames rhs
getAllTypeNames (TypeList _ t) = getAllTypeNames t
getAllTypeNames (TypeTuple _ hd tl) = (getAllTypeNames hd) ∪ (getAllTypeNamesList (toList tl))
getAllTypeNames _ = Set.empty

-- | Obtains all simple identifiers within a list of types. This excludes any
-- fully qualified names.
--
-- @since 26.7
getAllTypeNamesList :: [Type] -> Set.Set SimpleIdentifier
getAllTypeNamesList = Set.unions . fmap getAllTypeNames

-- | Obtains all simple identifiers within an expression. This excludes any
-- fully qualified names.
--
-- @since 26.7
getAllExprNames :: Expr -> Set.Set SimpleIdentifier
getAllExprNames (ExprVar _ (IdentifierSimpleIdentifier n)) = Set.singleton n
getAllExprNames (ExprApp _ lhs rhs) = getAllExprNames lhs ∪ getAllExprNames rhs
getAllExprNames (ExprCpp _ _ xs) = getAllExprNamesList xs
getAllExprNames (ExprList _ t) = getAllExprNamesList t
getAllExprNames (ExprTuple _ hd tl) = (getAllExprNames hd) ∪ (getAllExprNamesList (toList tl))
getAllExprNames _ = Set.empty

-- | Obtains all simple identifiers within a list of expressions. This excludes
-- any fully qualified names.
--
-- @since 26.7
getAllExprNamesList :: [Expr] -> Set.Set SimpleIdentifier
getAllExprNamesList = (⋃) . fmap getAllExprNames

-- | Obtains only simple identifiers from an identifier. Fully qualified names
-- are excluded.
--
-- @since 26.7
getSimpleIdentifierFromIdentifier :: Identifier -> Set.Set SimpleIdentifier
getSimpleIdentifierFromIdentifier (IdentifierSimpleIdentifier i) = Set.singleton i
getSimpleIdentifierFromIdentifier _ = Set.empty
