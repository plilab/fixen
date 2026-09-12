{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen.Common where

import Data.Char qualified as Char
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.CodeGen.Haskell.Syntax qualified as Hs
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common (isValidOpChar)
import Fixen.Utils

type CodeGenState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

-- | Multi-head and conditional premise-free rules need a retained fact batch.
-- A conditional may select an empty batch even with only one written fact.
-- Ordinary firings retain their bindings and evaluate on dequeue as before.
hasMultiConclusionSeeds :: Foldable f => f RuleInfo -> Bool
hasMultiConclusionSeeds = any (\info -> let r = _ruleDeclaration info in null (ruleAssumptions r) && needsBatch (ruleConclusion r))
  where
    needsBatch (Emit cs) = length cs > 1
    needsBatch _ = True

data CodeGenOptions = CodeGenOptions
  { codeGenDebug :: Bool
  -- ^ Whether to emit runtime debug traces.
  , debugColor :: Bool
  -- ^ Whether debug traces should use ANSI colors.
  }
  deriving (Eq, Show)

codeGenType :: Type -> Text
codeGenType = Hs.renderType . lowerType

-- | Lower source types without manufacturing source node IDs for generated
-- types. Unknown infix type fixities are preserved by the output printer.
lowerType :: Type -> Hs.Type
lowerType (TypeApp _ (TypeApp _ (TypeName _ n) l) r)
  | fullIdentifier n == "->" = Hs.TyArrow (lowerType l) (lowerType r)
  | isOp n = Hs.TyInfix (lowerType l) (lowerName n) (lowerType r)
lowerType (TypeName _ n) = Hs.TyName (lowerName n)
lowerType (TypeApp _ f x) = Hs.TyApp (lowerType f) (lowerType x)
lowerType (TypeList _ t) = Hs.TyList (lowerType t)
lowerType (TypeTuple _ hd tl) = Hs.TyTuple (lowerType <$> hd : NonEmpty.toList tl)
lowerType (TypeNatLit _ n) = Hs.TyInteger (toInteger n)
lowerType (TypeSymbolLit _ s) = Hs.TyString s
lowerType (TypeUnit _) = Hs.TyTuple []
lowerType TypeCpp {} = error "C++ type passed to Haskell generator"

codeGenExpr :: Expr -> Text
codeGenExpr = Hs.renderExpr . lowerExpr

lowerName :: Identifier -> Hs.Name
lowerName i
  | isOp i = Hs.Operator (fullIdentifier i)
  | otherwise = Hs.Name (fullIdentifier i)

lowerExpr :: Expr -> Hs.Expr
lowerExpr = lowerExprWithNames Map.empty

-- | Substitute identifiers before printing. Qualified foreign names are never
-- rewritten. Prefix application preserves the tree for unknown host fixities.
lowerExprWithNames :: Map Text Hs.Name -> Expr -> Hs.Expr
lowerExprWithNames names = go
  where
    go (ExprVar _ i@(IdentifierSimpleIdentifier s)) =
      Hs.Var (Map.findWithDefault (lowerName i) (simpleIdentifier s) names)
    go (ExprVar _ i) = Hs.Var (lowerName i)
    go (ExprApp _ f x) = Hs.App (go f) (go x)
    go (ExprList _ xs) = Hs.List (go <$> xs)
    go (ExprTuple _ hd tl) = Hs.Tuple (go <$> hd : NonEmpty.toList tl)
    go (ExprIntLit _ n) = Hs.IntegerLit n
    go (ExprStrLit _ s) = Hs.StringLit s
    go (ExprUnit _) = Hs.Tuple []
    go ExprCpp {} = error "C++ expression passed to Haskell generator"

isOp :: Identifier -> Bool
isOp i = all isValidOpChar (Text.unpack $ simpleIdentifier i)

dbFactSelector :: Text -> Text
dbFactSelector = Text.append "_facts"

capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, t') -> Text.cons (Char.toUpper c) t'
  Nothing -> t
