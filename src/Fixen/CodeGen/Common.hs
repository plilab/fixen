{-# LANGUAGE OverloadedStrings #-}

module Fixen.CodeGen.Common where

import Data.Char qualified as Char
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Token (opChars)
import Fixen.Utils

type CodeGenState = SymbolEnv :*: PositionEnv :*: NodeId :*: FixenErrors

codeGenType :: Type -> Text
codeGenType (TypeApp _ (TypeApp _ (TypeName _ n) l) r)
  | isOp n =
      Text.intercalate " " [asAtomic l, codeGenInfixIdentifier n, asAtomic r]
codeGenType (TypeName _ n) = codeGenIdentifier n
codeGenType (TypeApp _ lhs' rhs') =
  let lhs_code = if isInfix lhs' then parenthesize (codeGenType lhs') else codeGenType lhs'
      rhs_code = asAtomic rhs'
   in Text.intercalate " " [lhs_code, rhs_code]
codeGenType (TypeList _ t) = Text.concat ["[", codeGenType t, "]"]
codeGenType (TypeTuple _ hd tl) = Text.concat ["(", codeGenType hd, ", ", (Text.intercalate ", " (codeGenType <$> (NonEmpty.toList tl))), ")"]
codeGenType (TypeNatLit _ i) = Text.show i
codeGenType (TypeSymbolLit _ s) = Text.show s
codeGenType (TypeUnit _) = "()"

codeGenExpr :: Expr -> Text
codeGenExpr (ExprApp _ (ExprApp _ (ExprVar _ n) l) r)
  | isOp n =
      Text.intercalate " " [asAtomic l, codeGenInfixIdentifier n, asAtomic r]
codeGenExpr (ExprVar _ n) = codeGenIdentifier n
codeGenExpr (ExprApp _ lhs' rhs') =
  let lhs_code = if isInfix lhs' then parenthesize (codeGenExpr lhs') else codeGenExpr lhs'
      rhs_code = asAtomic rhs'
   in Text.intercalate " " [lhs_code, rhs_code]
codeGenExpr (ExprList _ ls) = Text.concat ["[", Text.intercalate ", " $ codeGenExpr <$> ls, "]"]
codeGenExpr (ExprTuple _ hd tl) = parenthesize $ Text.intercalate ", " $ codeGenExpr <$> hd : NonEmpty.toList tl
codeGenExpr (ExprIntLit _ i) = Text.show i
codeGenExpr (ExprStrLit _ i) = Text.show i
codeGenExpr (ExprUnit _) = "()"

class Atomic τ where
  isAtomic :: τ -> Bool
  asAtomic :: τ -> Text
  isInfix :: τ -> Bool

codeGenTypeAsAtomic :: Type -> Text
codeGenTypeAsAtomic t
  | isAtomic t = codeGenType t
  | otherwise = parenthesize (codeGenType t)

parenthesize :: Text -> Text
parenthesize t = Text.concat ["(", t, ")"]

-- isInfix :: Type -> Bool
-- isInfix (TypeApp _ (TypeApp _ (TypeName _ n) _) _) = isOp n
-- isInfix _ = False

instance Atomic Type where
  isAtomic :: Type -> Bool
  isAtomic (TypeApp _ _ _) = False
  isAtomic _ = True

  isInfix :: Type -> Bool
  isInfix (TypeApp _ (TypeApp _ (TypeName _ n) _) _) = isOp n
  isInfix _ = False

  asAtomic t
    | isAtomic t = codeGenType t
    | otherwise = parenthesize (codeGenType t)

instance Atomic Expr where
  isAtomic :: Expr -> Bool
  isAtomic (ExprApp _ _ _) = False
  isAtomic _ = True

  isInfix :: Expr -> Bool
  isInfix (ExprApp _ (ExprApp _ (ExprVar _ n) _) _) = isOp n
  isInfix _ = False

  asAtomic e
    | isAtomic e = codeGenExpr e
    | otherwise = parenthesize (codeGenExpr e)

codeGenIdentifier :: Identifier -> Text
codeGenIdentifier i =
  if isOp i
    then Text.concat ["(", fullIdentifier i, ")"]
    else fullIdentifier i

isOp :: Identifier -> Bool
isOp i = any (∈ opChars) (Text.unpack $ simpleIdentifier i)

codeGenInfixIdentifier :: Identifier -> Text
codeGenInfixIdentifier i =
  if any (∈ opChars) (Text.unpack $ simpleIdentifier i)
    then fullIdentifier i
    else Text.concat ["`", fullIdentifier i, "`"]

buildSimpleType :: Text -> Type
buildSimpleType t = TypeName (-1) (MkIdentifierSimple (-1) t)

dbFactSelector :: Text -> Text
dbFactSelector = Text.append "_facts"

capitalize :: Text -> Text
capitalize t = case Text.uncons t of
  Just (c, t') -> Text.cons (Char.toUpper c) t'
  Nothing -> t

codeGenExprWithNameReplacement :: IntMap Int -> Map Text Int -> Expr -> Text
codeGenExprWithNameReplacement name_supply var_map (ExprVar _ (IdentifierSimpleIdentifier (SimpleIdentifier _ i)))
  | i `Map.member` var_map =
      let c_v = var_map Map.! i
       in Text.concat ["_v", Text.show c_v, "_", Text.show $ name_supply IntMap.! c_v]
codeGenExprWithNameReplacement _ _ (ExprVar _ i) = codeGenIdentifier i
codeGenExprWithNameReplacement name_supply var_map (ExprApp _ lhs' rhs') =
  Text.concat ["(", codeGenExprWithNameReplacement name_supply var_map lhs', " ", codeGenExprWithNameReplacement name_supply var_map rhs', ")"]
codeGenExprWithNameReplacement _ _ (ExprIntLit _ i) = Text.show i
codeGenExprWithNameReplacement _ _ (ExprStrLit _ s) = Text.show s
codeGenExprWithNameReplacement name_supply var_map (ExprTuple _ hd tl) =
  let hd' = codeGenExprWithNameReplacement name_supply var_map hd
      tl' = codeGenExprWithNameReplacement name_supply var_map <$> NonEmpty.toList tl
      comp = Text.intercalate ", " $ hd' : tl'
   in Text.concat ["(", comp, ")"]
codeGenExprWithNameReplacement name_supply var_map (ExprList _ ls) =
  let comp = codeGenExprWithNameReplacement name_supply var_map <$> ls
      comp_code = Text.intercalate ", " comp
   in Text.concat ["[", comp_code, "]"]
codeGenExprWithNameReplacement _ _ (ExprUnit _) = "()"
