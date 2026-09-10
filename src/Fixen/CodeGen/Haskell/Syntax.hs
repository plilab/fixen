{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The subset of Haskell that Fixen generates. This is an output language,
-- not a parser for user Haskell. Embedded source is assembled separately.
--
-- Expressions, patterns and types are deliberately distinct. Names are still
-- checked by GHC; these types prevent syntactic category and layout mistakes,
-- not ill-typed programs or variable capture.
module Fixen.CodeGen.Haskell.Syntax where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.Parser.Common (isValidOpChar)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

data Name = Name Text | Operator Text
  deriving (Eq, Show)

data Expr
  = Var Name
  | App Expr Expr
  | Infix Expr Name Expr
  | IntegerLit Integer
  | StringLit Text
  | Tuple [Expr]
  | List [Expr]
  | Lambda (NonEmpty Pattern) Expr
  | Let (NonEmpty Decl) Expr
  | If Expr Expr Expr
  | Case Expr (NonEmpty (Pattern, Expr))
  | -- The final expression is separate so an empty do block or a trailing bind
    -- cannot be constructed.
    Do [Stmt] Expr
  | Record Name [(Name, Expr)]
  | Update Expr [(Name, Expr)]
  deriving (Eq, Show)

data Pattern
  = PVar Name
  | PWildcard
  | PCon Name [Pattern]
  | PTuple [Pattern]
  | PList [Pattern]
  | PAs Name Pattern
  deriving (Eq, Show)

data Type
  = TyName Name
  | TyApp Type Type
  | TyInfix Type Name Type
  | TyArrow Type Type
  | TyTuple [Type]
  | TyList Type
  | TyInteger Integer
  | TyString Text
  deriving (Eq, Show)

data Stmt
  = Bind Pattern Expr
  | LetStmt Pattern Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data Constructor
  = Constructor Name [Type]
  | RecordConstructor Name [(Name, Type)]
  deriving (Eq, Show)

data Decl
  = Signature Name Type
  | Function Name [Pattern] Expr
  | Binding Pattern Expr
  | Data Name (NonEmpty Constructor) [Name]
  | TypeAlias Name Type
  | Instance Type [Decl]
  deriving (Eq, Show)

-- | Qualified symbolic identifiers retain their qualification, e.g.
-- @HashMap.!?@ becomes @(HashMap.!?)@ in prefix position.
name :: Text -> Name
name t = case Text.unsnoc t of
  Just (_, c) | isValidOpChar c -> Operator t
  _ -> Name t

var :: Text -> Expr
var = Var . name

pat :: Text -> Pattern
pat "_" = PWildcard
pat t = PVar (name t)

typ :: Text -> Type
typ = TyName . name

call :: Text -> [Expr] -> Expr
call f = apps (var f)

apps :: Expr -> [Expr] -> Expr
apps = foldl App

functionType :: [Type] -> Type -> Type
functionType args result = foldr TyArrow result args

-- | Database suffixes of arity one are stored without a tuple wrapper.
tuple :: [Expr] -> Expr
tuple [x] = x
tuple xs = Tuple xs

tuplePattern :: [Pattern] -> Pattern
tuplePattern [x] = x
tuplePattern xs = PTuple xs

tupleType :: [Type] -> Type
tupleType [x] = x
tupleType xs = TyTuple xs

guardStmt :: Expr -> Stmt
guardStmt = ExprStmt . call "guard" . pure

andExpr :: [Expr] -> Expr
andExpr [] = var "True"
andExpr xs = foldr1 (\a b -> Infix a (Operator "&&") b) xs

-- | Explicit braces and semicolons make nested blocks independent of the
-- surrounding layout. Soft line breaks are only used inside these blocks.
block :: [Doc ann] -> Doc ann
block [] = lbrace <> rbrace
block docs = group (lbrace <> nest 2 (line <> vsep (punctuate semi docs)) <> line <> rbrace)

prettyName :: Name -> Doc ann
prettyName (Name t) = pretty t
prettyName (Operator t) = parens (pretty t)

prettyInfixName :: Name -> Doc ann
prettyInfixName (Name t) = "`" <> pretty t <> "`"
prettyInfixName (Operator t) = pretty t

prettyExpr :: Expr -> Doc ann
prettyExpr = exprPrec 0

exprPrec :: Int -> Expr -> Doc ann
exprPrec p e = case e of
  Var n -> prettyName n
  App f x -> parensIf (p > 10) (exprPrec 10 f <+> exprPrec 11 x)
  -- Foreign fixities are unknown. Parenthesize every infix node, preserving
  -- the tree even when the host module declares unusual fixities.
  Infix a op b -> parens (exprPrec 1 a <+> prettyInfixName op <+> exprPrec 1 b)
  IntegerLit n -> parensIf (n < 0) (pretty n)
  StringLit t -> prettyString t
  Tuple xs -> tupled (prettyExpr <$> xs)
  List xs -> list (prettyExpr <$> xs)
  Lambda ps body -> parensIf (p > 0) ("\\" <> hsep (patternPrec 11 <$> NonEmpty.toList ps) <+> "->" <+> prettyExpr body)
  Let ds body -> parensIf (p > 0) ("let" <+> block (prettyDecl <$> NonEmpty.toList ds) <+> "in" <+> prettyExpr body)
  If cond yes no -> parensIf (p > 0) ("if" <+> prettyExpr cond <+> "then" <+> prettyExpr yes <+> "else" <+> prettyExpr no)
  Case scrutinee alts -> parensIf (p > 0) ("case" <+> prettyExpr scrutinee <+> "of" <+> block [prettyPattern lhs <+> "->" <+> prettyExpr rhs | (lhs, rhs) <- NonEmpty.toList alts])
  Do stmts result -> parensIf (p > 0) ("do" <+> block ((prettyStmt <$> stmts) ++ [prettyExpr result]))
  Record n fields -> prettyName n <+> recordFields fields
  Update base fields -> parensIf (p > 10) (exprPrec 11 base <+> recordFields fields)
  where
    recordFields fields = encloseSep lbrace rbrace (comma <> space) [prettyName n <+> "=" <+> prettyExpr x | (n, x) <- fields]

prettyPattern :: Pattern -> Doc ann
prettyPattern = patternPrec 0

patternPrec :: Int -> Pattern -> Doc ann
patternPrec p = \case
  PVar n -> prettyName n
  PWildcard -> "_"
  PCon n ps -> parensIf (p > 10 && not (null ps)) (hsep (prettyName n : (patternPrec 11 <$> ps)))
  PTuple ps -> tupled (prettyPattern <$> ps)
  PList ps -> list (prettyPattern <$> ps)
  PAs n pat' -> prettyName n <> "@" <> patternPrec 11 pat'

prettyType :: Type -> Doc ann
prettyType = typePrec 0

typePrec :: Int -> Type -> Doc ann
typePrec p = \case
  TyName n -> prettyName n
  TyApp f x -> parensIf (p > 10) (typePrec 10 f <+> typePrec 11 x)
  TyInfix a op b -> parens (typePrec 1 a <+> prettyInfixName op <+> typePrec 1 b)
  TyArrow a b -> parensIf (p > 0) (typePrec 1 a <+> "->" <+> prettyType b)
  TyTuple xs -> tupled (prettyType <$> xs)
  TyList x -> brackets (prettyType x)
  TyInteger n -> pretty n
  TyString t -> prettyString t

prettyStmt :: Stmt -> Doc ann
prettyStmt = \case
  Bind lhs rhs -> prettyPattern lhs <+> "<-" <+> prettyExpr rhs
  LetStmt lhs rhs -> "let" <+> block [prettyPattern lhs <+> "=" <+> prettyExpr rhs]
  ExprStmt x -> prettyExpr x

prettyConstructor :: Constructor -> Doc ann
prettyConstructor = \case
  Constructor n args -> hsep (prettyName n : (typePrec 11 <$> args))
  RecordConstructor n fields -> prettyName n <+> encloseSep lbrace rbrace (comma <> space) [prettyName f <+> "::" <+> prettyType t | (f, t) <- fields]

prettyDecl :: Decl -> Doc ann
prettyDecl = \case
  Signature n t -> prettyName n <+> "::" <+> prettyType t
  Function n args body -> group (hsep (prettyName n : (patternPrec 11 <$> args)) <+> "=" <> nest 2 (line <> prettyExpr body))
  Binding lhs body -> prettyPattern lhs <+> "=" <+> prettyExpr body
  Data n cs derives ->
    "data"
      <+> prettyName n
      <+> "="
      <+> align (concatWith (\a b -> a <> line <> "|" <+> b) (prettyConstructor <$> NonEmpty.toList cs))
      <> if null derives then mempty else space <> "deriving" <+> tupled (prettyName <$> derives)
  TypeAlias n t -> "type" <+> prettyName n <+> "=" <+> prettyType t
  Instance t ds -> "instance" <+> prettyType t <+> "where" <+> block (prettyDecl <$> ds)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

prettyString :: Text -> Doc ann
prettyString = pretty . show . Text.unpack

render :: Doc ann -> Text
render = renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 100 1))

-- | Inline rendering is used at the boundary with fixed source templates.
-- Explicit block delimiters make this safe for all generated expressions.
renderInline :: Doc ann -> Text
renderInline = renderStrict . layoutPretty (LayoutOptions Unbounded)

renderExpr :: Expr -> Text
renderExpr = renderInline . prettyExpr

renderType :: Type -> Text
renderType = renderInline . prettyType

renderDecls :: [Decl] -> Text
renderDecls = render . vsep . fmap (nest 2 . prettyDecl)
