{-# LANGUAGE OverloadedStrings #-}

-- | The small subset of C++ syntax emitted by Fixen. Host code is kept out
-- of this tree; expressions always preserve their original grouping.
module Fixen.CodeGen.Cpp.Syntax where

import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

data Expr
  = Name Text
  | Call Expr [Expr]
  | Construct Text [Expr]
  | Binary Text Expr Expr
  | Unary Text Expr
  | Member Expr Text
  | Index Expr Expr
  | Conditional Expr Expr Expr
  deriving (Eq, Show)

data Stmt
  = Statement Expr
  | Declare Text Text Expr
  | Return Expr
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | For Text Expr [Stmt]
  | Scope [Stmt]
  deriving (Eq, Show)

expr :: Expr -> Doc ann
expr (Name n) = pretty n
expr (Call f xs) = expr f <> tupled (expr <$> xs)
expr (Construct t xs) = pretty t <> braces (commaSep (expr <$> xs))
expr (Binary op a b) = parens (expr a <+> pretty op <+> expr b)
expr (Unary op a) = parens (pretty op <> expr a)
expr (Member a n) = expr a <> dot <> pretty n
expr (Index a i) = expr a <> brackets (expr i)
expr (Conditional c a b) = parens (expr c <+> "?" <+> expr a <+> colon <+> expr b)

stmt :: Stmt -> Doc ann
stmt (Statement e) = expr e <> semi
-- Shared forest branches can introduce bindings unused by a particular leaf.
stmt (Declare t n e) = "[[maybe_unused]]" <+> pretty t <+> pretty n <+> "=" <+> expr e <> semi
stmt (Return e) = "return" <+> expr e <> semi
stmt (If c ss) = block ("if" <+> condition c) (stmt <$> ss)
stmt (IfElse c yes no) = block ("if" <+> condition c) (stmt <$> yes) <+> block "else" (stmt <$> no)
stmt (For n xs ss) = block ("for" <+> parens ("[[maybe_unused]]" <+> pretty n <+> colon <+> expr xs)) (stmt <$> ss)
stmt (Scope ss) = block mempty (stmt <$> ss)

-- Binary/unary expressions already have their own outer parentheses. Adding
-- another pair around an equality condition triggers Clang's -Wparentheses.
condition :: Expr -> Doc ann
condition c@Binary {} = expr c
condition c@Unary {} = expr c
condition c = parens (expr c)

block :: Doc ann -> [Doc ann] -> Doc ann
block header body = header <+> lbrace <> hardline <> indent 2 (vsep body) <> hardline <> rbrace

commaSep :: [Doc ann] -> Doc ann
commaSep = hsep . punctuate comma

function :: Text -> Text -> [Text] -> [Stmt] -> Doc ann
function result n args = block (pretty result <+> pretty n <> tupled [("[[maybe_unused]]" <+> pretty arg) | arg <- args]) . fmap stmt

call :: Text -> [Expr] -> Expr
call = Call . Name

andExpr :: [Expr] -> Expr
andExpr [] = Name "true"
andExpr xs = foldr1 (Binary "&&") xs

render :: [Doc ()] -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . vsep . punctuate hardline
