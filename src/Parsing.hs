{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import qualified Data.List as L
import Data.Void
import Data.Text (Text)
import Syntax
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string, char)
import qualified Text.Megaparsec.Char as C
import Data.Maybe (fromMaybe)
import Control.Monad (void)

type Parser = Parsec Void Text

reserved :: [Identifier]
reserved = ["lat", "rule", "rel", "ord", "query"]

-- Lexing rules
-- Consume spaces and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- Lexeme parsers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse an identifier
identifier :: Parser Identifier
identifier = do
  idx <- lexeme ((:) <$> letterChar <*> many alphaNumChar)
  if idx `elem` reserved
  then fail $ "'" ++ idx ++ " is a reserved keyword"
  else return idx

string :: Text -> Parser Text
string = lexeme . C.string

char :: Char -> Parser Char
char = lexeme . C.char

number :: Parser Int
number = lexeme (read <$> some digitChar)

turnstyle :: Parser ()
turnstyle = void $ string "|->" <|> string "|-"

typeExpr :: Parser TypeExpr
typeExpr = (TNat <$ string "Nat") <|> (TDist <$ string "Dist")

-- parse an expression first (head), then check if there are more (it is application) remembering the head
-- if there are more then its app, else return the expr
expr :: Parser Expr
expr = L.foldl' App <$> expr' <*> many expr'

expr' :: Parser Expr
expr' = choice [
  try idExpr,
  try intExpr,
  brackExpr]

idExpr :: Parser Expr
idExpr = Atom . Id <$> identifier

intExpr :: Parser Expr
intExpr = Atom . Int <$> number

brackExpr :: Parser Expr
brackExpr = between (char '(') (char ')') expr

-- Parse a proposition (relation applied to expressions)
proposition :: Parser Proposition
proposition = (,) <$> identifier <*> many expr'

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

ruleDecl :: Parser Declaration
ruleDecl = do
  void $ string "rule"
  name <- optional identifier
  void $ char ':'
  lhs <- commaSep proposition
  turnstyle
  RuleDecl name . Rule lhs <$> proposition

latDecl :: Parser Declaration
latDecl = LatDecl <$> (string "lat" *> identifier)

relDecl :: Parser Declaration
relDecl = do
  void $ string "rel"
  name <- identifier
  relType <- optional (char ':' *> typeExprList)
  return $ RelDecl name (fromMaybe [] relType)

typeExprList :: Parser [TypeExpr]
typeExprList = commaSep typeExpr

ordDecl :: Parser Declaration
ordDecl = do
  void $ string "ord"
  void $ char ':'
  lhs <- commaSep proposition
  turnstyle
  instl <- instantiation
  void $ string "<="
  OrdDecl lhs instl <$> instantiation

instantiation :: Parser Instantiation
instantiation = do
  name <- identifier
  let assigned = (,) <$> identifier <* char '=' <*> expr
  insts <- between (char '{') (char '}') (commaSep assigned)
  if null insts
  then fail "empty instantiation"
  else return $ Instantiation name insts

queryDecl :: Parser Declaration
queryDecl = do
  name <- string "query" *> identifier <* string "as"
  ruleName <- identifier
  modes <- many $ (True <$ char '+') <|> (False <$ char '-')
  return $ QueryDecl name ruleName modes

topLevel :: Parser Program
topLevel = do
  void sc
  res <- some $ choice [
    try latDecl,
    try relDecl,
    try ruleDecl,
    try ordDecl,
    queryDecl]
  void eof
  return $ Program res

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) Program
parseTopLevel = parse topLevel "input"

test :: Text -> IO ()
test = parseTest topLevel
