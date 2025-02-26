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
  then fail $ "'" ++ idx ++ "' is a reserved keyword"
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
typeExpr = (TNat <$ string "Nat") 
  <|> (TString <$ string "String") 
  <|> (TBool <$ string "Bool") 
  <|> (TVar <$> identifier)

expr :: Parser LitExpr
expr = L.foldl' App <$> expr' <*> many expr'

expr' :: Parser LitExpr
expr' = choice [
  try idExpr,
  try intExpr,
  brackExpr]

idExpr :: Parser LitExpr
idExpr = Atom . Id <$> identifier

intExpr :: Parser LitExpr
intExpr = Atom . Ground . GroundExpr . LInt <$> number

brackExpr :: Parser LitExpr
brackExpr = between (char '(') (char ')') expr

-- Parse a proposition (relation applied to expressions)
proposition :: Parser (Fact LitExpr)
proposition = Fact <$> identifier <*> many expr'

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

ruleDecl :: Parser LitDeclaration
ruleDecl = do
  void $ string "rule"
  name <- optional identifier
  void $ char ':'
  lhs <- commaSep proposition
  turnstyle
  RuleDecl name . Rule lhs <$> proposition

latDecl :: Parser LitDeclaration
latDecl = LatDecl <$> (string "lat" *> identifier)

relDecl :: Parser LitDeclaration
relDecl = do
  void $ string "rel"
  name <- identifier
  relType <- optional (char ':' *> typeExprList)
  return $ RelDecl name (fromMaybe [] relType)

typeExprList :: Parser [TypeExpr]
typeExprList = commaSep typeExpr

ordDecl :: Parser LitDeclaration
ordDecl = do
  void $ string "ord"
  void $ char ':'
  lhs <- commaSep proposition
  turnstyle
  instl <- instantiation
  void $ string "<="
  OrdDecl lhs instl <$> instantiation

instantiation :: Parser LitInstantiation
instantiation = do
  name <- identifier
  let assigned = (,) <$> identifier <* char '=' <*> expr
  insts <- between (char '{') (char '}') (commaSep assigned)
  if null insts
  then fail "empty instantiation"
  else return $ Instantiation name insts

queryDecl :: Parser LitDeclaration
queryDecl = do
  name <- string "query" *> identifier <* string "as"
  ruleName <- identifier
  modes <- many $ (True <$ char '+') <|> (False <$ char '-')
  return $ QueryDecl name ruleName modes

declaration :: Parser LitDeclaration
declaration = choice [
  try latDecl,
  try relDecl,
  try ruleDecl,
  try ordDecl,
  queryDecl]

topLevel :: Parser LitProgram
topLevel = Program <$> (between sc eof . some) declaration 

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) LitProgram
parseTopLevel = parse topLevel "input"

test :: Text -> IO ()
test = parseTest topLevel
