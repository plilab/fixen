module Parsing where

import Control.Monad
import qualified Data.List as L
import Data.Void
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Syntax.Common
import Syntax.Raw
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string, char)
import qualified Text.Megaparsec.Char as C

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

expr :: Parser RawExpr
expr = L.foldl' App <$> expr' <*> many expr'

expr' :: Parser RawExpr
expr' = choice [
  Atom <$> try idExpr,
  Atom <$> try intExpr,
  brackExpr]

atomicExpr :: Parser RawAtomExpr
atomicExpr =  try idExpr <|> intExpr

idExpr :: Parser RawAtomExpr
idExpr = Id <$> identifier

intExpr :: Parser RawAtomExpr
intExpr = Ground . LInt . IntLit <$> number

boolExpr :: Parser RawAtomExpr
boolExpr = Ground . LBool . BoolLit <$> 
    ( True  <$ string "True"
  <|> False <$ string "False")

strExpr :: Parser RawAtomExpr
strExpr = 
  Ground . LString . StrLit <$> 
    between "\"" "\"" (many L.charLiteral)

brackExpr :: Parser RawExpr
brackExpr = between (char '(') (char ')') expr

-- Parse a proposition (relation applied to expressions)
proposition :: Parser (Conclusion Identifier)
proposition = Conclusion <$> identifier <*> many expr'

atomicProposition :: Parser (Assumption Identifier)
atomicProposition = Assumption <$> identifier <*> many atomicExpr

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

ruleDecl :: Parser Declaration
ruleDecl = do
  void $ string "rule"
  name <- optional identifier
  void $ char ':'
  lhs <- commaSep atomicProposition
  turnstyle
  Rul name . Rule lhs <$> proposition

latDecl :: Parser Declaration
latDecl = Def . Foreign <$> (string "lat" *> identifier)

relDecl :: Parser Declaration
relDecl = do
  void $ string "rel"
  name <- identifier
  relType <- optional (char ':' *> typeExprList)
  return . Rel $ Signature name (fromMaybe [] relType)

typeExprList :: Parser [TypeExpr]
typeExprList = commaSep typeExpr

ordDecl :: Parser Declaration
ordDecl = do
  void $ string "ord"
  void $ char ':'
  lhs <- commaSep atomicProposition
  turnstyle
  instl <- instantiation
  void $ string "<="
  Ord . Rule lhs . OrdHead instl <$> instantiation

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
  return . Qry $ ModalDef name ruleName modes

declaration :: Parser Declaration
declaration = choice [
  try latDecl,
  try relDecl,
  try ruleDecl,
  try ordDecl,
  queryDecl]

topLevel :: Parser RawProgram
topLevel = RawProgram <$> (between sc eof . some) declaration 

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) RawProgram
parseTopLevel = parse topLevel "input"
