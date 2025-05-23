module Parsing ( parseTopLevel ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void
import Syntax.Common
import Syntax.Raw
    ( Declaration(..), RawAtomExpr, RawExpr, RawProgram(RawProgram) )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char hiding (string, char)
import qualified Data.List as L ( intercalate )
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void Text

reserved :: [Identifier]
reserved = ["module", "where", "import", "data", "rule", "rel", "priority", "query"]

-- Lexing rules
-- Consume spaces and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

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

capitalIdentifier :: Parser Identifier
capitalIdentifier = 
  lexeme ((:) <$> upperChar <*> many alphaNumChar)

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
expr = App <$> identifier <*> many expr'

expr' :: Parser RawExpr
expr' = choice [
    Atom <$> atomicExpr,
    brackExpr
  ]

atomicExpr :: Parser RawAtomExpr
atomicExpr =  try consExpr <|> try idExpr <|> try intExpr <|> strExpr{- boolExpr <|>  -}

idExpr :: Parser RawAtomExpr
idExpr = Id . Variable <$> identifier

intExpr :: Parser RawAtomExpr
intExpr = Ground . LInt <$> number

{- boolExpr :: Parser RawAtomExpr
boolExpr = Ground . LBool <$> 
    ( True  <$ string "True"
  <|> False <$ string "False") -}

consExpr :: Parser RawAtomExpr
consExpr = Ground . LCons <$> capitalIdentifier

strExpr :: Parser RawAtomExpr
strExpr = 
  Ground . LString <$> 
    between (char '"') (char '"') (many C.letterChar)

brackExpr :: Parser RawExpr
brackExpr = between (char '(') (char ')') expr

-- Parse a proposition (relation applied to expressions)
proposition :: Parser (Conclusion Var)
proposition = Conclusion <$> identifier <*> many expr'

atomicProposition :: Parser (Assumption Var)
atomicProposition = Assumption <$> identifier <*> many atomicExpr

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

moduleName :: Parser ModuleName
moduleName = L.intercalate "." <$> sepBy capitalIdentifier (C.char '.')

moduleDecl :: Parser Module
moduleDecl = string "module" *> fmap Module moduleName <* string "where"

importDecl :: Parser Declaration
importDecl = do
  void $ string "import"
  modName <- moduleName
  guard . not . null $ modName
  return . Imp . Import $ modName

ruleDecl :: Parser Declaration
ruleDecl = do
  void $ string "rule"
  name <- optional identifier
  void $ char ':'
  lhs <- commaSep atomicProposition
  turnstyle
  Rul name . Rule lhs <$> proposition

latDecl :: Parser Declaration
latDecl = Def . Foreign <$> (string "data" *> identifier)

relDecl :: Parser Declaration
relDecl = do
  void $ string "rel"
  name <- identifier
  relType <- optional (char ':' *> typeExprList)
  return . Rel $ Signature name (fromMaybe [] relType)

typeExprList :: Parser [TypeExpr]
typeExprList = commaSep typeExpr

priorityDecl :: Parser Declaration
priorityDecl = do
  void $ string "priority"
  void $ char ':'
  lhs <- commaSep atomicProposition
  turnstyle
  instl <- instantiation
  void $ string "<"
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
  try importDecl,
  try latDecl,
  try relDecl,
  try ruleDecl,
  try priorityDecl,
  queryDecl]

topLevel :: Parser RawProgram
topLevel = RawProgram <$> moduleDecl <*> (between sc eof . some) declaration 

parseTopLevel :: Text -> Either (ParseErrorBundle Text Void) RawProgram
parseTopLevel = parse topLevel "input"
