{-# LANGUAGE OverloadedStrings #-}

-- | Target-specific C++ frontend. Logical declarations use the shared AST;
-- native expressions retain their operator tree and source-level literals.
-- This deliberately does not parse arbitrary C++ statements/declarators.
module Fixen.Parser.Cpp (parse) where

import Control.Monad (unless, void, when)
import Control.Monad.Combinators.Expr
import Data.Char (isAlpha, isAlphaNum, isAscii, isUpper)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser qualified as Hs
import Fixen.Parser.Common (Parser, ParserState, parsePositioned)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

parse :: ParserState s => FilePath -> Text -> FixenPass s Program
parse path contents = do
  (namespace, header, declarations) <- Hs.fixenParse program path contents
  result <- Hs.partitionTopLevels header declarations
  pure result {programCppNamespace = Just (maybe "" id namespace)}

space :: Parser s ()
space = L.space C.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser s a -> Parser s a
lexeme = L.lexeme space

symbol :: Text -> Parser s Text
symbol = L.symbol space

word :: Text -> Parser s Text
word text = lexeme (P.try (P.chunk text <* P.notFollowedBy (P.satisfy identContinue)))

identContinue :: Char -> Bool
identContinue c = isAscii c && (isAlphaNum c || c == '_')

reserved :: [Text]
reserved = T.words "alignas alignof and and_eq asm auto bitand bitor bool break case catch char char16_t char32_t class compl const constexpr const_cast continue decltype default delete do double dynamic_cast else enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept not not_eq nullptr operator or or_eq private protected public register reinterpret_cast return short signed sizeof static static_assert static_cast struct switch template this thread_local throw true try typedef typeid typename union unsigned using virtual void volatile wchar_t while xor xor_eq"

rawName :: Parser s Text
rawName = lexeme $ P.try $ do
  first <- P.satisfy (\c -> isAscii c && (isAlpha c || c == '_'))
  rest <- P.takeWhileP Nothing identContinue
  let name = T.cons first rest
  when (name `elem` reserved || "fx_" `T.isPrefixOf` name || "__" `T.isInfixOf` name || reservedUnderscore name) $
    fail "reserved C++ identifier (including Fixen's fx_ implementation prefix)"
  pure name
  where
    reservedUnderscore n = case T.unpack n of
      '_' : c : _ -> isUpper c
      _ -> False

simple :: ParserState s => Parser s SimpleIdentifier
simple = parsePositioned $ do
  name <- rawName
  SimpleIdentifier <$> getNewNodeId <*> pure name

qualifiedName :: Parser s Text
qualifiedName = do
  global <- P.optional (symbol "::")
  names <- rawName `P.sepBy1` symbol "::"
  pure (maybe "" id global <> T.intercalate "::" names)

ident :: ParserState s => Parser s Identifier
ident = parsePositioned $ do
  name <- qualifiedName
  i <- getNewNodeId
  pure (IdentifierSimpleIdentifier (SimpleIdentifier i name))

parens, braces :: Parser s a -> Parser s a
parens = P.between (symbol "(") (symbol ")")
braces = P.between (symbol "{") (symbol "}")

arguments :: Parser s a -> Parser s [a]
arguments parser = parens (parser `P.sepBy` symbol ",")

program :: ParserState s => Parser s (Maybe Text, ModuleDeclaration, [Hs.TopLevel])
program = do
  space
  ns <- P.optional (word "namespace" *> qualifiedName <* symbol ";")
  -- The module field is retained for the Haskell AST API. C++ scope is
  -- represented separately, and an absent namespace really is global scope.
  header <- parsePositioned $ do
    i <- getNewNodeId
    j <- getNewNodeId
    k <- getNewNodeId
    pure (ModuleDeclaration i (ModuleName j (SimpleIdentifier k "Main" :| [])))
  declarations <- P.some (topLevel <* P.optional (symbol ";"))
  P.eof
  pure (ns, header, declarations)

topLevel :: ParserState s => Parser s Hs.TopLevel
topLevel =
  P.choice
    [ Hs.TLRelation <$> relation
    , Hs.TLRule <$> rule
    , Hs.TLPartialOrd <$> partialOrder
    , Hs.TLLattice <$> lattice
    , Hs.TLPriority <$> priority
    , Hs.TLQuery <$> query
    , Hs.TLInclude <$> include
    , Hs.TLCppBlock <$> headerInclude
    , Hs.TLCppBlock <$> codeBlock
    , Hs.TLPhases <$> phases
    ]
    P.<?> "C++ Fixen declaration (rel, rule, lat, partial ord, priority, query, include, #include, cpp fence, or phases)"

relation :: ParserState s => Parser s RelationDeclaration
relation = parsePositioned $ do
  _ <- word "rel"
  name <- simple
  params <- maybe [] id <$> P.optional (symbol ":" *> parameter `P.sepBy1` symbol ",")
  i <- getNewNodeId
  pure (RelationLike i name params)
  where
    parameter = parsePositioned $ do
      (name, t) <-
        (parens $ do n <- simple; _ <- symbol ":"; t <- typeExpr; pure (Just n, t))
          P.<|> ((Nothing,) <$> typeExpr)
      i <- getNewNodeId
      pure (RelationParameter i name t)

typeExpr :: ParserState s => Parser s Type
typeExpr = parsePositioned $ do
  name <- parsePositioned $ do
    text <- typeText
    SimpleIdentifier <$> getNewNodeId <*> pure text
  let value = simpleIdentifier name
  i <- getNewNodeId
  if T.all identContinue value && value `notElem` reserved
    then pure (TypeName i (IdentifierSimpleIdentifier name))
    else pure (TypeCpp i value)

-- Named types or built-ins, with nested templates and integer arguments.
-- More complex declarators can be given an alias in a native block/header.
typeText :: Parser s Text
typeText = do
  name <- builtin P.<|> qualifiedName
  args <- P.optional (symbol "<" *> templateArgument `P.sepBy` symbol "," <* symbol ">")
  pure (name <> maybe "" (\xs -> "<" <> T.intercalate ", " xs <> ">") args)
  where
    builtin =
      P.choice
        ( map
            (P.try . builtinWords)
            [ ["unsigned", "long", "long", "int"]
            , ["signed", "long", "long", "int"]
            , ["unsigned", "long", "long"]
            , ["signed", "long", "long"]
            , ["long", "long", "int"]
            , ["unsigned", "long", "int"]
            , ["signed", "long", "int"]
            , ["unsigned", "short", "int"]
            , ["signed", "short", "int"]
            , ["long", "long"]
            , ["long", "double"]
            , ["long", "int"]
            , ["short", "int"]
            , ["unsigned", "long"]
            , ["signed", "long"]
            , ["unsigned", "short"]
            , ["signed", "short"]
            , ["unsigned", "char"]
            , ["signed", "char"]
            , ["unsigned", "int"]
            , ["signed", "int"]
            , ["int"]
            , ["unsigned"]
            , ["signed"]
            , ["long"]
            , ["short"]
            , ["bool"]
            , ["char"]
            , ["char16_t"]
            , ["char32_t"]
            , ["wchar_t"]
            , ["float"]
            , ["double"]
            ]
        )
    builtinWords = fmap (T.intercalate " ") . mapM word
    templateArgument =
      ( lexeme $ do
          sign <- P.optional (P.single '-')
          digits <- P.takeWhile1P Nothing (\c -> c >= '0' && c <= '9')
          pure (maybe "" T.singleton sign <> digits)
      )
        P.<|> typeText

rule :: ParserState s => Parser s Rule
rule = parsePositioned $ do
  _ <- word "rule"
  names <- P.many simple
  _ <- symbol ":"
  premises <- premise `P.sepBy` symbol ","
  turnstile
  result <- conclusion
  let (asms, conditions) = Hs.partitionPremises premises
      (name, params) = case names of [] -> (Nothing, []); n : ns -> (Just n, ns)
  i <- getNewNodeId
  pure (Rule i name params asms conditions result)

premise :: ParserState s => Parser s Hs.RulePremise
premise =
  (Hs.RPCondition <$> parsePositioned (do _ <- word "if"; value <- expression; i <- getNewNodeId; pure (Condition i value)))
    P.<|> (Hs.RPAssumption <$> relationApplication simple)

relationApplication :: ParserState s => Parser s a -> Parser s (RelationLike a)
relationApplication arg = parsePositioned $ do
  name <- simple
  args <- arguments arg
  i <- getNewNodeId
  pure (RelationLike i name args)

conclusion :: ParserState s => Parser s Conclusion
conclusion = relationApplication expression

turnstile :: Parser s ()
turnstile = void (symbol "|-" P.<|> symbol "⊢")

partialOrder :: ParserState s => Parser s PartialOrdDeclaration
partialOrder = parsePositioned $ do
  _ <- word "partial"
  _ <- word "ord"
  name <- simple
  _ <- word "where"
  t <- orderField "type" typeExpr
  leq <- orderField "leq" operation
  mlbs <- orderField "mlbs" operation
  i <- getNewNodeId
  pure (PartialOrdDeclaration i name t leq mlbs)

lattice :: ParserState s => Parser s LatticeDeclaration
lattice = parsePositioned $ do
  _ <- word "lat"
  name <- simple
  _ <- word "where"
  t <- orderField "type" typeExpr
  leq <- orderField "leq" operation
  join <- orderField "join" operation
  meet <- orderField "meet" operation
  i <- getNewNodeId
  pure (LatticeDeclaration i name t leq join meet)

orderField :: Text -> Parser s a -> Parser s a
orderField name value = word name *> symbol "=" *> value <* P.optional (symbol ";")

operation :: ParserState s => Parser s Identifier
operation =
  ident
    P.<|> parens
      ( parsePositioned $ do
          name <- P.choice (map operator ["<=", ">=", "==", "!=", "<", ">", "+", "-", "*", "/", "%", "&&", "||"])
          i <- getNewNodeId
          pure (IdentifierSimpleIdentifier (SimpleIdentifier i name))
      )

priority :: ParserState s => Parser s Priority
priority = parsePositioned $ do
  _ <- word "priority"
  _ <- symbol ":"
  condition <- expression
  turnstile
  result <- parsePositioned $ do
    left <- ruleInstance
    void (symbol "<" P.<|> symbol "⊏")
    right <- ruleInstance
    i <- getNewNodeId
    pure (PriorityConclusion i left right)
  i <- getNewNodeId
  pure (Priority i condition result)
  where
    ruleInstance = parsePositioned $ do
      name <- simple
      substitutions <- braces ((do key <- simple; _ <- symbol "="; value <- simple; pure (key, value)) `P.sepBy` symbol ",")
      let keys = map (simpleIdentifier . fst) substitutions
      unless (length keys == length (nub keys)) (fail "duplicate priority substitution")
      i <- getNewNodeId
      pure (RuleInstance i name (Map.fromList substitutions))

query :: ParserState s => Parser s Query
query = parsePositioned $ do
  _ <- word "query"
  name <- simple
  _ <- symbol ":"
  rel <-
    relationApplication
      ( parsePositioned $ do
          mode <- (symbol "+" *> pure Input) P.<|> (symbol "-" *> pure Output)
          mode <$> getNewNodeId
      )
  i <- getNewNodeId
  pure (Query i rel name)

include :: ParserState s => Parser s Include
include = parsePositioned $ do
  _ <- word "include"
  path <- lexeme (P.between (C.char '"') (C.char '"') (P.takeWhile1P Nothing (\c -> c /= '"' && c /= '\n' && c /= '\r')))
  i <- getNewNodeId
  pure (Include i path)

headerInclude :: ParserState s => Parser s CppBlock
headerInclude = parsePositioned $ do
  _ <- C.char '#'
  C.hspace
  _ <- P.chunk "include"
  C.hspace
  open <- C.char '<' P.<|> C.char '"'
  let close = if open == '<' then '>' else '"'
  path <- P.takeWhile1P Nothing (\c -> c /= close && c /= '\n' && c /= '\r')
  _ <- C.char close
  C.hspace
  void (P.optional (L.skipLineComment "//"))
  void C.eol P.<|> P.eof
  space
  i <- getNewNodeId
  pure (CppBlock i False ("#include " <> T.singleton open <> path <> T.singleton close <> "\n"))

codeBlock :: ParserState s => Parser s CppBlock
codeBlock = parsePositioned $ do
  _ <- P.chunk "```cpp"
  footer <- maybe False (const True) <$> P.optional (P.try (P.chunk " footer"))
  C.hspace
  _ <- C.eol
  contents <- P.manyTill P.anySingle (P.try (L.nonIndented (pure ()) (P.chunk "```")))
  space
  i <- getNewNodeId
  pure (CppBlock i footer (T.pack contents))

phases :: ParserState s => Parser s PhasesDeclaration
phases = parsePositioned $ do
  _ <- word "phases"
  _ <- symbol ":"
  sets <- P.between (symbol "[") (symbol "]") (phaseSet `P.sepBy1` symbol ",")
  i <- getNewNodeId
  pure (PhasesDeclaration i (case sets of x : xs -> x :| xs; [] -> error "empty phases"))
  where
    phaseSet =
      (Right <$> parsePositioned (symbol "*" *> (EverythingElseRuleset <$> getNewNodeId)))
        P.<|> ( Left
                  <$> parsePositioned
                    ( do
                        names <- braces (simple `P.sepBy1` symbol ",")
                        i <- getNewNodeId
                        pure (Ruleset i (case names of x : xs -> x :| xs; [] -> error "empty ruleset"))
                    )
              )

node :: ParserState s => CppForm -> [Expr] -> Parser s Expr
node form args = parsePositioned (ExprCpp <$> getNewNodeId <*> pure form <*> pure args)

-- Maximal-munch guards prevent e.g. '+' from accepting '+=' or '++',
-- while still allowing adjacent different operators such as 'a * -b'.
operator :: Text -> Parser s Text
operator op = lexeme $ P.try $ do
  _ <- P.chunk op
  let longer = filter (\x -> op `T.isPrefixOf` x && x /= op) tokens
  P.notFollowedBy (P.choice [P.chunk (T.drop (T.length op) x) | x <- longer])
  pure op
  where
    tokens = ["++", "--", "->", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "==", "!=", "<=", ">=", "&&", "||", "<<", ">>", "|-"]

expression :: ParserState s => Parser s Expr
expression = parsePositioned $ do
  build <- makeExprParser (pure <$> unary) table
  condition <- build
  maybe
    (pure condition)
    ( \_ -> do
        yes <- expression
        _ <- symbol ":"
        no <- expression
        node CppConditional [condition, yes, no]
    )
    =<< P.optional (symbol "?")
  where
    binary op =
      InfixL
        ( operator op
            *> pure
              ( \left right -> do
                  a <- left
                  b <- right
                  node (CppBinary op) [a, b]
              )
        )
    table =
      map
        (map binary)
        [["*", "/", "%"], ["+", "-"], ["<<", ">>"], ["<=", ">=", "<", ">"], ["==", "!="], ["&"], ["^"], ["|"], ["&&"], ["||"]]

unary :: ParserState s => Parser s Expr
unary =
  parsePositioned $
    (do op <- P.choice (map operator ["+", "-", "!", "~"]); value <- unary; node (CppUnary op) [value])
      P.<|> (atom >>= postfix)

postfix :: ParserState s => Expr -> Parser s Expr
postfix value =
  (do args <- arguments expression; node CppCall (value : args) >>= postfix)
    P.<|> (do _ <- symbol "."; name <- rawName; node (CppMember name) [value] >>= postfix)
    P.<|> (do index <- P.between (symbol "[") (symbol "]") expression; node CppIndex [value, index] >>= postfix)
    P.<|> pure value

atom :: ParserState s => Parser s Expr
atom =
  parens expression
    P.<|> parsePositioned
      ( do
          _ <- word "static_cast"
          t <- P.between (symbol "<") (symbol ">") typeText
          value <- parens expression
          node (CppCast t) [value]
      )
    P.<|> parsePositioned
      ( do
          t <- P.try (typeText <* P.lookAhead (symbol "{"))
          values <- braces (expression `P.sepBy` symbol ",")
          node (CppConstruct t) values
      )
    P.<|> ( do
              _ <- P.try $ do
                t <- typeText
                unless ("<" `T.isInfixOf` t) P.empty
                P.lookAhead (symbol "(")
              fail "explicit template function calls are not supported; use a named native wrapper"
          )
    P.<|> parsePositioned
      ( do
          literal <- quoted '"' P.<|> quoted '\'' P.<|> number P.<|> word "true" P.<|> word "false"
          node (CppLiteral literal) []
      )
    P.<|> parsePositioned (do name <- ident; i <- getNewNodeId; pure (ExprVar i name))

quoted :: Char -> Parser s Text
quoted delimiter =
  lexeme $
    fst
      <$> P.match
        ( do
            _ <- C.char delimiter
            let character = void (P.satisfy (\c -> c /= delimiter && c /= '\\' && c /= '\n' && c /= '\r')) P.<|> escape
            if delimiter == '\'' then character else void (P.many character)
            void (C.char delimiter)
        )
  where
    escape = do
      _ <- C.char '\\'
      void (P.oneOf ("'\"?\\abfnrtv" :: String))
        P.<|> void (C.char 'x' *> P.some C.hexDigitChar)
        P.<|> void (C.char 'u' *> P.count 4 C.hexDigitChar)
        P.<|> void (C.char 'U' *> P.count 8 C.hexDigitChar)
        P.<|> void (C.octDigitChar *> P.optional C.octDigitChar *> P.optional C.octDigitChar)

number :: Parser s Text
number =
  lexeme $
    fst
      <$> P.match
        ( do
            void (P.try (P.chunk "0x" P.<|> P.chunk "0X") *> P.some C.hexDigitChar)
              P.<|> void (P.try (P.chunk "0b" P.<|> P.chunk "0B") *> P.some (P.oneOf ("01" :: String)))
              P.<|> do
                digits <- P.many C.digitChar
                fraction <- P.optional (C.char '.' *> P.many C.digitChar)
                when (null digits && maybe True null fraction) (fail "numeric literal")
                void (P.optional (P.oneOf ("eE" :: String) *> P.optional (P.oneOf ("+-" :: String)) *> P.some C.digitChar))
            void (P.takeWhileP Nothing (`elem` ("uUlLfF" :: String)))
            P.notFollowedBy (P.satisfy identContinue)
        )
