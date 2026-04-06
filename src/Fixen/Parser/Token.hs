{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser.Token
--     Description : Parsers for tokens in Fixen.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for tokens in Fixen.
module Fixen.Parser.Token (
  -- * Raw string parsers
  -- $raw

  -- ** Letter identifier strings
  parseRawLowerHsIdentifierString,
  parseRawCapitalizedHsIdentifierString,
  parseRawAnyCaseHsIdentifierString,

  -- ** Operator identifier strings
  parseRawOpChar,
  parseRawOpIdentifierString,

  -- * Identifier parsers
  -- $id
  parseLowerFirstSimpleIdentifier,
  parseCapitalizedSimpleIdentifier,
  parseCapitalizedFQN,
  parseLowerFirstFQN,
  parseCapitalizedIdentifier,
  parseAnyCasedLetterSimpleIdentifier,
  parseAnyCasedLetterFQN,
  parseAnyCasedLetterIdentifier,
  parseOpSimpleIdentifier,
  parseOpFQN,
  parseOpIdentifier,
  parseInfixTermIdentifier,
  parseNonInfixTermIdentifier,
  parseNonInfixOpIdentifier,
  parseModuleName,

  -- * Literals
  parseRawString,
  parseRawInteger,
  parseRawNatural,

  -- * Miscellaneous
  opChars,
  reserved,
  reservedOps,
  keyword,
  keywordOp,
  turnstile,
  ltOrSqSubsetEq,
) where

import Control.Applicative.Combinators (
  choice,
  manyTill,
  some,
  (<|>),
 )
import Control.Monad.Combinators.NonEmpty qualified as PNE
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Error.Diagnose.Position
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core qualified as Core
import Fixen.Parser.Common
import GHC.Natural (Natural)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))
import Text.Megaparsec.Pos qualified as MPos

-------------------------------------------------------------------------------
--
-- Tokenizers
--
-------------------------------------------------------------------------------

-- $raw
-- The @parseRaw...@ parsers are the lowest-level parsers that directly parse
-- the text and produce unannotated base types.

-- | Parses strings whose first character is a lowercase letter or an
-- underscore (@_@), and the remaining characters are valid in a Haskell
-- identifier. The string must also not be a 'reserved' keyword
--
-- Examples: @hello@ (ok), @x'@ (ok), @_123X@ (ok), @Int@ (not ok)
parseRawLowerHsIdentifierString :: Parser Text
parseRawLowerHsIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  str <-
    fmap pack $
      (:)
        <$> (C.lowerChar <|> P.single '_') -- first char must be lowercase or _
        <*> P.many -- remaining can be any alphanum or _ or '
          (C.alphaNumChar <|> P.single '_' <|> P.single '\'')
  if str `elem` reserved
    then -- disallow any string that is a reserved keyword!

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved keyword '" ++ unpack str ++ "'")
            )
        )
    else return str

-- | Parses strings whose first character is an uppercase letter, and the
-- remaining characters are valid in a Haskell type/constructor identifier.
--
-- Examples: @Int@ (ok), @int@ (not ok), @A'@ (ok)
parseRawCapitalizedHsIdentifierString :: Parser Text
parseRawCapitalizedHsIdentifierString =
  fmap pack $
    (:)
      <$> C.upperChar -- first char MUST be uppercase
      <*> P.many -- remaining can be any alphanum of _ or '
        (C.alphaNumChar <|> P.single '_' <|> P.single '\'')

-- | Parses strings whose characters do not form operators, and are essentially
-- valid identifiers for types and variables. The string must not be a
-- 'reserved' keyword.
parseRawAnyCaseHsIdentifierString :: Parser Text
parseRawAnyCaseHsIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  str <-
    fmap pack $
      (:)
        -- first char must be lower char, _ or upper char
        <$> (C.lowerChar <|> P.single '_' <|> C.upperChar)
        -- the rest is alphanum, _ or '
        <*> P.many (C.alphaNumChar <|> P.single '_' <|> P.single '\'')
  if str `elem` reserved -- disallow any string that is a reserved keyword!
    then
      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved keyword '" ++ unpack str ++ "'")
            )
        )
    else return str

-- | Parses a Haskell operator character. The valid characters are defined in
-- 'opChars'.
parseRawOpChar :: Parser Char
parseRawOpChar = choice (P.single <$> opChars)

-- | Parses the name of a Haskell operator. The name must not be a
-- 'reservedOps' keyword.
parseRawOpIdentifierString :: Parser Text
parseRawOpIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  -- use parseRawOpChar
  -- bug fix: was using many instead of some; must have
  --          more than one char!!
  str <- pack <$> some parseRawOpChar
  if str `elem` reservedOps
    then -- cannot use reserved operators!

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved operator (" ++ unpack str ++ ")")
            )
        )
    else return str

-------------------------------------------------------------------------------
--
-- Identifier parsers
--
-------------------------------------------------------------------------------

-- $id
-- The @parse...Identifier@ variants parse 'ASTAnnotatedString's. These parsers
-- __do not consume whitespace after parsing__. Also, different kinds of
-- identifiers are acceptable in different contexts, for instance, operators
-- can be used without parentheses in infix, while identifiers must be
-- surrounded by backticks.

-- | Parses a module name, which is a series of capitalized identifiers separated
-- by dots. For example, @Data.List@ or @MyCompany.MyProject.MyModule@.
parseModuleName :: Parser AST.ModuleName
parseModuleName = do
  (pos, ls) <- parsePositioned (PNE.sepBy1 parseCapitalizedSimpleIdentifier (P.single '.'))
  return $ Core.ModuleName pos ls

-- | The parser 'parseRawLowerHsIdentifierString' but wrapped as an
-- 'AST.SimpleIdentifier'. These are non-constructor variables that are __not__
-- used in infix form.
parseLowerFirstSimpleIdentifier :: Parser AST.SimpleIdentifier
parseLowerFirstSimpleIdentifier = do
  (pos, str) <- parsePositioned parseRawLowerHsIdentifierString
  return $ Core.SimpleIdentifier pos str

-- | Parses a fully qualified name whose first identifier starts with a lowercase
-- letter. For example, @Data.List.map@ or @MyModule.myFunction@.
parseLowerFirstFQN :: Parser AST.FullyQualifiedName
parseLowerFirstFQN = do
  (pos, (module_name, ident)) <- parsePositioned $ do
    module_name <- parsePrefix
    _ <- P.single '.'
    ident <- parseLowerFirstSimpleIdentifier
    return (module_name, ident)
  return $ Core.FullyQualifiedName pos module_name ident
  where
    parsePrefix :: Parser AST.ModuleName
    parsePrefix = do
      (pos, (hd, tl)) <- parsePositioned $ do
        f <- parseCapitalizedSimpleIdentifier
        ls <- manyNonFailing (P.single '.' *> parseCapitalizedSimpleIdentifier)
        return (f, ls)
      return $ Core.ModuleName pos (hd NE.:| tl)
    manyNonFailing :: Parser a -> Parser [a]
    manyNonFailing p = do
      m <- P.observing (P.try p)
      case m of
        Left _ -> return []
        Right e -> (:) e <$> manyNonFailing p

-- | The parser 'parseRawCapitalizedHsIdentifierString' but wrapped as an
-- 'AST.SimpleIdentifier'. These are non-constructor variables that are __not__
-- used in infix form.
parseCapitalizedSimpleIdentifier :: Parser AST.SimpleIdentifier
parseCapitalizedSimpleIdentifier = do
  (pos, str) <- parsePositioned parseRawCapitalizedHsIdentifierString
  return $ Core.SimpleIdentifier pos str

-- | Parses a fully qualified name whose first identifier starts with a capital
-- letter. For example, @Data.List@ or @MyModule.MyType@.
parseCapitalizedFQN :: Parser AST.FullyQualifiedName
parseCapitalizedFQN = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  -- parse a series of capitalized identifiers separated by dots
  ls <- P.sepBy1 parseCapitalizedSimpleIdentifier (P.single '.')
  case ls of
    -- must have at least two identifiers to be a FQN
    (x : x' : ls') -> do
      let -- first_ident is the first ident, remaining_idents is a non-empty
          -- list of the rest. remaining_idents is guaranteed to be
          -- non-empty because of the pattern match above
          (first_ident, remaining_idents) = (x, x' NE.:| ls')
          -- mod_name is the module name, which is all but the last ident
          mod_name = first_ident NE.:| NE.init remaining_idents
          -- name is the actual name, which is the last ident
          name = NE.last remaining_idents
          -- m_pos is the position of the module name, which spans from the
          -- start of the first ident, to the end of the last ident in
          -- the module name
          m_init_pos = AST.getPosition first_ident
          m_last_pos = AST.getPosition (NE.last mod_name)
          m_pos =
            Position
              { begin = begin m_init_pos
              , end = end m_last_pos
              , file = file m_init_pos
              }
          -- name_pos is the position of the name
          name_pos = AST.getPosition name
          -- fqn_pos is the positiong of the whole FQN, which spans from the
          -- start of the first ident to the end of the name
          fqn_pos =
            Position
              { begin = begin m_init_pos
              , end = end name_pos
              , file = file m_init_pos
              }
      return $ Core.FullyQualifiedName fqn_pos (Core.ModuleName m_pos mod_name) name
    _ ->
      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail "expected module name")
            )
        )

-- | Parses either a capitalized fully qualified name like @Data.List@ or a
-- capitalized simple identifier like @Just@.
parseCapitalizedIdentifier :: Parser AST.Identifier
parseCapitalizedIdentifier =
  (Core.IdentifierFullyQualifiedName <$> P.try parseCapitalizedFQN)
    <|> (Core.IdentifierSimpleIdentifier <$> parseCapitalizedSimpleIdentifier)

-- | The parser 'parseRawAnyCaseHsIdentifierString' but wrapped as an
-- 'AST.SimpleIdentifier'. These are essentially normal terms or constructors
-- that are __not__ used in infix.
parseAnyCasedLetterSimpleIdentifier :: Parser AST.SimpleIdentifier
parseAnyCasedLetterSimpleIdentifier = do
  (pos, str) <- parsePositioned parseRawAnyCaseHsIdentifierString
  return $ Core.SimpleIdentifier pos str

-- | Parses either a fully qualified name whose first letter is lowercase like
-- @Data.List.map@ or a capitalized fully qualified name like @Data.List@.
parseAnyCasedLetterFQN :: Parser AST.FullyQualifiedName
parseAnyCasedLetterFQN = P.try parseLowerFirstFQN <|> parseCapitalizedFQN

-- | Parses either a fully qualified name like @Data.List@ or a simple
-- identifier like @Just@ or @myFunction@. These are essentially normal
-- terms or constructors that are __not__ used in infix.
parseAnyCasedLetterIdentifier :: Parser AST.Identifier
parseAnyCasedLetterIdentifier =
  (Core.IdentifierFullyQualifiedName <$> P.try parseAnyCasedLetterFQN)
    <|> (Core.IdentifierSimpleIdentifier <$> parseAnyCasedLetterSimpleIdentifier)

-- | Parses a simple operator identifier, which is a string of operator
-- characters.
parseOpSimpleIdentifier :: Parser AST.SimpleIdentifier
parseOpSimpleIdentifier = do
  (pos, str) <- parsePositioned parseRawOpIdentifierString
  return $ Core.SimpleIdentifier pos str

-- | Parses a fully qualified operator name, e.g. @Data.List.++@.
parseOpFQN :: Parser AST.FullyQualifiedName
parseOpFQN = do
  (pos, (module_name, ident)) <- parsePositioned $ do
    module_name <- parsePrefix
    _ <- P.single '.'
    ident <- parseOpSimpleIdentifier
    return (module_name, ident)
  return $ Core.FullyQualifiedName pos module_name ident
  where
    parsePrefix :: Parser AST.ModuleName
    parsePrefix = do
      (pos, (hd, tl)) <- parsePositioned $ do
        f <- parseCapitalizedSimpleIdentifier
        ls <- manyNonFailing (P.single '.' *> parseCapitalizedSimpleIdentifier)
        return (f, ls)
      return $ Core.ModuleName pos (hd NE.:| tl)
    manyNonFailing :: Parser a -> Parser [a]
    manyNonFailing p = do
      m <- P.observing (P.try p)
      case m of
        Left _ -> return []
        Right e -> (:) e <$> manyNonFailing p

-- | Parses either a fully qualified operator name like @Data.List.++@ or a
-- simple operator identifier like @++@.
parseOpIdentifier :: Parser AST.Identifier
parseOpIdentifier =
  (Core.IdentifierFullyQualifiedName <$> P.try parseOpFQN)
    <|> (Core.IdentifierSimpleIdentifier <$> parseOpSimpleIdentifier)

-- | Parses a term-level non-infix identifer. These are:
--
--   1. Letter-based terms with backticks like @`elem`@ or @`Just`@
--   2. Operators like @++@ or @<=@
parseInfixTermIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseInfixTermIdentifier indent_check = P.try (parseInfixLetterIdentifier indent_check) <|> parseOpIdentifier

-- | Parses a term-level infix letter-based identifier. These are essentially
-- variables or constructors that are used in infix position. For example,
-- @`elem`@ and @`notElem`@. Note that the annotated source positions do not
-- include the backticks.
parseInfixLetterIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseInfixLetterIdentifier indentCheck = do
  _ <- P.single '`'
  _ <- indentCheck
  ident <- parseAnyCasedLetterIdentifier
  _ <- indentCheck
  _ <- P.single '`'
  return ident

-- | Parses a term-level non-infix identifier. These are:
--
--    1. First-letter lowercase terms like @myFunction@ or @MyModule.var@
--    2. Capitalized letter terms like constructors, e.g. @Just@ or @Data.Maybe.Just@
--    3. Parenthesized operators like @(++)@ or @(Data.List.++)@
--
--  The parser takes an indentation checker as an argument, which is used to
--  ensure that the operator is indented correctly.
parseNonInfixTermIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseNonInfixTermIdentifier indent_check =
  P.try parseAnyCasedLetterIdentifier
    <|> parseNonInfixOpIdentifier indent_check

-- | Parses an operator identifier used in non-infix notation. Essentially the
-- parser 'parseOpIdentifier' wrapped in parentheses. The parser takes an
-- indentation checker as an argument, which is used to ensure that the operator
-- is indented correctly.
parseNonInfixOpIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseNonInfixOpIdentifier indent_check = do
  betweenParentheses indent_check (indent_check >> parseOpIdentifier)

-------------------------------------------------------------------------------
--
-- Literals
--
-------------------------------------------------------------------------------

-- | Parses a string literal, i.e. @"blabla"@
parseRawString :: Parser Text
parseRawString =
  fmap pack $
    l $
      C.char '"' >> manyTill L.charLiteral (C.char '"')

-- | Parses a (signed) integer literal.
parseRawInteger :: Parser Integer
parseRawInteger = L.signed sc L.decimal

-- | Parses a natural literal.
parseRawNatural :: Parser Natural
parseRawNatural = L.decimal

-------------------------------------------------------------------------------
--
-- Keywords/constants
--
-- Remember to update the documentation if any of these change!!
--
-------------------------------------------------------------------------------

-- |
-- @
-- opChars = ":!#$%&*+./<=>?@\\^|-~"
-- @
opChars :: [Char]
opChars = ":!#$%&*+./<=>?@\\^|-~"

-- |
-- @
-- reserved = ["if"]
-- @
reserved :: [Text]
reserved = ["if"]

-- |
--
-- @
-- reservedOps = ["="]
-- @
reservedOps :: [Text]
reservedOps = ["="]

-- | Parses a string as a keyword, ensuring that it is not trailed by more
-- letters, numbers, underscores and @'@s.
keyword :: Text -> Parser Text
keyword s = do
  x <- C.string s
  P.notFollowedBy (C.alphaNumChar <|> C.char '_' <|> C.char '\'')
  return x

-- | Parses a string as a keyword operator, ensuring that it is not trailed
-- by more op chars.
keywordOp :: Text -> Parser Text
keywordOp s = do
  x <- C.string s
  P.notFollowedBy parseRawOpChar
  return x

-- | Parses a turnstile @|-@ or @⊢@
turnstile :: Parser Text
turnstile = P.try (keywordOp "⊢") <|> keywordOp "|-"

-- | Parses a partial order less than symbol @<@ or @⊏@
ltOrSqSubsetEq :: Parser Text
ltOrSqSubsetEq = P.try (keywordOp "<") <|> keywordOp "⊏"
