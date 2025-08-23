-- |
--     Module      : Mozzarella.Parser.Token
--     Description : Parsers for tokens in Mozzarella.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for tokens in Mozzarella.
module Mozzarella.Parser.Token (
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

  -- ** Non-infix identifiers
  parseLowerFirstIdentifier,
  parseCapitalizedIdentifier,
  parseAnyCasedLetterIdentifier,
  parseNonInfixOpIdentifier,
  parseNonInfixTermIdentifier,

  -- ** Infix identifier
  parseInfixAnyCasedLetterIdentifier,
  parseInfixOperatorIdentifier,
  parseInfixTermIdentifier,

  -- * Literals
  parseRawString,
  parseRawInteger,

  -- * Miscellaneous
  opChars,
  reserved,
  reservedOps,
  keyword,
  keywordOp,
  turnstile,
) where

import Control.Applicative.Combinators (
  choice,
  manyTill,
  some,
  (<|>),
 )
import Data.Set qualified as Set
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Core ((:<:) (..))
import Mozzarella.Parser.Common
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))

-------------------------------------------------------------------------------
--
-- Tokenizers
--
-------------------------------------------------------------------------------

-- $raw
-- The @parseRaw...@ parsers are the lowest-level parsers that directly parse
-- the text and produce unannotated base types. The raw parsers __do not__
-- __consume whitespace__ after the token.

-- | Parses strings whose first character is a lowercase letter or an
-- underscore (@_@), and the remaining characters are valid in a Haskell
-- identifier. The string must also not be a 'reserved' keyword
--
-- Examples: @hello@ (ok), @x'@ (ok), @_123X@ (ok), @Int@ (not ok)
parseRawLowerHsIdentifierString :: Parser String
parseRawLowerHsIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  str <-
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
                (ErrorFail $ "unexpected reserved keyword '" ++ str ++ "'")
            )
        )
    else return str

-- | Parses strings whose first character is an uppercase letter, and the
-- remaining characters are valid in a Haskell type/constructor identifier.
--
-- Examples: @Int@ (ok), @int@ (not ok), @A'@ (ok)
parseRawCapitalizedHsIdentifierString :: Parser String
parseRawCapitalizedHsIdentifierString =
  (:)
    <$> C.upperChar -- first char MUST be uppercase
    <*> P.many -- remaining can be any alphanum of _ or '
      (C.alphaNumChar <|> P.single '_' <|> P.single '\'')

-- | Parses strings whose characters do not form operators, and are essentially
-- valid identifiers for types and variables. The string must not be a
-- 'reserved' keyword.
parseRawAnyCaseHsIdentifierString :: Parser String
parseRawAnyCaseHsIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  str <-
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
                (ErrorFail $ "unexpected reserved keyword '" ++ str ++ "'")
            )
        )
    else return str

-- | Parses a Haskell operator character. The valid characters are defined in
-- 'opChars'.
parseRawOpChar :: Parser Char
parseRawOpChar = choice (P.single <$> opChars)

-- | Parses the name of a Haskell operator. The name must not be a
-- 'reservedOps' keyword.
parseRawOpIdentifierString :: Parser String
parseRawOpIdentifierString = do
  -- Get the offset for throwing errors
  offset_start <- P.getOffset
  -- use parseRawOpChar
  -- bug fix: was using many instead of some; must have
  --          more than one char!!
  str <- some parseRawOpChar
  if str `elem` reservedOps
    then -- cannot use reserved operators!

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved operator (" ++ str ++ ")")
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

-- | The parser 'parseRawLowerHsIdentifierString' but wrapped as an
-- 'ASTAnnotatedString'. These are non-constructor variables that are __not__
-- used in infix form.
parseLowerFirstIdentifier :: Parser AST.TermLetterIdentifier
parseLowerFirstIdentifier = do
  (pos, str) <- parsePositioned parseRawLowerHsIdentifierString
  return $ AST.TermLetterIdentifier pos str

-- | The parser 'parseRawCapitalizedHsIdentifierString' but wrapped as an
-- 'ASTAnnotatedString'. These are non-constructor variables that are __not__
-- used in infix form.
parseCapitalizedIdentifier :: Parser AST.TypeLetterIdentifier
parseCapitalizedIdentifier = do
  (pos, str) <- parsePositioned parseRawCapitalizedHsIdentifierString
  return $ AST.TypeLetterIdentifier pos str

-- | The parser 'parseRawAnyCaseHsIdentifierString' but wrapped as an
-- 'ASTAnnotatedString'. These are essentially normal terms or constructors
-- that are __not__ used in infix.
parseAnyCasedLetterIdentifier :: Parser AST.TermLetterIdentifier
parseAnyCasedLetterIdentifier = do
  (pos, str) <- parsePositioned parseRawAnyCaseHsIdentifierString
  return $ AST.TermLetterIdentifier pos str

-- | Parses operators that are __not used in infix notation__. This means that
-- they are surrounded with parentheses. The annotated source positions include
-- the parentheses. For example, @(++)@ or @(*)@, but not @/=@.
parseNonInfixOpIdentifier :: Parser AST.OpIdentifier
parseNonInfixOpIdentifier = do
  (pos, str) <-
    parsePositioned
      ( P.single
          '('
          *> parseRawOpIdentifierString
          <* P.single ')'
      )
  return $ AST.OpIdentifier pos str

-- | Parses a term-level non-infix identifer. These are:
--
--   1. First-letter lowercase terms like @myFunction@
--   2. Capitalized letter terms like constructors, e.g. @Just@
--   3. Parenthesizes operators like @(++)@
parseNonInfixTermIdentifier :: Parser AST.TermIdentifier
parseNonInfixTermIdentifier =
  ((↑) <$> P.try parseAnyCasedLetterIdentifier)
    <|> ((↑) <$> parseNonInfixOpIdentifier)

-- | Parses a term-level infix letter-based identifier. These are essentially
-- variables or constructors that are used in infix position. For example,
-- @`elem`@ and @`notElem`@. Note that the annotated source positions include
-- the backticks.
parseInfixAnyCasedLetterIdentifier :: Parser AST.TermLetterIdentifier
parseInfixAnyCasedLetterIdentifier = do
  (pos, str) <- parsePositioned $ do
    _ <- P.single '`'
    str <- parseRawAnyCaseHsIdentifierString
    _ <- P.single '`'
    return str
  return $ AST.TermLetterIdentifier pos str

-- | Parses an operator identifier used in infix notation. Essentially the
-- parser 'parseRawOpIdentifierString' annotated with source positions.
parseInfixOperatorIdentifier :: Parser AST.OpIdentifier
parseInfixOperatorIdentifier = do
  (pos, str) <- parsePositioned parseRawOpIdentifierString
  return $ AST.OpIdentifier pos str

-- | Parses a term-level non-infix identifer. These are:
--
--   1. Letter-based terms with backticks like @`elem`@ or @`Just`@
--   2. Operators like @++@ or @<=@
parseInfixTermIdentifier :: Parser AST.TermIdentifier
parseInfixTermIdentifier =
  ((↑) <$> P.try parseInfixOperatorIdentifier)
    <|> ((↑) <$> parseInfixAnyCasedLetterIdentifier)

-------------------------------------------------------------------------------
--
-- Literals
--
-------------------------------------------------------------------------------

-- | Parses a string literal, i.e. @"blabla"@
parseRawString :: Parser String
parseRawString = l $ C.char '"' >> manyTill L.charLiteral (C.char '"')

-- | Parses a (signed) integer literal.
parseRawInteger :: Parser Integer
parseRawInteger = L.signed sc L.decimal

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
reserved :: [String]
reserved = ["if"]

-- |
--
-- @
-- reservedOps = ["="]
-- @
reservedOps :: [String]
reservedOps = ["="]

-- | Parses a string as a keyword, ensuring that it is not trailed by more
-- letters, numbers, underscores and @'@s.
keyword :: String -> Parser String
keyword s = do
  x <- C.string s
  P.notFollowedBy (C.alphaNumChar <|> C.char '_' <|> C.char '\'')
  return x

-- | Parses a string as a keyword operator, ensuring that it is not trailed
-- by more op chars.
keywordOp :: String -> Parser String
keywordOp s = do
  x <- C.string s
  P.notFollowedBy parseRawOpChar
  return x

-- | Parses a turnstile @|-@ or @⊢@
turnstile :: Parser String
turnstile = P.try (keywordOp "⊢") <|> keywordOp "|-"
