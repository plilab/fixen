{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--     Module      : Fixen.Parser.Common
--     Description : Common parsing utilities for the Fixen parser
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides shared parsing utilities used across the Fixen
--     parser. These include:
--
--     * 'Parser' and 'ParserState' — the core parser type and state constraints
--
--     * Comma-separated list parsers ('commaSepBy', 'commaSepBy1', etc.)
--       that handle indentation-aware comma separation
--
--     * Indentation guards ('indented', 'indentedByMoreThan', etc.)
--       that enforce the Fixen language's indentation rules
--
--     * Repetition combinators ('manyI', 'someI', etc.) that parse
--       zero-or-more / one-or-more items with indentation checks
--
--     * Whitespace and comment handling ('sc', 'l')
--
--     * Position tracking ('parsePositioned') that records source
--       positions for parsed AST nodes
--
--     * Bracket combinators ('betweenParentheses', 'betweenSquareBrackets',
--       'betweenCurlyBraces') for parsing delimited expressions
--
-- @since 26.7
module Fixen.Parser.Common where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.List.NonEmpty
import Data.Set qualified as Set
import Data.Text
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Error
import Fixen.Utils
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos qualified as MPos

--------------------------------------------------------------------------------

-- * Parsers and Parser States

--------------------------------------------------------------------------------

-- | The state carried by the parser.
--
--   This must be a product of at least three components:
--
--   * 'PositionEnv' — maps 'NodeId' values to source positions
--   * 'NodeId' — the current node ID counter (incremented on each new node)
--   * 'FixenErrors' — accumulated error diagnostics
--
--   This state is shared across all parser combinators and is updated
--   as the parser consumes input and constructs AST nodes.
--
-- @since 26.7
type ParserState σ = (WithPositionEnv σ, NodeIded σ, WithErrors σ)

-- | The parser type used by the Fixen parser.
--
--   This is a Megaparsec parser ('P.ParsecT') layered on top of the 'State'
--   monad.
--
--   The parser type uses the custom 'FixenParseError' datatype. Internally,
--   throughout parsing, all errors are represented using this custom datatype.
--   This was introduced since 26.8.
--
--   In practice, you will rarely construct values of type 'Parser' directly.
--   Instead, use the combinators provided in this module and the higher-level
--   parsers in 'Fixen.Parser'.
--
-- @since 26.7
type Parser σ = P.ParsecT FixenParseError Text (State σ)

--------------------------------------------------------------------------------

-- * Parser Combinators

--------------------------------------------------------------------------------

-- ** Comma separation

-- | Parse zero or more comma-separated items, with indentation checking.
-- Starts with a comma. Used by the other comma-separation parsers.
-- Commits on the comma.
--
-- @since 26.8
commaSepByCommaFirst :: Parser σ MPos.Pos -> Parser σ α -> Parser σ [α]
commaSepByCommaFirst i p' = do
  manyICommitted (== ',') i (exactMatchNoLookahead "," *> i *> p')

-- | Parse zero or more comma-separated items, with indentation checking.
--
-- @since 26.7
commaSepBy
  :: Parser σ MPos.Pos
  -- ^ The indentation check
  --
  -- @since 26.7
  -> Parser σ α
  -- ^ The parser for a single item
  --
  -- @since 26.7
  -> Parser σ [α]
commaSepBy indent_check p = do
  x <- P.observing $ P.try $ do
    _ <- indent_check
    p
  case x of
    Left _ -> return []
    Right e -> (e :) <$> commaSepByCommaFirst indent_check p

-- | This is 'commaSepBy' with 'indented'.
--
-- @since 26.7
commaSepBy'
  :: ParserState σ
  => Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ [α]
commaSepBy' = commaSepBy indented

-- | Parse one or more items separated by commas, with indentation checking.
--
-- @since 26.7
commaSepBy1
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 26.7
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ (NonEmpty α)
commaSepBy1 indent_check p = do
  _ <- indent_check
  e <- p
  (e :|) <$> commaSepByCommaFirst indent_check p

-- | This is 'commaSepBy1', using 'indented' for indentation.
--
-- @since 26.7
commaSepBy1'
  :: Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ (NonEmpty α)
commaSepBy1' = commaSepBy1 indented

-- | Parse two or more items separated by commas, with indentation checking.
--
-- @since 26.7
commaSepBy2
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 26.7
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ (α, NonEmpty α)
commaSepBy2 indent_check p = do
  e <- indent_check *> p <* indent_check
  _ <- exactMatchNoLookahead ","
  _ <- indent_check
  ls <- commaSepBy1 indent_check p
  return (e, ls)

-- | This is 'commaSepBy2', using 'indented' for indentation.
--
-- @since 26.7
commaSepBy2'
  :: Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ (α, NonEmpty α)
commaSepBy2' = commaSepBy2 indented

-- ** Many and Some

-- $manyAndSome
--
-- The reason we have these combinators is because the original 'many' and
-- 'some' combinators by 'Megaparsec' does not backtrack properly and is not
-- indentation-sensitive like we want.

-- | Parse zero or more items, each preceded by an indentation check.
--
-- @since 26.7
manyI
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 26.7
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ [α]
manyI indent_check p = do
  m <- P.observing (P.try $ indent_check *> p)
  case m of
    Left _ -> return []
    Right e -> (:) e <$> manyI indent_check p

-- | 'manyI' without backtracking when the lookahead
-- character meets a predicate.
--
-- @since 26.8
manyICommitted
  :: (Char -> Bool)
  -- ^ The lookahead characters to commit the parse with
  -> Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 26.8
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.8
  -> Parser σ [α]
manyICommitted s indent_check p = do
  m <- P.observing $ P.try $ indent_check *> P.lookAhead P.anySingle
  case m of
    Left _ -> return []
    Right c -> do
      if s c
        then do
          e <- p
          (:) e <$> manyICommitted s indent_check p
        else do
          m' <- P.observing $ P.try p
          case m' of
            Left _ -> return []
            Right e -> (:) e <$> manyICommitted s indent_check p

-- | This is 'manyI' using 'indented' for indentation.
--
-- @since 26.7
manyI'
  :: Parser σ α
  -- ^ The parser for individual items.
  --
  -- @since 26.7
  -> Parser σ [α]
manyI' = manyI indented

-- | Parse one or more items, each preceded by an indentation check.
--
-- @since 26.7
someI
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 26.7
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 26.7
  -> Parser σ (NonEmpty α)
someI indent_check p = do
  e <- indent_check *> p
  (:|) e <$> manyI indent_check p

-- | This is 'someI', using 'indented' for indentation.
--
-- @since 26.7
someI' :: Parser σ α -> Parser σ (NonEmpty α)
someI' = someI indented

-- ** Parsing Between Delimiters with Indentation Checking

-- | A helper for parsing stuff between delimiters.
--
-- @since 26.8
betweenIndented
  :: Text
  -- ^ Opening, e.g., "("
  -> Text
  -- ^ Closing, e.g., ")"
  -> Text
  -- ^ The name of the opening and closing punctuations, e.g. parenthesis
  -> Parser σ MPos.Pos
  -- ^ The indentation check
  -> Parser σ α
  -- ^ The parser of the contents between the delimiters
  -> Parser σ α
betweenIndented open close punctuation_name indent_check p = do
  start <- P.getOffset
  _ <- l $ exactMatchNoLookahead open
  r <- p
  offset_after_p <- P.getOffset
  indent1 <- P.observing indent_check
  case indent1 of
    Left err -> do
      -- try parsing the close
      c <- P.observing $ P.try (exactMatchNoLookahead close)
      case c of
        Left _ ->
          customErrorWithOffset offset_after_p $
            FixenCustomError
              Nothing
              []
              ("unclosed " ++ unpack punctuation_name)
              (Just (start, start + 1, "opening " ++ unpack punctuation_name))
              []
        Right _ -> P.parseError err
    Right _ -> do
      _ <- exactMatchNoLookahead close
      return r

-- | Parse an expression delimited by parentheses @(@ ... @)@.
--
-- @since 26.7
betweenParentheses :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenParentheses = betweenIndented "(" ")" "parenthesis"

-- | Parse an expression delimited by square brackets @[@ ... @]@.
--
-- @since 26.7
betweenSquareBrackets :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenSquareBrackets = betweenIndented "[" "]" "bracket"

-- | Parse an expression delimited by curly braces @{@ ... @}@.
--
-- @since 26.7
betweenCurlyBraces :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenCurlyBraces = betweenIndented "{" "}" "brace"

--------------------------------------------------------------------------------

-- * Symbols

-- $symbols
-- We move this section to this module since the space consumer requires a valid
-- way of identifying Haskell line comment openings. For instance, @----@ is a
-- valid line comment opening, but not @--|@.

--------------------------------------------------------------------------------

-- | The set of valid Haskell operator characters.
--
--   This is the union of:
--
--   * Symbol characters: @:!#$%&*+./<=>?@\\^|-~@
--   * The colon character @:@ (used for constructors like @:Cons@)
--
--   This was moved from 'Fixen.Parser.Token' to this module
--   since 26.8 so that this module can deal with line comments
--
-- @since 26.7
opChars :: [Char]
opChars = ":!#$%&*+./<=>?@\\^|-~"

-- | Determines if it is an ascii operator character.
--
-- @since 26.8
isAsciiOpChar :: Char -> Bool
isAsciiOpChar = (∈ opChars)

-- | Determines if it is a unicode symbol or punctuation.
--
-- @since 26.8
isUnicodeSymbol :: Char -> Bool
isUnicodeSymbol x = isSymbol x ∨ isPunctuation x

-- | Determines if it is a valid operator character in Haskell.
--
-- @since 26.8
isValidOpChar :: Char -> Bool
isValidOpChar x =
  isAsciiOpChar x
    ∨ (isUnicodeSymbol x ∧ x ∉ hsSpecialChars ∧ x ≠ '_' ∧ x ≠ '"' ∧ x ≠ '\'')

-- | The list of "special" characters as defined by the Haskell 98 report.
--
-- @since 26.8
hsSpecialChars :: [Char]
hsSpecialChars = "(),;[]`{}"

--------------------------------------------------------------------------------

-- * Indentation and Whitespace

--------------------------------------------------------------------------------

-- | Ensure that the current position is indented by at least one character
-- relative to the previous line.
--
-- This is the fundamental indentation guard used throughout the parser.
-- In Fixen, every syntactic element that is not a top-level declaration
-- must be indented. This prevents ambiguity where a wrongly placed
-- declaration could be confused as belonging to a previous construct.
--
-- For example, in a Fixen file:
--
-- @
-- rel MyRel :
--     A,
-- B        -- ERROR: not indented, parser will reject
-- @
--
-- @since 26.7
indented :: Parser σ MPos.Pos
indented = indentedByMoreThan (MPos.mkPos 1)

-- | Ensure that the current position is indented by strictly more than
-- the given amount (in columns).
--
-- @since 26.7
indentedByMoreThan :: MPos.Pos -> Parser σ MPos.Pos
indentedByMoreThan = L.indentGuard sc GT

-- | Ensure that the current position is indented by exactly the given
-- amount (in columns).
--
-- @since 26.7
indentedByExactly :: MPos.Pos -> Parser σ MPos.Pos
indentedByExactly = L.indentGuard sc EQ

-- | The whitespace and comment consumer.
--
--   This handles:
--
--   * Whitespace
--   * Single-line comments starting with @--@
--   * Block comments delimited by @{-@ and @-}@
--
--   This is used by all other combinators in this module to consume
--   whitespace and comments before parsing meaningful tokens.
--
--   The 'sc' (space consumer) is a standard Megaparsec convention:
--   it is the primary whitespace handler, and other combinators like
--   'l' (lexeme) and 'symbol' build on top of it.
--
-- @since 26.7
sc :: Parser σ ()
sc = L.space C.space1 lineCommentParser (L.skipBlockComment "{-" "-}")

-- | A proper line comment parser.
--
-- @since 26.8
lineCommentParser :: Parser σ ()
lineCommentParser = P.try $ do
  start <- P.getOffset
  _ <- C.string "--"
  dashes <- P.takeWhileP (Just "dash") isValidOpChar

  when (Data.Text.any (/= '-') dashes) $
    customErrorWithOffset start $
      FixenCustomError
        (Just (2 + Data.Text.length dashes))
        []
        "invalid line comment opening"
        Nothing
        []
  void (P.takeWhileP (Just "character") (/= '\n'))

-- | Parse a token and consume all trailing whitespace and comments.
--
--   This is an alias for 'L.lexeme'. It parses the given parser and
--   then consumes all whitespace and comments that follow. This ensures
--   that the parser cursor is positioned at the next meaningful token.
--
--   This is the standard way to parse tokens in the Fixen parser:
--   wrap any token parser in 'l' to ensure proper whitespace handling.
--
--   Example: @l (keywordOp "if")@ parses the keyword @if@ and consumes
--   any trailing whitespace, positioning the cursor at the next token.
--
-- @since 26.7
l :: Parser σ α -> Parser σ α
l = L.lexeme sc

--------------------------------------------------------------------------------

-- * Source Position Tracking

--------------------------------------------------------------------------------

-- | Parse an AST node and record its source position in the 'PositionEnv'.
--
-- This is the primary mechanism for attaching source positions to AST
-- nodes.
--
-- Usage pattern:
--
-- @
-- -- Parse a value and record its position:
-- myParser :: Parser MyASTNode
-- myParser = parsePositioned $ do
--     -- ... parse the node ...
--     return node
-- @
--
-- If you want to use 'l', do so __outside__ the parser, i.e.:
--
-- @
-- myParser = l $ parsePositioned $ do
--     -- ... parse ...
-- @
--
-- This ensures the end position does not include trailing whitespace.
--
-- @since 26.7
parsePositioned :: ParserState σ => HasNodeId α NodeId => Parser σ α -> Parser σ α
parsePositioned p = do
  start <- P.getSourcePos
  a <- p
  end <- P.getSourcePos
  let pos = mkDiagPos start end
  setPosition a pos
  return a
  where
    -- Construct a Diag.Position from Megaparsec source positions
    mkDiagPos :: MPos.SourcePos -> MPos.SourcePos -> Position
    mkDiagPos start end =
      -- Extract file name, line and column for start and end
      let file_path = MPos.sourceName start
          start_line = MPos.unPos $ MPos.sourceLine start
          start_col = MPos.unPos $ MPos.sourceColumn start
          end_line = MPos.unPos $ MPos.sourceLine end
          end_col = MPos.unPos $ MPos.sourceColumn end
       in Position
            { begin = (start_line, start_col)
            , end = (end_line, end_col)
            , file = file_path
            }

--------------------------------------------------------------------------------

-- * Miscellaneous

--------------------------------------------------------------------------------

-- | A replacement of 'C.string' from megaparsec that does not
-- produce some of the most ridiculous error messages.
--
-- @since 26.8
exactMatchNoLookahead
  :: Text
  -- ^ The string to match on
  -> Parser σ Text
exactMatchNoLookahead s = P.try $ do
  start <- P.getOffset
  ls <- getLongestMatchingToken (unpack s)
  if ls ≠ s
    then
      customErrorWithOffset start $
        FixenTrivialParseError
          (Just (textToTokens ls))
          (Set.singleton (textToTokens s))
          []
          []
    else return s
  where
    getLongestMatchingToken :: [Char] -> Parser σ Text
    getLongestMatchingToken [] = return ""
    getLongestMatchingToken (x : xs) = do
      x' <- P.anySingle
      if x ≠ x'
        then return $ Data.Text.singleton x'
        else do
          xs' <- getLongestMatchingToken xs
          return $ Data.Text.cons x' xs'

-- | A helper function for converting text into a series of tokens for
-- error reporting
--
-- @since 26.8
textToTokens :: Text -> P.ErrorItem (P.Token Text)
textToTokens t = case unpack t of
  [] -> error "text is empty, cannot convert to tokens!"
  (x : xs) -> P.Tokens (x :| xs)
