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
--     * 'Parser' and 'ParserState' — the core parser type and state
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
module Fixen.Parser.Common (
   Parser,
   ParserState,
   commaSepBy1,
   commaSepBy1',
   commaSepBy2,
   commaSepBy2',
   commaSepBy,
   commaSepBy',
   indented,
   indentedByMoreThan,
   indentedByExactly,
   sc,
   l,
   parsePositioned,
   betweenParentheses,
   betweenSquareBrackets,
   betweenCurlyBraces,
   manyI,
   manyI',
   someI,
   someI',
  ) where

import Data.Text (Text)
import Data.Void (Void)

import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Maybe ()
import Data.List.NonEmpty
import Error.Diagnose qualified as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Fixen.Data.NodeId (HasNodeId, NodeId)
import Fixen.Monad
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos qualified as MPos

-- | The state carried by the parser.
--
--   This is a product of three components:
--
--   * 'PositionEnv' — maps 'NodeId' values to source positions
--   * 'NodeId' — the current node ID counter (incremented on each new node)
--   * 'FixenErrors' — accumulated error diagnostics
--
--   This state is shared across all parser combinators and is updated
--   as the parser consumes input and constructs AST nodes.
type ParserState = (PositionEnv :*: NodeId :*: FixenErrors)

-- | The parser type used by the Fixen parser.
--
--   This is a Megaparsec parser ('P.ParsecT') layered on top of:
--
--   * 'State.State' — for mutable state (the 'ParserState')
--   * 'IO' — for file reads and error output
--
--   The 'Void' error token means this parser does not produce token-level
--   parse errors (all errors are handled at a higher level via the
--   'FixenErrors' accumulator).
--
--   In practice, you will rarely construct values of type 'Parser' directly.
--   Instead, use the combinators provided in this module and the higher-level
--   parsers in 'Fixen.Parser'.
type Parser = P.ParsecT Void Text (State.State ParserState)

-- | Parse one or more items separated by commas, with indentation checking.
--
--   This parser:
--
--   1. Checks that the first item is properly indented (via @indent_check@)
--   2. Parses the first item
--   3. Repeatedly parses @,@ followed by an indented item
--   4. Returns all items as a 'NonEmpty' list
--
--   The @indent_check@ argument is a parser that verifies the current
--   position has the required indentation. This ensures that comma-separated
--   lists are properly nested within their parent construct.
--
--   Example output for @commaSepBy1 indented parseExpr@ on input
--   @\"a, b, c\"@ (with proper indentation):
--
--   @
--   a :| [b, c]
--   @
commaSepBy1
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  -> Parser a
  -- ^ The parser for individual items
  -> Parser (NonEmpty a)
commaSepBy1 indent_check p = do
  -- Verify the first item is properly indented
  _ <- indent_check
  -- Parse the first item
  e <- p
  -- Parse zero or more additional items, each preceded by a comma and
  -- an indentation check
  ls <- manyI indent_check (l (P.single ',') *> p)
  -- Return the non-empty list of all items
  return $ e :| ls

-- | Parse two or more items separated by commas, with indentation checking.
--
--   Like 'commaSepBy1' but returns the first item separately from the
--   rest, as @(a, NonEmpty a)@. This is useful when the semantics of
--   the parsed construct distinguish the first element from the others.
--
--   For example, a tuple type @(T1, T2, T3)@ could be parsed as
--   @(T1, T2 :| [T3])@, allowing the parser to handle the first
--   element differently if needed.
commaSepBy2
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  -> Parser a
  -- ^ The parser for individual items
  -> Parser (a, NonEmpty a)
commaSepBy2 indent_check p = do
  -- Verify the first item is properly indented
  _ <- indent_check
  -- Parse the first item
  e <- p
  -- Verify the second item is properly indented
  _ <- indent_check
  -- Parse the comma between the first and second items
  _ <- P.single ','
  -- Verify the second item is properly indented
  _ <- indent_check
  -- Parse the second item
  e' <- p
  -- Parse zero or more additional items
  ls <- manyI indent_check (l (P.single ',') *> p)
  -- Return (first, non-empty list starting with second)
  return (e, e' :| ls)

-- | Parse two or more comma-separated items, using 'indented' for indentation.
--
--   This is the convenience variant of 'commaSepBy2' that uses 'indented'
--   as the indentation check, so you only need to provide the item parser.
commaSepBy2' :: Parser a -> Parser (a, NonEmpty a)
commaSepBy2' = commaSepBy2 indented

-- | Parse one or more comma-separated items, using 'indented' for indentation.
--
--   This is the convenience variant of 'commaSepBy1' that uses 'indented'
--   as the indentation check, so you only need to provide the item parser.
commaSepBy1' :: Parser a -> Parser (NonEmpty a)
commaSepBy1' = commaSepBy1 indented

-- | Parse zero or more comma-separated items, with indentation checking.
--
--   This parser first attempts to parse one or more items. If that fails,
--   it returns an empty list. This allows constructs like @[]@ (empty list)
--   to be valid.
--
--   The implementation uses 'P.observing' to attempt the non-empty parse
--   without consuming input on failure, then falls back to an empty list.
commaSepBy :: Parser MPos.Pos -> Parser a -> Parser [a]
commaSepBy indent_check p = do
  -- Try to parse at least one item; if it fails, return empty list
  x <- P.observing $ P.try $ do
    _ <- sc
    _ <- indent_check
    p
  case x of
    -- Parsing failed — return empty list
    Left _ -> return []
    -- Got first item — parse remaining items
    Right e -> do
      (:) e <$> commaSepByCommaFirst indent_check p

commaSepBy' :: Parser a -> Parser [a]
commaSepBy' = commaSepBy indented

-- | Internal helper for 'commaSepBy': parses comma-first sequences.
--
--   This is used by 'commaSepBy' to parse the remaining items after
--   the first one has already been consumed. It checks for a comma
--   followed by an item, recursing until no more commas are found.
commaSepByCommaFirst :: Parser MPos.Pos -> Parser a -> Parser [a]
commaSepByCommaFirst indent_check p = do
  -- Try to parse a comma followed by an item
  com <- P.observing $ P.try $ indent_check *> l (P.single ',')
  case com of
    -- No comma found — end of list
    Left _ -> return []
    -- Comma found — parse item and recurse
    Right _ -> do
      e <- p
      ls <- commaSepByCommaFirst indent_check p
      return $ e : ls

-- | Ensure that the current position is indented by at least one character
-- relative to the previous line.
--
--   This is the fundamental indentation guard used throughout the parser.
--   In Fixen, every syntactic element that is not a top-level declaration
--   must be indented. This prevents ambiguity where a wrongly placed
--   declaration could be confused as belonging to a previous construct.
--
--   For example, in a Fixen file:
--
--   @
--   relation MyRel :
--       A,
--   B        -- ERROR: not indented, parser will reject
--   @
--
--   The 'indented' guard ensures that @B@ is rejected because it is not
--   indented relative to the previous line.
indented :: Parser MPos.Pos
indented = indentedByMoreThan (MPos.mkPos 1)

-- | Ensure that the current position is indented by strictly more than
-- the given amount (in columns).
--
--   This uses Megaparsec's 'L.indentGuard' with the 'GT' (greater than)
--   comparison. It first consumes whitespace via 'sc', then checks that
--   the current column is strictly greater than the given threshold.
--
--   Use this when you need fine-grained control over indentation levels,
--   such as requiring exactly 4 spaces of indentation or more.
indentedByMoreThan :: MPos.Pos -> Parser MPos.Pos
indentedByMoreThan = L.indentGuard sc GT

-- | Ensure that the current position is indented by exactly the given
-- amount (in columns).
--
--   This uses Megaparsec's 'L.indentGuard' with the 'EQ' (equal)
--   comparison. It first consumes whitespace via 'sc', then checks that
--   the current column exactly matches the given threshold.
--
--   Use this when a specific indentation level is required, such as
--   aligning items in a list at a fixed column.
indentedByExactly :: MPos.Pos -> Parser MPos.Pos
indentedByExactly = L.indentGuard sc EQ

-- | Parse zero or more items, each preceded by an indentation check.
--
--   This is the indentation-aware variant of 'many'. It parses items
--   recursively, checking indentation before each one. The recursion
--   terminates when the indentation check fails (i.e., the next item
--   is not properly indented, indicating the end of the list).
--
--   All but the last parsed item will have whitespace consumed after
--   them (via the '@indent_check @*>' pattern in the caller).
--
--   Example: parsing a list of expressions in a Fixen rule:
--
--   @
--   rule myRule:
--       expr1,
--       expr2,
--       expr3
--   @
--
--   Here, 'manyI indented (parseExpr)' would parse all three expressions.
manyI
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  -> Parser a
  -- ^ The parser for individual items
  -> Parser [a]
manyI indent_check p = do
  -- Try to parse an item with an indentation check
  m <- P.observing (P.try $ indent_check *> p)
  case m of
    -- Indentation check failed — end of list
    Left _ -> return []
    -- Got an item — recurse to parse more
    Right e -> (:) e <$> manyI indent_check p

-- | Parse one or more items, using 'indented' for indentation.
--
--   This is the convenience variant of 'manyI' that uses 'indented'
--   as the indentation check.
manyI' :: Parser a -> Parser [a]
manyI' = manyI indented

-- | Parse one or more items, each preceded by an indentation check.
--
--   This is the indentation-aware variant of 'some'. It parses at least
--   one item (checking indentation), then recursively parses zero or more
--   additional items.
--
--   Returns a 'NonEmpty' list, guaranteeing at least one element.
someI
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  -> Parser a
  -- ^ The parser for individual items
  -> Parser (NonEmpty a)
someI indent_check p = do
  -- Check indentation before the first item
  _ <- indent_check
  -- Parse the first item
  e <- p
  -- Parse zero or more additional items
  (:|) e <$> manyI indent_check p

-- | Parse one or more items, using 'indented' for indentation.
--
--   This is the convenience variant of 'someI' that uses 'indented'
--   as the indentation check.
someI' :: Parser a -> Parser (NonEmpty a)
someI' = someI indented

-- | The whitespace and comment consumer.
--
--   This handles:
--
--   * Single-line whitespace (spaces and tabs via 'C.space1')
--   * Single-line comments starting with @--@
--   * Block comments delimited by @{-@ and @-}@
--
--   This is used by all other combinators in this module to consume
--   whitespace and comments before parsing meaningful tokens.
--
--   The 'sc' (space consumer) is a standard Megaparsec convention:
--   it is the primary whitespace handler, and other combinators like
--   'l' (lexeme) and 'symbol' build on top of it.
sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

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
l :: Parser a -> Parser a
l = L.lexeme sc

-- | Provide a dummy 'HasHints' instance for 'Void'.
--
--   This suppresses the orphan instance warning. Since our parser uses
--   'Void' as the error token type, it does not produce token-level
--   hints. All error reporting is handled through the 'FixenErrors'
--   accumulator instead.
instance HasHints Void String where
  hints _ = []

-- | Parse an AST node and record its source position in the 'PositionEnv'.
--
--   This is the primary mechanism for attaching source positions to AST
--   nodes. It works by:
--
--   1. Recording the source position before parsing (@start@)
--   2. Running the inner parser to parse the node
--   3. Recording the source position after parsing (@end@)
--   4. Computing a 'Diag.Position' from the start and end positions
--   5. Storing the position in the state, keyed by the node's 'NodeId'
--   6. Returning the parsed node unchanged
--
--   The 'NodeId' of the node is used as the key in the 'PositionEnv'.
--   This 'NodeId' is assigned when the node is constructed (via
--   'Fixen.Monad.Env.NodeId.fixenGetNewNodeId'), and the position
--   is stored by 'fixenSetPosition'.
--
--   Usage pattern:
--
--   @
--   -- Parse a value and record its position:
--   myParser :: Parser MyASTNode
--   myParser = parsePositioned $ do
--       -- ... parse the node ...
--       return node
--   @
--
--   The outer 'l' wrapper (from 'l') should be used to consume trailing
--   whitespace:
--
--   @
--   myParser = l $ parsePositioned $ do
--       -- ... parse ...
--   @
--
--   This ensures the end position does not include trailing whitespace.
parsePositioned :: HasNodeId a => Parser a -> Parser a -- (Diag.Position, a)
parsePositioned p = do
  -- Record the source position before parsing
  start <- P.getSourcePos
  -- Parse the AST node
  a <- p
  -- Record the source position after parsing
  end <- P.getSourcePos
  -- Compute the source position range
  let pos = mkDiagPos start end
  -- Store the position in the state, keyed by the node's NodeId
  fixenSetPosition a pos
  -- Return the parsed node (unchanged)
  return a
  where
    -- Construct a Diag.Position from Megaparsec source positions
    mkDiagPos :: MPos.SourcePos -> MPos.SourcePos -> Diag.Position
    mkDiagPos start end =
      -- Extract file name, line and column for start and end
      let file_path = MPos.sourceName start
          start_line = MPos.unPos $ MPos.sourceLine start
          start_col = MPos.unPos $ MPos.sourceColumn start
          end_line = MPos.unPos $ MPos.sourceLine end
          end_col = MPos.unPos $ MPos.sourceColumn end
      -- Construct the diagnostic position
      in  Diag.Position
            { Diag.begin = (start_line, start_col)
            , Diag.end = (end_line, end_col)
            , Diag.file = file_path
            }

-- | Parse an expression delimited by parentheses @(@ ... @)@, with
-- indentation checking after the closing parenthesis.
--
--   This is a convenience wrapper around 'P.between' that parses
--   balanced parentheses around a sub-expression. The indentation
--   check after the closing parenthesis ensures that the closing
--   paren is at the expected indentation level.
--
--   Example: @betweenParentheses indent_check parseExpr@ parses
--   @\"( expr )\"@ where @expr@ is a properly indented expression.
betweenParentheses :: Parser MPos.Pos -> Parser a -> Parser a
betweenParentheses indent_check = P.between (L.symbol sc "(") (indent_check >> ")")

-- | Parse an expression delimited by square brackets @[@ ... @]@, with
-- indentation checking after the closing bracket.
--
--   This is a convenience wrapper around 'P.between' that parses
--   balanced square brackets around a sub-expression.
--
--   Example: @betweenSquareBrackets indent_check parseExpr@ parses
--   @\"[ expr ]\"@ where @expr@ is a properly indented expression.
betweenSquareBrackets :: Parser MPos.Pos -> Parser a -> Parser a
betweenSquareBrackets indent_check = P.between (L.symbol sc "[") (indent_check >> "]")

-- | Parse an expression delimited by curly braces @{@ ... @}@, with
-- indentation checking after the closing brace.
--
--   This is a convenience wrapper around 'P.between' that parses
--   balanced curly braces around a sub-expression.
--
--   Example: @betweenCurlyBraces indent_check parseExpr@ parses
--   @\"{ expr }\"@ where @expr@ is a properly indented expression.
betweenCurlyBraces :: Parser MPos.Pos -> Parser a -> Parser a
betweenCurlyBraces indent_check = P.between (L.symbol sc "{") (indent_check >> "}")
