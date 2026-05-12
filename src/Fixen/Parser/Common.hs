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
-- @since 0.0.1
module Fixen.Parser.Common where

import Control.Monad.State.Strict
import Data.List.NonEmpty
import Data.Text
import Data.Void
import Error.Diagnose.Compat.Megaparsec
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
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
-- @since 0.0.1
type ParserState σ = (WithPositionEnv σ, NodeIded σ, WithErrors σ)

-- | The parser type used by the Fixen parser.
--
--   This is a Megaparsec parser ('P.ParsecT') layered on top of the 'State'
--   monad.
--
--   The 'Void' error token means this parser does not produce token-level
--   parse errors (all errors are handled at a higher level via the
--   'FixenErrors' accumulator).
--
--   In practice, you will rarely construct values of type 'Parser' directly.
--   Instead, use the combinators provided in this module and the higher-level
--   parsers in 'Fixen.Parser'.
--
-- @since 0.0.1
type Parser σ = P.ParsecT Void Text (State σ)

--------------------------------------------------------------------------------

-- * Parser Combinators

--------------------------------------------------------------------------------

-- ** Comma separation

-- | Parse zero or more comma-separated items, with indentation checking.
--
-- @since 0.0.1
commaSepBy
  :: Parser σ MPos.Pos
  -- ^ The indentation check
  --
  -- @since 0.0.1
  -> Parser σ α
  -- ^ The parser for a single item
  --
  -- @since 0.0.1
  -> Parser σ [α]
commaSepBy indent_check p = do
  x <- P.observing $ P.try $ do
    _ <- sc
    _ <- indent_check
    p
  case x of
    Left _ -> return []
    Right e -> (e :) <$> commaSepByCommaFirst indent_check p
  where
    commaSepByCommaFirst i p' = do
      com <- P.observing $ P.try $ i *> l (P.single ',')
      case com of
        Left _ -> return []
        Right _ -> do
          e <- p'
          ls <- commaSepByCommaFirst i p'
          return $ e : ls

-- | This is 'commaSepBy' with 'indented'.
--
-- @since 0.0.1
commaSepBy'
  :: ParserState σ
  => Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ [α]
commaSepBy' = commaSepBy indented

-- | Parse one or more items separated by commas, with indentation checking.
--
-- @since 0.0.1
commaSepBy1
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 0.0.1
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ (NonEmpty α)
commaSepBy1 indent_check p = do
  _ <- indent_check
  e <- p
  ls <- manyI indent_check (l (P.single ',') *> p)
  return $ e :| ls

-- | This is 'commaSepBy1', using 'indented' for indentation.
--
-- @since 0.0.1
commaSepBy1'
  :: Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ (NonEmpty α)
commaSepBy1' = commaSepBy1 indented

-- | Parse two or more items separated by commas, with indentation checking.
--
-- @since 0.0.1
commaSepBy2
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 0.0.1
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ (α, NonEmpty α)
commaSepBy2 indent_check p = do
  e <- indent_check *> p <* indent_check
  _ <- P.single ','
  _ <- indent_check
  e' <- p
  ls <- manyI indent_check (l (P.single ',') *> p)
  return (e, e' :| ls)

-- | This is 'commaSepBy2', using 'indented' for indentation.
--
-- @since 0.0.1
commaSepBy2'
  :: Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
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
-- @since 0.0.1
manyI
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 0.0.1
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ [α]
manyI indent_check p = do
  m <- P.observing (P.try $ indent_check *> p)
  case m of
    Left _ -> return []
    Right e -> (:) e <$> manyI indent_check p

-- | This is 'manyI' using 'indented' for indentation.
--
-- @since 0.0.1
manyI'
  :: Parser σ α
  -- ^ The parser for individual items.
  --
  -- @since 0.0.1
  -> Parser σ [α]
manyI' = manyI indented

-- | Parse one or more items, each preceded by an indentation check.
--
-- @since 0.0.1
someI
  :: Parser σ MPos.Pos
  -- ^ The parser that checks the indentation level before each item
  --
  -- @since 0.0.1
  -> Parser σ α
  -- ^ The parser for individual items
  --
  -- @since 0.0.1
  -> Parser σ (NonEmpty α)
someI indent_check p = do
  e <- indent_check *> p
  (:|) e <$> manyI indent_check p

-- | This is 'someI', using 'indented' for indentation.
--
-- @since 0.0.1
someI' :: Parser σ α -> Parser σ (NonEmpty α)
someI' = someI indented

-- ** Parsing Between Delimiters with Indentation Checking

-- | Parse an expression delimited by parentheses @(@ ... @)@.
--
-- @since 0.0.1
betweenParentheses :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenParentheses indent_check = P.between (L.symbol sc "(") (indent_check >> ")")

-- | Parse an expression delimited by square brackets @[@ ... @]@.
--
-- @since 0.0.1
betweenSquareBrackets :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenSquareBrackets indent_check = P.between (L.symbol sc "[") (indent_check >> "]")

-- | Parse an expression delimited by curly braces @{@ ... @}@.
--
-- @since 0.0.1
betweenCurlyBraces :: Parser σ MPos.Pos -> Parser σ α -> Parser σ α
betweenCurlyBraces indent_check = P.between (L.symbol sc "{") (indent_check >> "}")

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
-- @since 0.0.1
indented :: Parser σ MPos.Pos
indented = indentedByMoreThan (MPos.mkPos 1)

-- | Ensure that the current position is indented by strictly more than
-- the given amount (in columns).
--
-- @since 0.0.1
indentedByMoreThan :: MPos.Pos -> Parser σ MPos.Pos
indentedByMoreThan = L.indentGuard sc GT

-- | Ensure that the current position is indented by exactly the given
-- amount (in columns).
--
-- @since 0.0.1
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
-- @since 0.0.1
sc :: Parser σ ()
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
--
-- @since 0.0.1
l :: Parser σ α -> Parser σ α
l = L.lexeme sc

-- | Provide a dummy 'HasHints' instance for 'Void'.
--
--   This suppresses the orphan instance warning. Since our parser uses
--   'Void' as the error token type, it does not produce token-level
--   hints. All error reporting is handled through the 'FixenErrors'
--   accumulator instead.
--
-- @since 0.0.1
instance HasHints Void String where
  hints _ = []

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
-- @since 0.0.1
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
