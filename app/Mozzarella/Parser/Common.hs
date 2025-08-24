{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Shut the orphan instance warning up because it's very annoying.

-- |
--     Module      : Mozzarella.Parser.Common
--     Description : Common utilities for the Mozzarella parser
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Common utilities for the Mozzarella parser.
module Mozzarella.Parser.Common (
  Parser,
  commaSepBy1,
  commaSepBy,
  indented,
  comma,
  sc,
  l,
  parsePositioned,
  betweenParentheses,
) where

import Data.Text (Text)
import Data.Void (Void)
import Error.Diagnose qualified as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos qualified as MPos

-- | The type of our parser.
type Parser = P.Parsec Void Text

-- | Parses some (one or more) items, separated by the comma @,@. Commas
-- are 'indented'.
commaSepBy1 :: Parser a -> Parser [a]
commaSepBy1 p = P.sepBy1 p comma

-- | Parses many (zero or more) items, separated by the comma @,@. Commas
-- are 'indented'.
commaSepBy :: Parser a -> Parser [a]
commaSepBy p = P.sepBy p comma

-- | A clause to ensure that the current token is indented by at least one
-- character. The key observation is that everything except the top-level
-- declarations should have at least some indentation. Otherwise, things just
-- look weird and the parser has no idea whether a wrongly declared top-level
-- declaration is part of the previous top-level declaration or not.
indented :: Parser MPos.Pos
indented = L.indentGuard sc GT (MPos.mkPos 1)

-- | The parser for an 'indented' comma @,@.
comma :: Parser Char
comma = indented *> l (P.single ',')

-- | The space consumer. Double dashes @--@ are single-line comment indicators,
-- and block comments are opened and closed with @/-@ and @-/@ respectively.
sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "/-" "-/")

-- | Parses something and consumes all whitespace and comments after.
l :: Parser a -> Parser a
l = L.lexeme sc

instance HasHints Void String where
  hints _ = []

-- | Obtains the start and end source positions from parsing something with
-- a parser. Importantly, ensure that the parser does not consume any
-- whitespace and comments afterwards, otherwise the positions reported will
-- include the trailing whitespace.
--
-- The way to use this would be something like this:
--
-- @
-- myParser :: Parser (Position, String)
-- myParser = l $ parsePositioned $ string "foo"
-- @
--
-- This way, @myParser@ will consume all whitespace after @foo@, but the
-- source position for @foo@ will not include the whitespace
parsePositioned :: Parser a -> Parser (Diag.Position, a)
parsePositioned p = do
  start <- P.getSourcePos
  a <- p
  end <- P.getSourcePos
  let pos = mkDiagPos start end
  return (pos, a)
  where
    mkDiagPos :: MPos.SourcePos -> MPos.SourcePos -> Diag.Position
    mkDiagPos start end =
      let file_path = MPos.sourceName start
          start_line = MPos.unPos $ MPos.sourceLine start
          start_col = MPos.unPos $ MPos.sourceColumn start
          end_line = MPos.unPos $ MPos.sourceLine end
          end_col = MPos.unPos $ MPos.sourceColumn end
      in  Diag.Position
            { Diag.begin = (start_line, start_col)
            , Diag.end = (end_line, end_col)
            , Diag.file = file_path
            }

-- | Parses using a parser that is between parentheses @(@ and @)@
betweenParentheses :: Parser a -> Parser a
betweenParentheses = P.between (L.symbol sc "(") (L.symbol sc ")")
