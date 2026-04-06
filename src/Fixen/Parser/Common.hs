{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Shut the orphan instance warning up because it's very annoying.

-- |
--     Module      : Fixen.Parser.Common
--     Description : Common utilities for the Fixen parser
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Common utilities for the Fixen parser.
module Fixen.Parser.Common (
  Parser,
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

-- import Control.Applicative.Combinators
import Data.List.NonEmpty
import Error.Diagnose qualified as Diag
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos qualified as MPos

-- | The type of our parser.
type Parser = P.Parsec Void Text

-- | Parses some (one or more) items, separated by the comma @,@. Commas
-- are indented by some customizable indentation level.
commaSepBy1
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level
  -> Parser a
  -- ^ The parser for the items
  -> Parser (NonEmpty a)
commaSepBy1 indent_check p = do
  _ <- indent_check
  e <- p
  ls <- manyI indent_check (l (P.single ',') *> p)
  return $ e :| ls

-- | Parses two or more items, separated by the comma @,@. Commas
-- are indented by some customizable indentation level.
commaSepBy2
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level
  -> Parser a
  -- ^ The parser for the items
  -> Parser (a, NonEmpty a)
commaSepBy2 indent_check p = do
  _ <- indent_check
  e <- p
  _ <- indent_check
  _ <- P.single ','
  _ <- indent_check
  e' <- p
  ls <- manyI indent_check (l (P.single ',') *> p)
  return (e, e' :| ls)

-- | Parses two or more items, separated by the comma @,@. Commas are 'indented'.
commaSepBy2' :: Parser a -> Parser (a, NonEmpty a)
commaSepBy2' = commaSepBy2 indented

-- | Parses some (one or more) items, separated by the comma @,@. Commas
-- are 'indented'.
commaSepBy1' :: Parser a -> Parser (NonEmpty a)
commaSepBy1' = commaSepBy1 indented

-- | Parses many (zero or more) items, separated by the comma @,@. Commas
-- are 'indented'.
commaSepBy :: Parser MPos.Pos -> Parser a -> Parser [a]
commaSepBy indent_check p = do
  x <- P.observing $ P.try $ do
    _ <- sc
    _ <- indent_check
    p
  case x of
    Left _ -> return []
    Right e -> do
      (:) e <$> commaSepByCommaFirst indent_check p

commaSepBy' :: Parser a -> Parser [a]
commaSepBy' = commaSepBy indented

commaSepByCommaFirst :: Parser MPos.Pos -> Parser a -> Parser [a]
commaSepByCommaFirst indent_check p = do
  com <- P.observing $ P.try $ indent_check *> l (P.single ',')
  case com of
    Left _ -> return []
    Right _ -> do
      e <- p
      ls <- commaSepByCommaFirst indent_check p
      return $ e : ls

-- | A clause to ensure that the current token is indented by at least one
-- character. The key observation is that everything except the top-level
-- declarations should have at least some indentation. Otherwise, things just
-- look weird and the parser has no idea whether a wrongly declared top-level
-- declaration is part of the previous top-level declaration or not.
indented :: Parser MPos.Pos
indented = indentedByMoreThan (MPos.mkPos 1)

-- | A clause to ensure that the current token is indented by some amount.
-- This is mainly used for parsing priority declarations where multiple
-- orders can be declared. In those cases, we want to ensure that within
-- each order declaration, the tokens are indented by more than the
-- indentation of the order declaration itself.
indentedByMoreThan :: MPos.Pos -> Parser MPos.Pos
indentedByMoreThan = L.indentGuard sc GT

-- | A clause to ensure that the current token is indented by some amount.
-- This is mainly used for parsing priority declarations where multiple
-- orders can be declared. In those cases, we want to ensure that within
-- each order declaration, the tokens are indented by more than the
-- indentation of the order declaration itself.
indentedByExactly :: MPos.Pos -> Parser MPos.Pos
indentedByExactly = L.indentGuard sc EQ

-- | Given a parser @p@, runs @p@ as many times as possible and returns
-- all the results. Indentation checks for @p@ will be performed.
-- All but the last parse of @p@ will consume whitespace after.
manyI
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level
  -> Parser a
  -- ^ The parser for the items
  -> Parser [a]
manyI indent_check p = do
  m <- P.observing (P.try $ indent_check *> p)
  case m of
    Left _ -> return []
    Right e -> (:) e <$> manyI indent_check p

-- | Given a parser @p@, runs @p@ at least once and returns
-- all the results. The items are 'indented'. All but the last parse of @p@ will
-- consume whitespace after.
manyI'
  :: Parser a
  -- ^ The parser for the items
  -> Parser [a]
manyI' = manyI indented

-- | Given a parser @p@, runs @p@ as at least once and as many times as possible and returns
-- all the results. Indentation checks for @p@ will be performed.
-- All but the last parse of @p@ will consume whitespace after.
someI
  :: Parser MPos.Pos
  -- ^ The parser that checks the indentation level
  -> Parser a
  -- ^ The parser for the items
  -> Parser (NonEmpty a)
someI indent_check p = do
  _ <- indent_check
  e <- p
  (:|) e <$> manyI indent_check p

-- | Given a parser @p@, runs @p@ at least once and returns all the results. The items
-- are 'indented'. All but the last parse of @p@ will consume whitespace after.
someI'
  :: Parser a
  -- ^ The parser for the items
  -> Parser (NonEmpty a)
someI' = someI indented

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
betweenParentheses :: Parser MPos.Pos -> Parser a -> Parser a
betweenParentheses indent_check = P.between (L.symbol sc "(") (indent_check >> ")")

-- | Parses using a parser that is between square brackets @[@ and @]@
betweenSquareBrackets :: Parser MPos.Pos -> Parser a -> Parser a
betweenSquareBrackets indent_check = P.between (L.symbol sc "[") (indent_check >> "]")

-- | Parses using a parser that is between curly braces @{@ and @}@
betweenCurlyBraces :: Parser MPos.Pos -> Parser a -> Parser a
betweenCurlyBraces indent_check = P.between (L.symbol sc "{") (indent_check >> "}")
