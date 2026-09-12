{-# LANGUAGE OverloadedStrings #-}

-- | A deliberately small Haskell pattern grammar. Relation arguments are
-- atomic; constructor applications and infix patterns must be parenthesized.
-- Infix nesting is explicit (as in Fixen expressions), so foreign fixities do
-- not need to be known by Fixen.
module Fixen.Parser.Pattern (parsePatternAtom, parsePattern) where

import Control.Applicative (optional, (<|>))
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

parsePatternAtom :: ParserState σ => Parser σ Pattern
parsePatternAtom =
  l $
    parsePositioned $
      (PatternVar <$> parseLowerFirstSimpleIdentifierOrHole)
        <|> P.try constructor
        <|> parenthesized
        <|> listPattern
        <|> (do n <- parseRawInteger; i <- getNewNodeId; pure (PatternInt i n))
        <|> (do s <- parseRawString; i <- getNewNodeId; pure (PatternString i s))
        <|> (do c <- C.char '\'' *> L.charLiteral <* C.char '\''; i <- getNewNodeId; pure (PatternChar i c))
  where
    constructor = do
      c <- P.try parseCapitalizedIdentifier <|> betweenParentheses indented constructorOperator
      i <- getNewNodeId
      pure (PatternCon i c [])
    parenthesized = betweenParentheses indented $ do
      ps <- commaSepBy' parsePattern
      case ps of
        [p] -> pure p
        _ -> do i <- getNewNodeId; pure (PatternTuple i ps)
    listPattern = do
      ps <- betweenSquareBrackets indented (commaSepBy' parsePattern)
      i <- getNewNodeId
      pure (PatternList i ps)

parsePattern :: ParserState σ => Parser σ Pattern
parsePattern = parsePositioned $ do
  first <- parsePatternAtom
  lhs <- case first of
    PatternCon i c [] -> PatternCon i c <$> manyI' parsePatternAtom
    _ -> pure first
  infixOp <- optional (P.try (indented *> l (P.try (parseInfixTypeLetterIdentifier indented) <|> constructorOperator)))
  case infixOp of
    Nothing -> pure lhs
    Just op -> do
      rhs <- indented *> parsePatternAtom
      i <- getNewNodeId
      pure (PatternCon i op [lhs, rhs])

-- ':' is reserved in Fixen declarations, but is the list constructor here.
constructorOperator :: ParserState σ => Parser σ Identifier
constructorOperator = P.try parseTypeOpIdentifier <|> (IdentifierSimpleIdentifier <$> parsePositioned cons)
  where
    cons = do
      _ <- C.char ':' <* P.notFollowedBy (P.satisfy isValidOpChar)
      i <- getNewNodeId
      pure (SimpleIdentifier i ":")
