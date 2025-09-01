{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Mozzarella.Parser.Type
--     Description : Parsers for Mozzarella (Haskell) types.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for Mozzarella types. Currently, types in Mozzarella
--     are of the following grammar:
--
--     @
--     type ::= \<atom_type\>+ -- Type applications
--
--     atom_type ::= '(' \<type\> ')'
--                |  \<ident\>
--     @
--
--     "Atom types" (@\<atom_type\>@) are easier to parse, which is why
--     they are split off from the main @\<type\>@ production.
module Mozzarella.Parser.Type where

import Control.Applicative.Combinators (
  (<|>),
 )
import Control.Monad
import Data.List
import Data.List.NonEmpty
import Error.Diagnose.Position qualified as DPos
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Core
import Mozzarella.Parser.Common
import Mozzarella.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Pos qualified as MPos

-- | Parses an 'AST.Type' at the top-level.
parseType :: Parser MPos.Pos -> Parser AST.Type
parseType indentCheck = P.try (parseInfixType indentCheck) <|> parseTypeApp indentCheck

-- | Parses an infix 'AST.Type'. This is a top-level infix type, so
-- the colon @(:)@ is not allowed unless the type is enclosed in parentheses.
parseInfixType :: Parser MPos.Pos -> Parser AST.Type
parseInfixType indentCheck = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the type is not at the
    -- top-level of indentation. intermediate parses are wrapped with 'l'.
    _ <- indentCheck
    lhs <- l (parseParenType indentCheck)
    _ <- indentCheck
    -- Make sure that at the top level, you cannot use the colon!!
    op <- l parseOpIdentifier
    when (identifier op == ":") $
      fail "(:) cannot appear in type without being enclosed in parentheses"
    _ <- indentCheck
    rhs <- parseParenType indentCheck
    -- Apply op to lhs.
    let first_app :: AST.Type =
          -- calculate the stupid annotation. since lhs comes before op, we use
          -- the start position of lhs as the start of the app, and the end
          -- position of op as the end of the app. However, the internal
          -- representation of this is (op e).
          let start_pos = DPos.begin $ AST.getPosition lhs
              end_pos = DPos.end $ AST.getPosition op
              file_name = DPos.file $ AST.getPosition lhs
              pos =
                -- This is the source position of the fn app we are building
                DPos.Position
                  { DPos.begin = start_pos
                  , DPos.end = end_pos
                  , DPos.file = file_name
                  }
              -- Reconstruct operator identifier as an expression
              op_expr = TypeName (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              TypeApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ TypeApp pos first_app rhs

-- | Parses a type variable, which is just a capitalized identifier.
parseTypeVar :: Parser MPos.Pos -> Parser AST.Type
parseTypeVar indentCheck = do
  -- must be indented
  _ <- indentCheck
  -- Parse the identifier. Can be either capitalized or a non-infix op.
  ident <- P.try parseCapitalizedIdentifier <|> parseNonInfixOpIdentifier indentCheck
  -- they can share annotations
  let pos = AST.getPosition ident
  return $ TypeName pos ident

-- | Parses an application expression in non infix notation.
parseTypeApp :: Parser MPos.Pos -> Parser AST.Type
parseTypeApp indent_check = do
  -- Essentially, an expression application is a list of atom expressions.
  -- Each expression must be indented
  (x :| xs) <- someI indent_check (parseParenType indent_check)
  -- Fold the list as an application.
  return $ foldl' folder x xs
  where
    -- Builds function applications with a pair.
    folder :: AST.Type -> AST.Type -> AST.Type
    folder t t' =
      -- calculate source positions.
      let startPos = DPos.begin $ AST.getPosition t
          endPos = DPos.end $ AST.getPosition t'
          file_name = DPos.file $ AST.getPosition t
          new_pos =
            DPos.Position
              { DPos.begin = startPos
              , DPos.end = endPos
              , DPos.file = file_name
              }
      in  -- make the app.
          TypeApp new_pos t t'

-- | Parses an integer literal expression, such as @42@ or @0@.
parseTypeNatLit :: Parser MPos.Pos -> Parser AST.Type
parseTypeNatLit indent_check = do
  _ <- indent_check
  (pos, n) <- parsePositioned parseRawNatural
  return $ TypeNatLit pos n

-- | Parses a string literal expression, such as @"hello"@ or @""@.
parseTypeSymbolLit :: Parser MPos.Pos -> Parser AST.Type
parseTypeSymbolLit indent_check = do
  _ <- indent_check
  (pos, str) <- parsePositioned parseRawString
  return $ TypeSymbolLit pos str

-- | Parses a unit literal, i.e., @()@.
parseTypeUnit :: Parser MPos.Pos -> Parser AST.Type
parseTypeUnit indent_check = do
  _ <- indent_check
  (pos, _) <- parsePositioned $ betweenParentheses indent_check (return ())
  return $ TypeUnit pos

-- | Parses a tuple literal, such as @(e1, e2, e3)@. Note that @(e)@ is not a
-- tuple literal, but just @e@ enclosed in parentheses.
parseTupleType :: Parser MPos.Pos -> Parser AST.Type
parseTupleType indent_check = do
  _ <- indent_check
  (pos, (e, exprs)) <-
    parsePositioned $
      betweenParentheses
        indent_check
        (commaSepBy2 indent_check (parseAnyType indent_check))
  return $ TypeTuple pos e exprs

-- | Parses a list literal, such as @[e1, e2, e3]@. Note that @[]@ is the empty
-- list literal.
parseListType :: Parser MPos.Pos -> Parser AST.Type
parseListType indent_check = do
  _ <- indent_check
  (pos, exprs) <-
    parsePositioned $
      betweenSquareBrackets
        indent_check
        (parseAnyType indent_check)
  return $ TypeList pos exprs

parseAnyType :: Parser MPos.Pos -> Parser AST.Type
parseAnyType indent_check =
  P.try (parseNestedInfixType indent_check)
    <|> parseTypeApp indent_check

-- | Parses an atomic (parenthesized) expression
parseParenType :: Parser MPos.Pos -> Parser AST.Type
parseParenType indent_check =
  P.try f
    <|> P.try (parseTypeUnit indent_check)
    <|> P.try (parseTupleType indent_check)
    <|> P.try (parseListType indent_check)
    <|> P.try (parseTypeVar indent_check)
    <|> P.try (parseTypeNatLit indent_check)
    <|> parseTypeSymbolLit indent_check
  where
    f = do
      _ <- indented
      (pos, t) <- parsePositioned $ betweenParentheses indent_check item
      return $ AST.setPosition pos t
    item = P.try (parseNestedInfixType indent_check) <|> parseTypeApp indent_check

-- | Parses an infix 'ASTType' that is part of a larger expression, i.e.,
-- is enclosed in parentheses.
parseNestedInfixType :: Parser MPos.Pos -> Parser AST.Type
parseNestedInfixType indent_check = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the expression is not at the
    -- top-level. intermediate parses are wrapped with 'l'.
    _ <- indent_check
    lhs <- l (parseParenType indent_check)
    _ <- indent_check
    op <- l parseOpIdentifier
    _ <- indent_check
    rhs <- parseParenType indent_check
    -- Apply op to lhs.
    let first_app :: AST.Type =
          -- calculate the stupid annotation. since lhs comes before op, we use
          -- the start position of lhs as the start of the app, and the end
          -- position of op as the end of the app. However, the internal
          -- representation of this is (op e).
          let start_pos = DPos.begin $ AST.getPosition lhs
              end_pos = DPos.end $ AST.getPosition op
              file_name = DPos.file $ AST.getPosition lhs
              pos =
                -- This is the source position of the fn app we are building
                DPos.Position
                  { DPos.begin = start_pos
                  , DPos.end = end_pos
                  , DPos.file = file_name
                  }
              -- Reconstruct operator identifier as an expression
              op_expr = TypeName (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              TypeApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ TypeApp pos first_app rhs
