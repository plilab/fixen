{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser.Expr
--     Description : Parsers for Fixen expressions.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for Fixen expressions. Currently, expressions in Fixen
--     are of the following grammar:
--
--     @
--     expr ::= \<atom_expr\> \<infix_op\> \<atom_expr\>  -- Infix operations
--           |  \<atom_expr\>+                        -- Function applications
--
--     atom_expr ::= '(' \<expr\> ')'
--                |  \<ident\>
--                |  \<int_literal\>
--                |  \<str_literal\>
--     @
--
--     "Atom expressions" (@\<atom_expr\>@) are easier to parse, which is why
--     they are split off from the main @\<expr\>@ production. This is so that
--     we do not need precedence parsing, since infix expressions with compound
--     operands have to be parenthesized.
module Fixen.Parser.Expr (
  parseExpr,
  parseInfixExpr,
  parseExprApp,
  parseParenExpr,
  parseExprVar,
  parseIntLit,
  parseStringLit,
) where

import Control.Applicative.Combinators (
  (<|>),
 )
import Control.Monad (when)
import Data.List (foldl')
import Data.List.NonEmpty
import Error.Diagnose.Position qualified as DPos
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core
import Fixen.Parser.Common
import Fixen.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Pos qualified as MPos

-- | Parses an 'AST.Expr' at the top-level.
parseExpr :: Parser MPos.Pos -> Parser AST.Expr
parseExpr indentCheck = P.try (parseInfixExpr indentCheck) <|> parseExprApp indentCheck

-- | Parses an infix 'ASTExpr'. This is a top-level infix expression, so
-- the turnstile @(|-)@ is not allowed unless the expression is enclosed in parentheses.
parseInfixExpr :: Parser MPos.Pos -> Parser AST.Expr
parseInfixExpr indentCheck = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the expression is not at the
    -- top-level. intermediate parses are wrapped with 'l'.
    _ <- indentCheck
    lhs <- l (parseParenExpr indentCheck)
    _ <- indentCheck
    -- Make sure that at the top level, you cannot use the turnstile!!
    op <- l $ parseInfixTermIdentifier indentCheck
    when (identifier op == "|-") $ fail "turnstile (|-) cannot appear in expression without being enclosed in parentheses"
    _ <- indentCheck
    rhs <- parseParenExpr indentCheck
    -- Apply op to lhs.
    let first_app :: AST.Expr =
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
              op_expr = ExprVar (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              ExprApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ ExprApp pos first_app rhs

-- | Parse an expression variable, in non infix notation.
parseExprVar :: Parser MPos.Pos -> Parser AST.Expr
parseExprVar indentCheck = do
  -- must be indented
  _ <- indentCheck
  -- Parse the identifier
  ident <- parseNonInfixTermIdentifier indentCheck
  -- they can share annotations
  let pos = AST.getPosition ident
  return $ ExprVar pos ident

-- | Parses an application expression in non infix notation.
parseExprApp :: Parser MPos.Pos -> Parser AST.Expr
parseExprApp indent_check = do
  -- Essentially, an expression application is a list of atom expressions.
  -- Each expression must be indented
  (x :| xs) <- someI indent_check (parseParenExpr indent_check)
  -- Fold the list as an application.
  return $ foldl' folder x xs
  where
    -- Builds function applications with a pair.
    folder :: AST.Expr -> AST.Expr -> AST.Expr
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
          ExprApp new_pos t t'

-- | Parses an integer literal expression, such as @42@ or @0@.
parseIntLit :: Parser MPos.Pos -> Parser AST.Expr
parseIntLit indent_check = do
  _ <- indent_check
  (pos, n) <- parsePositioned parseRawInteger
  return $ ExprIntLit pos n

-- | Parses a string literal expression, such as @"hello"@ or @""@.
parseStringLit :: Parser MPos.Pos -> Parser AST.Expr
parseStringLit indent_check = do
  _ <- indent_check
  (pos, str) <- parsePositioned parseRawString
  return $ ExprStrLit pos str

-- | Parses a unit literal, i.e., @()@.
parseUnitLit :: Parser MPos.Pos -> Parser AST.Expr
parseUnitLit indent_check = do
  _ <- indent_check
  (pos, _) <- parsePositioned $ betweenParentheses indent_check (return ())
  return $ ExprUnit pos

-- | Parses a tuple literal, such as @(e1, e2, e3)@. Note that @(e)@ is not a
-- tuple literal, but just @e@ enclosed in parentheses.
parseTupleLit :: Parser MPos.Pos -> Parser AST.Expr
parseTupleLit indent_check = do
  _ <- indent_check
  (pos, (e, exprs)) <-
    parsePositioned $
      betweenParentheses
        indent_check
        (commaSepBy2 indent_check (parseAnyExpr indent_check))
  return $ ExprTuple pos e exprs

-- | Parses a list literal, such as @[e1, e2, e3]@. Note that @[]@ is the empty
-- list literal.
parseListLit :: Parser MPos.Pos -> Parser AST.Expr
parseListLit indent_check = do
  _ <- indent_check
  (pos, exprs) <-
    parsePositioned $
      betweenSquareBrackets
        indent_check
        (commaSepBy indent_check (parseAnyExpr indent_check))
  return $ ExprList pos exprs

parseAnyExpr :: Parser MPos.Pos -> Parser AST.Expr
parseAnyExpr indent_check =
  P.try (parseNestedInfixExpr indent_check)
    <|> parseExprApp indent_check

-- | Parses an atomic (parenthesized) expression
parseParenExpr :: Parser MPos.Pos -> Parser AST.Expr
parseParenExpr indent_check =
  P.try f
    <|> P.try (parseUnitLit indent_check)
    <|> P.try (parseTupleLit indent_check)
    <|> P.try (parseListLit indent_check)
    <|> P.try (parseExprVar indent_check)
    <|> P.try (parseIntLit indent_check)
    <|> parseStringLit indent_check
  where
    f = do
      _ <- indented
      (pos, t) <- parsePositioned $ betweenParentheses indent_check item
      return $ AST.setPosition pos t
    item = P.try (parseNestedInfixExpr indent_check) <|> parseExprApp indent_check

-- | Parses an infix 'ASTExpr' that is part of a larger expression, i.e.,
-- is enclosed in parentheses.
parseNestedInfixExpr :: Parser MPos.Pos -> Parser AST.Expr
parseNestedInfixExpr indent_check = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the expression is not at the
    -- top-level. intermediate parses are wrapped with 'l'.
    _ <- indent_check
    lhs <- l (parseParenExpr indent_check)
    _ <- indent_check
    op <- l $ parseInfixTermIdentifier indent_check
    _ <- indent_check
    rhs <- parseParenExpr indent_check
    -- Apply op to lhs.
    let first_app :: AST.Expr =
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
              op_expr = ExprVar (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              ExprApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ ExprApp pos first_app rhs
