{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Mozzarella.Parser.Expr
--     Description : Parsers for Mozzarella expressions.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for Mozzarella expressions. Currently, expressions in Mozzarella
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
module Mozzarella.Parser.Expr (
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
import Error.Diagnose.Position qualified as DPos
import Mozzarella.IR.AST qualified as AST
import Mozzarella.Parser.Common
import Mozzarella.Parser.Token
import Text.Megaparsec qualified as P

-- | Parses an 'ASTExpr'.
parseExpr :: Parser AST.Expr
parseExpr = P.try parseInfixExpr <|> parseExprApp

-- | Parses an infix 'ASTExpr'.
parseInfixExpr :: Parser AST.Expr
parseInfixExpr = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the expression is not at the
    -- top-level. intermediate parses are wrapped with 'l'.
    _ <- indented
    lhs <- l parseParenExpr
    _ <- indented
    -- Make sure taht at the top level, you cannot use the turnstile!!
    op@(AST.TermIdentifierOp (AST.OpIdentifier _ v)) <- l parseInfixTermIdentifier
    when (v == "|-") $ fail "turnstile (|-) cannot appear in expression without being enclosed in parentheses"
    _ <- indented
    rhs <- parseParenExpr
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
              op_expr = AST.ExprTermVar (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              AST.ExprApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ AST.ExprApp pos first_app rhs

-- | Parse an expression variable, in non infix notation.
parseExprVar :: Parser AST.Expr
parseExprVar = do
  -- must be indented
  _ <- indented
  -- Parse the identifier
  ident <- parseNonInfixTermIdentifier
  -- they can share annotations
  let pos = AST.getPosition ident
  return $ AST.ExprTermVar pos ident

-- | Parses an application expression in non infix notation.
parseExprApp :: Parser AST.Expr
parseExprApp = do
  -- Essentially, an expression application is a list of atom expressions.
  -- Each expression must be indented
  (x : xs) <- indentedWhiteSpaceConsumingSome parseParenExpr
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
          AST.ExprApp new_pos t t'

-- | Parses an integer literal
parseIntLit :: Parser AST.Expr
parseIntLit = do
  _ <- indented
  (pos, n) <- parsePositioned parseRawInteger
  return $ AST.ExprIntLit pos n

-- | Parses a string literal
parseStringLit :: Parser AST.Expr
parseStringLit = do
  _ <- indented
  (pos, str) <- parsePositioned parseRawString
  return $ AST.ExprStrLit pos str

-- | Parses an atomic (parenthesized) expression
parseParenExpr :: Parser AST.Expr
parseParenExpr =
  P.try f
    <|> P.try parseExprVar
    <|> P.try parseIntLit
    <|> parseStringLit
  where
    f = do
      _ <- indented
      (pos, t) <- parsePositioned $ betweenParentheses (P.try parseNestedInfixExpr <|> parseExprApp)
      return $ AST.setPosition pos t

-- | Parses an infix 'ASTExpr' that is part of a larger expression, i.e.,
-- is enclosed in parentheses.
parseNestedInfixExpr :: Parser AST.Expr
parseNestedInfixExpr = do
  (pos, (first_app, rhs)) <- parsePositioned $ do
    -- This thing you see write here (this do-block) consumes the infix expr
    -- e op e' and return (op e, e'). The reason for this is so that wrapping
    -- this do-block in parsePositioned gives us the annotation for the entire
    -- expr (op e) e' for free without having to recalculate all that nonsense.

    -- every step of the way, we check that the expression is not at the
    -- top-level. intermediate parses are wrapped with 'l'.
    _ <- indented
    lhs <- l parseParenExpr
    _ <- indented
    op <- l parseInfixTermIdentifier
    _ <- indented
    rhs <- parseParenExpr
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
              op_expr = AST.ExprTermVar (AST.getPosition op) op
          in  -- now apply op_expr to the lhs
              AST.ExprApp pos op_expr lhs
    return (first_app, rhs)
  -- just apply first_app to rhs.
  return $ AST.ExprApp pos first_app rhs
