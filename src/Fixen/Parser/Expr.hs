{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser.Expr
--     Description : Parsers for Fixen expressions (atoms, applications, infix, literals)
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for Fixen expressions. Expressions in Fixen follow a simple grammar
--     where compound operands in infix expressions must be parenthesized:
--
--     @
--     expr ::= \<atom_expr\> \<infix_op\> \<atom_expr\>   -- Infix operations
--           |  \<atom_expr\>+                          -- Function applications
--
--     atom_expr ::= '(' \<expr\> ')'
--                |  \<ident\>
--                |  \<int_literal\>
--                |  \<str_literal\>
--     @
--
--     "Atom expressions" (@\<atom_expr\>@) are factored out from the main
--     @\<expr\>@ production because they are simpler to parse. This design
--     avoids the need for precedence parsing — infix expressions with compound
--     operands must be parenthesized (e.g., @(a + b) + c@ instead of @a + b + c@).
--
--     The module provides parsers for all expression forms:
--
--     * 'parseExpr' — top-level entry point
--     * 'parseInfixExpr' — infix operations (with turnstile restriction)
--     * 'parseNestedInfixExpr' — infix operations inside parentheses (turnstile allowed)
--     * 'parseExprApp' — function applications (@f x y@)
--     * 'parseExprVar' — identifiers (@foo@, @Data.List.map@)
--     * 'parseIntLit' — integer literals (@42@)
--     * 'parseStringLit' — string literals (@@"hello"@@)
--     * 'parseUnitLit' — unit (@()@)
--     * 'parseTupleLit' — tuples (@(a, b, c)@)
--     * 'parseListLit' — lists (@[a, b, c]@)
--     * 'parseParenExpr' — atomic expressions including parenthesized sub-expressions
--     * 'parseAnyExpr' — any expression at the atom level
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
import Control.Monad (foldM, when)
import Data.List.NonEmpty
import Error.Diagnose.Position qualified as DPos
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Pos qualified as MPos

-- | Parses a Fixen expression at the top level. This is the primary entry point
-- for the expression parser.
--
-- The parser tries two alternatives in order:
--
-- 1. 'parseInfixExpr' — attempts to parse an infix expression of the form
--    @lhs \<op\> rhs@, where both sides are parenthesized or atomic expressions.
--    At the top level, the turnstile operator @(|-)@ is forbidden unless the
--    expression is enclosed in parentheses (enforced by 'parseInfixExpr').
--
-- 2. 'parseExprApp' — if no infix operator is found, falls back to parsing
--    a left-associative chain of atom expressions as a function application,
--    i.e. @f x1 x2 ... xn@.
--
-- The @indentCheck@ argument is a parser that verifies correct indentation
-- before each token. It is threaded through both branches to enforce that
-- every syntactic element is properly indented relative to its context.
--
-- This function corresponds to the top-level @\<expr\>@ production in the
-- Fixen expression grammar:
--
-- @
-- expr ::= \<infix_expr\>          -- e.g. @a + b@, @x \`elem\` ys@
--        |  \<app_expr\>           -- e.g. @f x y@, @cons True False@
-- @
parseExpr :: Parser MPos.Pos -> Parser AST.Expr
parseExpr indentCheck = P.try (parseInfixExpr indentCheck) <|> parseExprApp indentCheck

-- | Parses a top-level infix expression of the form @lhs \<op\> rhs@.
--
-- This parser handles infix operations where both operands are either
-- parenthesized expressions or atomic expressions (literals, variables,
-- tuples, lists). Compound operands must be parenthesized, which is why
-- this function delegates operand parsing to 'parseParenExpr'.
--
-- The parser enforces the following constraints:
--
-- 1. **Indentation**: Every token (left operand, operator, right operand)
--    must be properly indented. The @indentCheck@ argument verifies this
--    before each token. Left and right operands are wrapped with 'l' to
--    enforce indentation at the current nesting level, while the right
--    operand is parsed without 'l' since it appears at the same nesting
--    level as the operator.
--
-- 2. **No top-level turnstile**: The turnstile operator @(|-)@ is reserved
--    for rule definitions and is forbidden in top-level expressions. It
--    is only permitted when enclosed in parentheses (handled by
--    'parseParenExpr' → 'parseNestedInfixExpr').
--
-- === Infix-to-application transformation
--
-- Fixen represents infix expressions as nested function applications in
-- the AST. An expression @lhs \<op\> rhs@ is transformed into:
--
-- @
-- ExprApp second_app_id (ExprApp first_app_id (ExprVar op_id op) lhs) rhs
-- @
--
-- This represents the term @((\<op\> lhs) rhs)@, i.e., the operator @\<op\>@
-- is first applied to @lhs@, then the result is applied to @rhs@. For
-- example, @3 + 5@ becomes @((+) 3) 5@ in the AST.
--
-- The position annotations are computed as follows:
--
-- - @op_expr@ (the operator as a variable expression): inherits the
--   position of the original @op@ identifier from when it was parsed.
-- - @first_app@ (@(\<op\> lhs)@): spans from the beginning of @lhs@ to
--   the end of @\<op\>@. This captures the partial application.
-- - @second_app@ (@((\<op\> lhs) rhs)@): spans the entire expression,
--   set automatically by 'parsePositioned'.
--
-- The use of 'parsePositioned' on the outer do-block means the final
-- 'ExprApp' receives the position of the complete @lhs \<op\> rhs@
-- expression, avoiding the need to manually compute the end position
-- of @rhs@.
--
-- === Why two 'ExprApp' nodes?
--
-- The two-level application structure mirrors how infix operators work
-- in the lambda calculus: @lhs \<op\> rhs@ is syntactic sugar for
-- @((\<op\> lhs) rhs)@. The first 'ExprApp' creates the partial
-- application @(\<op\> lhs)@, and the second applies that to @rhs@.
--
-- Note that compound infix expressions like @a + b + c@ are invalid at the
-- top level because both operands must be atomic (variables, literals) or
-- parenthesized. The expression @a + b + c@ would fail because @a + b@ is
-- neither atomic nor parenthesized. To nest infix expressions, parentheses
-- are required: @((a + b) + c)@ parses as two separate infix expressions,
-- and @a + (b + c)@ similarly.
parseInfixExpr :: Parser MPos.Pos -> Parser AST.Expr
parseInfixExpr indentCheck =
  parsePositioned $ do
    -- Step 1: Parse the left operand. Wrapped with 'l' to enforce indentation
    -- at the current nesting level. The operand must be a parenthesized or
    -- atomic expression (delegated to parseParenExpr).
    _ <- indentCheck
    lhs <- l (parseParenExpr indentCheck)
    -- Step 2: Parse the infix operator. Also wrapped with 'l' for indentation.
    -- The operator is parsed as an Identifier (e.g., '+', '++', `elem`).
    _ <- indentCheck
    op <- l $ parseInfixTermIdentifier indentCheck
    -- Step 3: Reject the turnstile operator at the top level. The turnstile
    -- (|-) is reserved for rule definitions and is only allowed inside
    -- parentheses (handled by parseNestedInfixExpr).
    when (simpleIdentifier op == "|-") $ fail "turnstile (|-) cannot appear in expression without being enclosed in parentheses"
    -- Step 4: Parse the right operand. Not wrapped with 'l' because it sits
    -- at the same indentation level as the operator.
    _ <- indentCheck
    rhs <- parseParenExpr indentCheck
    -- Step 5: Build the AST representation. Infix expressions are encoded as
    -- nested function applications: lhs op rhs → ((op lhs) rhs).
    --
    -- 5a: Create the operator as a variable expression (ExprVar). We generate
    --     a fresh NodeId and inherit the operator's position from when it was
    --     originally parsed.
    op_id <- fixenGetNewNodeId
    op_pos <- fixenGetPosition op
    let op_expr = ExprVar op_id op
    fixenSetPosition op_expr op_pos
    -- 5b: Compute the position span for the partial application (op lhs).
    --     This spans from the start of lhs to the end of op, capturing the
    --     first-level application before rhs is applied.
    start_pos <- DPos.begin <$> fixenGetPosition lhs
    end_pos <- DPos.end <$> fixenGetPosition op
    file_name <- DPos.file <$> fixenGetPosition lhs
    let first_app_pos =
          DPos.Position
            { DPos.begin = start_pos
            , DPos.end = end_pos
            , DPos.file = file_name
            }
    -- 5c: Create the partial application node: (op lhs).
    first_app_id <- fixenGetNewNodeId
    let first_app = ExprApp first_app_id op_expr lhs
    fixenSetPosition first_app first_app_pos
    -- 5d: Create the final application node: ((op lhs) rhs). The position for
    --     this node is set automatically by parsePositioned (which wraps the
    --     entire do-block), capturing the full span of lhs op rhs.
    second_app_id <- fixenGetNewNodeId
    return $ ExprApp second_app_id first_app rhs

-- | Parses an expression variable (identifier) in non-infix notation.
--
-- This parser handles regular (non-backtick-enclosed) identifiers such as
-- @myFunction@, @Data.List.map@, or @Just@. These are parsed by
-- 'parseNonInfixTermIdentifier', which supports:
--
-- - Lowercase-starting identifiers (variables, functions): @foo@, @bar@
-- - Capitalized-starting identifiers (constructors, types): @Just@, @Data.Maybe.Maybe@
-- - Parenthesized operators: @(++)@, @(Data.List.++)@
--
-- The parsed identifier is wrapped in an 'ExprVar' AST node. A fresh 'NodeId'
-- is generated for the node, and the position is captured automatically by
-- 'parsePositioned'.
--
-- Indentation is enforced: the identifier must appear at the correct indentation
-- level relative to its context.
parseExprVar :: Parser MPos.Pos -> Parser AST.Expr
parseExprVar indentCheck = parsePositioned $ do
  -- Verify correct indentation before parsing the identifier.
  _ <- indentCheck
  -- Parse a non-infix identifier (e.g., @foo@, @Data.List.map@, @Just@).
  -- This delegates to parseNonInfixTermIdentifier which handles all the
  -- various identifier forms (lowercase, capitalized, parenthesized operators).
  ident <- parseNonInfixTermIdentifier indentCheck
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap the identifier in an ExprVar AST node. The position is set by
  -- parsePositioned (the outer wrapper), which captures the span from the
  -- start of the identifier to its end.
  return $ ExprVar i ident

-- | Parses a left-associative chain of atom expressions as a function
-- application, i.e. @f x1 x2 ... xn@.
--
-- This parser handles function applications where each argument is either
-- an atomic expression (variable, literal, tuple, list) or a parenthesized
-- expression. For example:
--
-- @
-- f x              -- single application
-- f x y z          -- multiple applications (left-associative)
-- f (a + b)        -- application with parenthesized argument
-- cons True False  -- constructor application
-- @
--
-- The parser works by:
--
-- 1. Parsing a non-empty sequence of atom expressions using 'someI'
--    (indented some), ensuring each argument is properly indented.
-- 2. Folding the sequence left-to-right using 'foldM', building nested
--    'ExprApp' nodes. For @f x y z@, this produces:
--    @((f x) y) z@ represented as
--    @ExprApp id3 (ExprApp id2 (ExprApp id1 f x) y) z@.
--
-- === Position annotations
--
-- Each 'ExprApp' node spans from the beginning of its left operand to the
-- end of its right operand. This means:
--
-- - @f x@ spans from the start of @f@ to the end of @x@
-- - @(f x) y@ spans from the start of @f@ to the end of @y@
-- - The final node spans the entire application chain
--
-- The 'NodeId' for each application node is freshly generated, and the
-- position is set via 'fixenSetPosition'. The outermost position is also
-- captured by 'parsePositioned' (the wrapper around the entire do-block).
parseExprApp :: Parser MPos.Pos -> Parser AST.Expr
parseExprApp indent_check = parsePositioned $ do
  -- Parse a non-empty left-associative chain of atom expressions.
  -- someI ensures each expression is properly indented.
  -- The result is a NonEmpty list: the head is the function, the tail are arguments.
  (x :| xs) <- someI indent_check (parseParenExpr indent_check)
  -- Fold left-to-right, building nested ExprApp nodes.
  -- For [f, x, y, z], this produces ((f x) y) z.
  foldM folder x xs
  where
    -- | Combines two expressions into a function application: @t t'@.
    -- Generates a fresh NodeId and computes the position span from the
    -- start of @t@ to the end of @t'@.
    folder :: AST.Expr -> AST.Expr -> Parser AST.Expr
    folder t t' = do
      -- Generate a fresh NodeId for this application node.
      new_id <- fixenGetNewNodeId
      -- Retrieve the positions of both operands from the PositionEnv.
      t_pos <- fixenGetPosition t
      t'_pos <- fixenGetPosition t'
      -- Compute the position span: from the beginning of the left operand
      -- to the end of the right operand, preserving the file name.
      let new_pos =
            DPos.Position
              { DPos.begin = DPos.begin t_pos
              , DPos.end = DPos.end t'_pos
              , DPos.file = DPos.file t_pos
              }
      -- Create the application node: t applied to t'.
      let app = ExprApp new_id t t'
      -- Set the position for this node in the PositionEnv.
      fixenSetPosition app new_pos
      return app

-- | Parses an integer literal expression, such as @42@ or @0@.
--
-- This parser handles non-negative integer literals. The parsed integer is
-- wrapped in an 'ExprIntLit' AST node with a freshly generated 'NodeId'.
-- Indentation is enforced: the literal must appear at the correct indentation
-- level relative to its context.
--
-- The position is captured automatically by 'parsePositioned', spanning from
-- the start to the end of the integer digits.
parseIntLit :: Parser MPos.Pos -> Parser AST.Expr
parseIntLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the integer.
  _ <- indent_check
  -- Parse the raw integer digits (delegates to parseRawInteger).
  n <- parseRawInteger
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap the integer in an ExprIntLit AST node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ ExprIntLit i n

-- | Parses a string literal expression, such as @"hello"@ or @""@.
--
-- This parser handles double-quoted string literals, including empty strings.
-- The parsed string is wrapped in an 'ExprStrLit' AST node with a freshly
-- generated 'NodeId'. Indentation is enforced: the string must appear at the
-- correct indentation level relative to its context.
--
-- The position is captured automatically by 'parsePositioned', spanning from
-- the opening quote to the closing quote.
parseStringLit :: Parser MPos.Pos -> Parser AST.Expr
parseStringLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the string.
  _ <- indent_check
  -- Parse the raw string content (delegates to parseRawString).
  str <- parseRawString
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap the string in an ExprStrLit AST node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ ExprStrLit i str

-- | Parses a unit literal, i.e., @()@.
--
-- This parser handles the unit expression, which is the Fixen equivalent of
-- Haskell's @()@. It produces an 'ExprUnit' AST node with a freshly generated
-- 'NodeId'. Indentation is enforced: the opening parenthesis must appear at
-- the correct indentation level.
--
-- The position is captured automatically by 'parsePositioned', spanning from
-- the opening parenthesis to the closing parenthesis.
parseUnitLit :: Parser MPos.Pos -> Parser AST.Expr
parseUnitLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the unit literal.
  _ <- indent_check
  -- Consume the parentheses. The content is empty (return ()) since unit has
  -- no payload, but the parentheses themselves define the syntax.
  betweenParentheses indent_check (return ())
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap in an ExprUnit AST node. The position is set by parsePositioned
  -- (the outer wrapper), which spans the entire @()@.
  return $ ExprUnit i

-- | Parses a tuple literal, such as @(e1, e2, e3)@. Note that @(e)@ is not a
-- tuple literal, but just @e@ enclosed in parentheses.
--
-- This parser handles tuples with two or more elements. A tuple is a
-- comma-separated list of expressions enclosed in parentheses. The minimum
-- size is 2 (enforced by 'commaSepBy2'), so single-element groups like @(e)@
-- are not parsed here—they fall through to other alternatives in 'parseParenExpr'.
--
-- Examples:
--
-- @
-- (1, 2)           -- 2-tuple (pair)
-- (True, "hi", 42) -- 3-tuple
-- @
--
-- Each element is parsed by 'parseAnyExpr', which handles atoms, applications,
-- and nested infix expressions. Indentation is enforced: the opening parenthesis
-- must appear at the correct indentation level.
--
-- The position is captured automatically by 'parsePositioned', spanning from
-- the opening parenthesis to the closing parenthesis.
parseTupleLit :: Parser MPos.Pos -> Parser AST.Expr
parseTupleLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the tuple.
  _ <- indent_check
  -- Parse a comma-separated list of at least 2 expressions inside parentheses.
  -- commaSepBy2 ensures minimum size of 2. The result is split into head (e)
  -- and tail (exprs) for the ExprTuple constructor.
  (e, exprs) <-
    betweenParentheses
      indent_check
      (commaSepBy2 indent_check (parseAnyExpr indent_check))
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap the tuple in an ExprTuple AST node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ ExprTuple i e exprs

-- | Parses a list literal, such as @[e1, e2, e3]@. Note that @[]@ is the empty
-- list literal.
--
-- This parser handles square-bracket-enclosed comma-separated lists of
-- expressions. Empty lists (@[]@) and lists with any number of elements are
-- supported. Each element is parsed by 'parseAnyExpr', which handles atoms,
-- applications, and nested infix expressions.
--
-- Indentation is enforced: the opening bracket must appear at the correct
-- indentation level.
--
-- The position is captured automatically by 'parsePositioned', spanning from
-- the opening bracket to the closing bracket.
parseListLit :: Parser MPos.Pos -> Parser AST.Expr
parseListLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the list.
  _ <- indent_check
  -- Parse a comma-separated list of expressions inside square brackets.
  -- commaSepBy accepts zero or more elements, so empty lists are valid.
  exprs <-
    betweenSquareBrackets
      indent_check
      (commaSepBy indent_check (parseAnyExpr indent_check))
  -- Generate a fresh NodeId for this expression node.
  i <- fixenGetNewNodeId
  -- Wrap the list in an ExprList AST node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ ExprList i exprs

-- | Parses any expression at the atom level: either a nested infix expression
-- or a function application.
--
-- This is the top-level entry point used by 'parseParenExpr' (for operands
-- of infix expressions) and by 'parseNestedInfixExpr' (for the right operand).
-- It tries infix expressions first (via 'P.try'), falling back to function
-- applications if no infix operator is found.
--
-- This function does NOT handle atomic literals directly — those are parsed
-- by 'parseParenExpr', which wraps this function.
parseAnyExpr :: Parser MPos.Pos -> Parser AST.Expr
parseAnyExpr indent_check =
  P.try (parseNestedInfixExpr indent_check)
    <|> parseExprApp indent_check

-- | Parses an atomic expression — the smallest indivisible expression units.
--
-- This is the central fallback parser used throughout the expression grammar.
-- Every expression type ultimately resolves to one of these alternatives.
-- The parser tries each option in order, using 'P.try' to backtrack on failure
-- (except for the last alternative, which is guaranteed to succeed or fail
-- the entire parse).
--
-- The alternatives, in priority order, are:
--
-- 1. **Parenthesized expressions** (@(e)@) — handled by the local helper @f@.
--    This captures expressions wrapped in parentheses, which can contain infix
--    operations (via 'parseNestedInfixExpr') or function applications
--    (via 'parseExprApp'). This is tried first because parentheses have the
--    highest precedence.
--
-- 2. **Unit literals** (@()@) — handled by 'parseUnitLit'.
--
-- 3. **Tuple literals** (@(e1, e2, ...@)@) — handled by 'parseTupleLit'.
--    Note that single-element groups like @(e)@ are not tuples; they fall
--    through to alternative 1.
--
-- 4. **List literals** (@[e1, e2, ...]@) — handled by 'parseListLit'.
--
-- 5. **Expression variables** (@foo@, @Data.List.map@) — handled by
--    'parseExprVar'.
--
-- 6. **Integer literals** (@42@, @0@) — handled by 'parseIntLit'.
--
-- 7. **String literals** (@@"hello"@@, @""@) — handled by 'parseStringLit'.
--
-- === The parenthesized expression helper (@f@)
--
-- The local helper @f@ handles expressions enclosed in parentheses that are
-- not unit literals, tuples, or lists. It verifies indentation with 'indented'
-- (which requires the opening parenthesis to be at the correct level), then
-- delegates to 'parseNestedInfixExpr' (for infix operations like @(a + b)@)
-- or 'parseExprApp' (for applications like @(f x)@).
--
-- The 'parsePositioned' wrapper ensures the position spans the entire
-- @(...)@, including the parentheses.
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
    -- | Handles parenthesized expressions that are not unit/tuple/list literals.
    -- Verifies indentation, then delegates to parseNestedInfixExpr or parseExprApp.
    f = do
      -- Verify that the opening parenthesis is at the correct indentation level.
      _ <- indented
      -- Parse the content inside parentheses and capture the position of the
      -- entire (...) span.
      parsePositioned $ betweenParentheses indent_check item
    -- | The content inside parentheses: either an infix expression or a function
    -- application. Infix is tried first because it has higher precedence.
    item = P.try (parseNestedInfixExpr indent_check) <|> parseExprApp indent_check

-- | Parses an infix expression that appears inside parentheses. Unlike
-- 'parseInfixExpr', this parser allows the turnstile operator @(|-)@ since
-- it is only invoked for parenthesized sub-expressions (via 'parseParenExpr').
--
-- This function is the recursive heart of infix parsing. When 'parseParenExpr'
-- encounters a left parenthesis, it tries 'parseNestedInfixExpr' first. If the
-- content starts with an atomic expression followed by an infix operator and
-- another expression, it parses the infix operation. Otherwise, it falls back
-- to 'parseExprApp' for function applications.
--
-- This design enables nested infix expressions with parentheses:
--
-- @
-- (a + b) + c      -- outer infix, left operand is parenthesized infix
-- a + (b + c)      -- outer infix, right operand is parenthesized infix
-- (a |- b)         -- turnstile inside parentheses (used in rules)
-- @
--
-- See 'parseInfixExpr' for details on the AST representation and position
-- computation.
parseNestedInfixExpr :: Parser MPos.Pos -> Parser AST.Expr
parseNestedInfixExpr indent_check = parsePositioned $ do
  -- Step 1: Parse the left operand. Wrapped with 'l' to enforce indentation
  -- at the current nesting level. The operand must be a parenthesized or
  -- atomic expression (delegated to parseParenExpr).
  _ <- indent_check
  lhs <- l (parseParenExpr indent_check)
  -- Step 2: Parse the infix operator. Also wrapped with 'l' for indentation.
  -- Unlike parseInfixExpr, the turnstile (|-) is allowed here since we are
  -- inside parentheses.
  _ <- indent_check
  op <- l $ parseInfixTermIdentifier indent_check
  -- Step 3: Parse the right operand. Not wrapped with 'l' because it sits
  -- at the same indentation level as the operator.
  _ <- indent_check
  rhs <- parseParenExpr indent_check
  -- Step 4: Build the AST representation. Infix expressions are encoded as
  -- nested function applications: lhs op rhs → ((op lhs) rhs).
  --
  -- 4a: Create the operator as a variable expression (ExprVar). We generate
  --     a fresh NodeId and inherit the operator's position from when it was
  --     originally parsed.
  op_id <- fixenGetNewNodeId
  op_pos <- fixenGetPosition op
  let op_expr = ExprVar op_id op
  fixenSetPosition op_expr op_pos
  -- 4b: Compute the position span for the partial application (op lhs).
  --     This spans from the start of lhs to the end of op, capturing the
  --     first-level application before rhs is applied.
  start_pos <- DPos.begin <$> fixenGetPosition lhs
  end_pos <- DPos.end <$> fixenGetPosition op
  file_name <- DPos.file <$> fixenGetPosition lhs
  let first_app_pos =
        DPos.Position
          { DPos.begin = start_pos
          , DPos.end = end_pos
          , DPos.file = file_name
          }
  -- 4c: Create the partial application node: (op lhs).
  first_app_id <- fixenGetNewNodeId
  let first_app = ExprApp first_app_id op_expr lhs
  fixenSetPosition first_app first_app_pos
  -- 4d: Create the final application node: ((op lhs) rhs). The position for
  --     this node is set automatically by parsePositioned (which wraps the
  --     entire do-block), capturing the full span of lhs op rhs.
  second_app_id <- fixenGetNewNodeId
  return $ ExprApp second_app_id first_app rhs
