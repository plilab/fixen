{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.Parser.Type
-- Description : Parsers for Fixen (Haskell) types
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- Parsers for Fixen types. Types in Fixen follow a simple grammar
-- where compound operands in infix type applications must be parenthesized:
--
-- @
-- type ::= \<atom_type\>+                          -- Type applications
--
-- atom_type ::= '(' \<type\> ')'
--            |  \<ident\>
--            |  \<nat_literal\>
--            |  \<symbol_literal\>
--            |  '()'
--            |  '(' \<type\> ',' \<type\> (',' \<type\>)* ')'
--            |  '[' \<type\> (',' \<type\>)* ']'
-- @
--
-- "Atom types" (@\<atom_type\>@) are factored out from the main
-- @\<type\>@ production because they are simpler to parse. This design
-- avoids the need for precedence parsing — infix types with compound
-- operands must be parenthesized (e.g., @(T1 -> T2) -> T3@ instead of
-- @T1 -> T2 -> T3@).
--
-- The module provides parsers for all type forms:
--
-- * 'parseType' — top-level entry point
-- * 'parseInfixType' — infix type applications (colon @(:)@ restricted)
-- * 'parseNestedInfixType' — infix types inside parentheses
-- * 'parseTypeApp' — type applications (@T1 T2 T3@)
-- * 'parseTypeVar' — type variables (capitalized identifiers)
-- * 'parseTypeNatLit' — natural number literals (@42@)
-- * 'parseTypeSymbolLit' — symbol literals (@@"hello"@@)
-- * 'parseTypeUnit' — unit type (@()@)
-- * 'parseTupleType' — tuple types (@(T1, T2)@)
-- * 'parseListType' — list types (@[T]@)
-- * 'parseParenType' — atomic types including parenthesized sub-types
-- * 'parseAnyType' — any type at the atom level
--
-- @since 26.7
module Fixen.Parser.Type where

import Control.Applicative.Combinators (
  (<|>),
 )
import Control.Monad
import Data.List.NonEmpty
import Error.Diagnose.Position qualified as DPos
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Token
import Text.Megaparsec qualified as P
import Text.Megaparsec.Pos qualified as MPos

--------------------------------------------------------------------------------

-- * Top-level Types

--------------------------------------------------------------------------------

-- | Parse a 'Type' at the top level.
--
--   This is the primary entry point for the type parser. It tries two
--   alternatives in order:
--
--   1. 'parseInfixType' — attempts to parse an infix type application of
--      the form @lhs \<op\> rhs@, where both sides are parenthesized or
--      atomic types. At the top level, the colon operator @(:)@ is
--      forbidden unless the type is enclosed in parentheses.
--
--   2. 'parseTypeApp' — if no infix operator is found, falls back to
--      parsing a left-associative chain of atom types as a type
--      application, i.e. @T1 T2 T3@.
--
--   The @indentCheck@ argument is a parser that verifies correct indentation
--   before each token. It is threaded through both branches to enforce that
--   every syntactic element is properly indented relative to its context.
--
-- @since 26.7
parseType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseType indentCheck = P.try (parseInfixType indentCheck) <|> parseTypeApp indentCheck

--------------------------------------------------------------------------------

-- * Atomic Types

--------------------------------------------------------------------------------

-- | Parse an atomic (parenthesized) type — the smallest indivisible type units.
--
--   This is the central fallback parser used throughout the type grammar.
--   Every type ultimately resolves to one of these alternatives. The parser
--   tries each option in order, using 'P.try' to backtrack on failure
--   (except for the last alternative, which is guaranteed to succeed or fail
--   the entire parse).
--
--   The alternatives, in priority order, are:
--
--   1. **Parenthesized types** (@(T)@) — handled by the local helper @f@.
--      This captures types wrapped in parentheses, which can contain infix
--      type applications (via 'parseNestedInfixType') or type applications
--      (via 'parseTypeApp'). This is tried first because parentheses have the
--      highest precedence.
--
--   2. **Unit types** (@()@) — handled by 'parseTypeUnit'.
--
--   3. **Tuple types** (@(T1, T2, ...)@) — handled by 'parseTupleType'.
--      Note that single-element groups like @(T)@ are not tuples; they fall
--      through to alternative 1.
--
--   4. **List types** (@[T1, T2, ...]@) — handled by 'parseListType'.
--
--   5. **Type variables** (@Int@, @Data.Map.Map@) — handled by
--      'parseTypeVar'.
--
--   6. **Natural number literals** (@42@, @0@) — handled by 'parseTypeNatLit'.
--
--   7. **Symbol literals** (@@"hello"@@, @""@) — handled by 'parseTypeSymbolLit'.
--
--   === The parenthesized type helper (@f@)
--
--   The local helper @f@ handles types enclosed in parentheses that are not
--   unit types, tuple types, or list types. It verifies indentation with
--   'indented' (which requires the opening parenthesis to be at the correct
--   level), then delegates to 'parseNestedInfixType' (for infix types like
--   @(T1 -> T2)@) or 'parseTypeApp' (for applications like @(T a)@).
--
--   The 'parsePositioned' wrapper ensures the position spans the entire
--   @(...)@, including the parentheses.
--
-- @since 26.7
parseParenType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseParenType indent_check =
  P.try f
    <|> P.try (parseTypeUnit indent_check)
    <|> P.try (parseTupleType indent_check)
    <|> P.try (parseListType indent_check)
    <|> P.try (parseTypeVar indent_check)
    <|> P.try (parseTypeNatLit indent_check)
    <|> parseTypeSymbolLit indent_check
  where
    -- \| Handle parenthesized types that are not unit/tuple/list types.
    -- Verifies indentation, then delegates to parseNestedInfixType or parseTypeApp.
    f = do
      -- Verify that the opening parenthesis is at the correct indentation level.
      _ <- indented
      -- Parse the content inside parentheses and capture the position of the
      -- entire (...) span.
      parsePositioned $ betweenParentheses indent_check item
    -- \| The content inside parentheses: either an infix type or a type
    -- application. Infix is tried first because it has higher precedence.
    item = P.try (parseNestedInfixType indent_check) <|> parseTypeApp indent_check

--------------------------------------------------------------------------------

-- * Other 'Type' Parsers

--------------------------------------------------------------------------------

-- | Parse an infix 'Type' at the top level.
--
--   This parser handles infix type applications where both operands are
--   either parenthesized types or atomic types (variables, literals,
--   tuples, lists). Compound operands must be parenthesized, which is why
--   this function delegates operand parsing to 'parseParenType'.
--
--   The parser enforces the following constraints:
--
--   1. **Indentation**: Every token (left operand, operator, right operand)
--      must be properly indented. The @indentCheck@ argument verifies this
--      before each token. Left and right operands are wrapped with 'l' to
--      enforce indentation at the current nesting level, while the right
--      operand is parsed without 'l' since it appears at the same nesting
--      level as the operator.
--
--   2. **No top-level colon**: The colon operator @(:)@ is reserved for
--      type declarations and is forbidden in top-level type expressions.
--      It is only permitted when enclosed in parentheses (handled by
--      'parseParenType' → 'parseNestedInfixType').
--
--   === Infix-to-application transformation
--
--   Fixen represents infix types as nested type applications in the
--   A type @lhs \<op\> rhs@ is transformed into:
--
--   @
--   TypeApp second_app_id (TypeApp first_app_id (TypeName op_id op) lhs) rhs
--   @
--
--   This represents the term @((\<op\> lhs) rhs)@, i.e., the operator @\<op\>@
--   is first applied to @lhs@, then the result is applied to @rhs@. For
--   example, @T1 -> T2@ becomes @((->) T1) T2@ in the
--
--   The position annotations are computed as follows:
--
--   - @op_expr@ (the operator as a 'TypeName'): inherits the position of
--     the original @op@ identifier from when it was parsed.
--   - @first_app@ (@(\<op\> lhs)@): spans from the beginning of @lhs@ to
--     the end of @\<op\>@, capturing the partial application.
--   - @second_app@ (@((\<op\> lhs) rhs)@): spans the entire expression,
--     set automatically by 'parsePositioned'.
--
--   The use of 'parsePositioned' on the outer do-block means the final
--   'TypeApp' receives the position of the complete @lhs \<op\> rhs@
--   expression, avoiding the need to manually compute the end position
--   of @rhs@.
--
--   === Why two 'TypeApp' nodes?
--
--   The two-level application structure mirrors how infix operators work
--   in the lambda calculus: @lhs \<op\> rhs@ is syntactic sugar for
--   @((\<op\> lhs) rhs)@. The first 'TypeApp' creates the partial
--   application @(\<op\> lhs)@, and the second applies that to @rhs@.
--
--   Note that compound infix types like @T1 -> T2 -> T3@ are invalid at the
--   top level because both operands must be atomic (variables, literals) or
--   parenthesized. The expression @T1 -> T2 -> T3@ would fail because
--   @T1 -> T2@ is neither atomic nor parenthesized. To nest infix types,
--   parentheses are required: @((T1 -> T2) -> T3)@ parses as two separate
--   infix types, and @T1 -> (T2 -> T3)@ similarly.
--
-- @since 26.7
parseInfixType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseInfixType indentCheck =
  parsePositioned $ do
    -- Step 1: Parse the left operand. Wrapped with 'l' to enforce indentation
    -- at the current nesting level. The operand must be a parenthesized or
    -- atomic type (delegated to parseParenType).
    _ <- indentCheck
    lhs <- l (parseParenType indentCheck)
    -- Step 2: Parse the infix operator. Also wrapped with 'l' for indentation.
    -- The operator is parsed as an identifier (e.g., '->', '::', `elem`).
    _ <- indentCheck
    op <- l parseOpIdentifier
    -- Step 3: Reject the colon operator at the top level. The colon is
    -- reserved for type declarations and is only allowed inside
    -- parentheses (handled by parseNestedInfixType).
    when (simpleIdentifier op == ":") $
      fail "(:) cannot appear in type without being enclosed in parentheses"
    -- Step 4: Parse the right operand. Not wrapped with 'l' because it sits
    -- at the same indentation level as the operator.
    _ <- indentCheck
    rhs <- parseParenType indentCheck
    -- Step 5: Build the representation. Infix types are encoded as
    -- nested type applications: lhs op rhs → ((op lhs) rhs).
    --
    -- 5a: Create the operator as a TypeName. We generate a fresh NodeId
    --     and inherit the operator's position from when it was originally
    --     parsed.
    op_id <- getNewNodeId
    op_pos <- getPosition op
    let op_expr = TypeName op_id op
    setPosition op_expr op_pos
    -- 5b: Compute the position span for the partial application (op lhs).
    --     This spans from the start of lhs to the end of op, capturing the
    --     first-level application before rhs is applied.
    start_pos <- DPos.begin <$> getPosition lhs
    end_pos <- DPos.end <$> getPosition op
    file_name <- DPos.file <$> getPosition lhs
    let first_app_pos =
          DPos.Position
            { DPos.begin = start_pos
            , DPos.end = end_pos
            , DPos.file = file_name
            }
    -- 5c: Create the partial application node: (op lhs).
    first_app_id <- getNewNodeId
    let first_app = TypeApp first_app_id op_expr lhs
    setPosition first_app first_app_pos
    -- 5d: Create the final application node: ((op lhs) rhs). The position for
    --     this node is set automatically by parsePositioned (which wraps the
    --     entire do-block), capturing the full span of lhs op rhs.
    second_app_id <- getNewNodeId
    return $ TypeApp second_app_id first_app rhs

-- | Parse a type variable, which is a capitalized identifier.
--
--   This parser handles type variables and type constructors such as
--   @Int@, @Maybe@, or @Data.Map.Map@. It tries to parse a capitalized
--   identifier first ('parseCapitalizedIdentifier'), then falls back to
--   a non-infix operator identifier ('parseNonInfixOpIdentifier') for
--   parenthesized operators like @(->)@.
--
--   The parsed identifier is wrapped in a 'TypeName' node. A fresh
--   'NodeId' is generated for the node, and the position is captured
--   automatically by 'parsePositioned'.
--
--   Indentation is enforced: the type variable must appear at the correct
--   indentation level relative to its context.
--
-- @since 26.7
parseTypeVar :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTypeVar indentCheck = parsePositioned $ do
  -- Verify correct indentation before parsing the type variable.
  _ <- indentCheck
  -- Parse a capitalized identifier (e.g., @Int@, @Maybe@) or a non-infix
  -- operator identifier (e.g., @(->)@). The try combinator ensures that
  -- if parseCapitalizedIdentifier fails, we backtrack and try the fallback.
  ident <- P.try parseCapitalizedIdentifier <|> parseNonInfixOpIdentifier indentCheck
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap the identifier in a TypeName node. The position is set by
  -- parsePositioned (the outer wrapper), which captures the span from the
  -- start of the identifier to its end.
  return $ TypeName i ident

-- | Parse a left-associative chain of atom types as a type application,
-- i.e. @T1 T2 T3@.
--
--   This parser handles type applications where each argument is either
--   an atomic type (variable, literal, tuple, list) or a parenthesized
--   type. For example:
--
--   @
--   Maybe Int              -- single application
--   Data.Map.Map Text Int  -- multiple applications
--   IO (Maybe Int)         -- application with parenthesized argument
--   @
--
--   The parser works by:
--
--   1. Parsing a non-empty sequence of atom types using 'someI'
--      (indented some), ensuring each argument is properly indented.
--   2. Folding the sequence left-to-right using 'foldM', building nested
--      'TypeApp' nodes. For @T1 T2 T3@, this produces:
--      @((T1 T2) T3)@ represented as
--      @TypeApp id3 (TypeApp id2 (TypeApp id1 T1 T2) T3)@.
--
--   === Position annotations
--
--   Each 'TypeApp' node spans from the beginning of its left operand to the
--   end of its right operand. This means:
--
--   - @T1 T2@ spans from the start of @T1@ to the end of @T2@
--   - @(T1 T2) T3@ spans from the start of @T1@ to the end of @T3@
--   - The final node spans the entire application chain
--
--   The 'NodeId' for each application node is freshly generated, and the
--   position is set via 'setPosition'. The outermost position is also
--   captured by 'parsePositioned' (the wrapper around the entire do-block).
--
-- @since 26.7
parseTypeApp :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTypeApp indent_check = parsePositioned $ do
  -- Parse a non-empty left-associative chain of atom types.
  -- someI ensures each type is properly indented.
  -- The result is a NonEmpty list: the head is the type constructor,
  -- the tail are type arguments.
  (x :| xs) <- someI indent_check (parseParenType indent_check)
  -- Fold left-to-right, building nested TypeApp nodes.
  -- For [T1, T2, T3], this produces ((T1 T2) T3).
  foldM folder x xs
  where
    -- \| Combine two types into a type application: @t t'@.
    -- Generates a fresh NodeId and computes the position span from the
    -- start of @t@ to the end of @t'@.
    folder :: ParserState σ => Type -> Type -> Parser σ Type
    folder t t' = do
      -- Generate a fresh NodeId for this application node.
      new_id <- getNewNodeId
      -- Retrieve the positions of both operands from the PositionEnv.
      t_pos <- getPosition t
      t'_pos <- getPosition t'
      -- Compute the position span: from the beginning of the left operand
      -- to the end of the right operand, preserving the file name.
      let new_pos =
            DPos.Position
              { DPos.begin = DPos.begin t_pos
              , DPos.end = DPos.end t'_pos
              , DPos.file = DPos.file t_pos
              }
      -- Create the application node: t applied to t'.
      let app = TypeApp new_id t t'
      -- Set the position for this node in the PositionEnv.
      setPosition app new_pos
      return app

-- | Parse a natural number literal type, such as @42@ or @0@.
--
--   This parser handles non-negative integer literals used as type-level
--   natural number literals. The parsed natural is wrapped in a 'TypeNatLit'
--   node with a freshly generated 'NodeId'. Indentation is enforced:
--   the literal must appear at the correct indentation level relative to
--   its context.
--
--   The position is captured automatically by 'parsePositioned', spanning
--   from the start to the end of the number digits.
--
-- @since 26.7
parseTypeNatLit :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTypeNatLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the natural number.
  _ <- indent_check
  -- Parse the raw natural number digits (delegates to parseRawNatural).
  n <- parseRawNatural
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap the natural in a TypeNatLit node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ TypeNatLit i n

-- | Parse a symbol (string) literal type, such as @@"hello"@@ or @""@.
--
--   This parser handles double-quoted string literals used as type-level
--   symbol literals, including empty strings. The parsed string is wrapped
--   in a 'TypeSymbolLit' node with a freshly generated 'NodeId'.
--   Indentation is enforced: the string must appear at the correct
--   indentation level relative to its context.
--
--   The position is captured automatically by 'parsePositioned', spanning
--   from the opening quote to the closing quote.
--
-- @since 26.7
parseTypeSymbolLit :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTypeSymbolLit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the string.
  _ <- indent_check
  -- Parse the raw string content (delegates to parseRawString).
  str <- parseRawString
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap the string in a TypeSymbolLit node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ TypeSymbolLit i str

-- | Parse a unit type, i.e., @()@.
--
--   This parser handles the unit type, which is the Fixen equivalent of
--   Haskell's @()@. It produces a 'TypeUnit' node with a freshly
--   generated 'NodeId'. Indentation is enforced: the opening parenthesis
--   must appear at the correct indentation level.
--
--   The position is captured automatically by 'parsePositioned', spanning
--   from the opening parenthesis to the closing parenthesis.
--
-- @since 26.7
parseTypeUnit :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTypeUnit indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the unit type.
  _ <- indent_check
  -- Consume the parentheses. The content is empty (return ()) since unit has
  -- no payload, but the parentheses themselves define the syntax.
  betweenParentheses indent_check (return ())
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap in a TypeUnit node. The position is set by parsePositioned
  -- (the outer wrapper), which spans the entire @()@.
  return $ TypeUnit i

-- | Parse a tuple type, such as @(T1, T2, T3)@. Note that @(T)@ is not a
-- tuple type, but just @T@ enclosed in parentheses.
--
--   This parser handles tuples with two or more elements. A tuple type is a
--   comma-separated list of types enclosed in parentheses. The minimum
--   size is 2 (enforced by 'commaSepBy2'), so single-element groups like
--   @(T)@ are not parsed here — they fall through to other alternatives in
--   'parseParenType'.
--
--   Examples:
--
--   @
--   (Int, Bool)              -- 2-tuple (pair)
--   (Text, Int, Bool, a)     -- 4-tuple
--   @
--
--   Each element is parsed by 'parseAnyType', which handles atoms, type
--   applications, and nested infix types. Indentation is enforced: the
--   opening parenthesis must appear at the correct indentation level.
--
--   The position is captured automatically by 'parsePositioned', spanning
--   from the opening parenthesis to the closing parenthesis.
--
-- @since 26.7
parseTupleType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseTupleType indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the tuple type.
  _ <- indent_check
  -- Parse a comma-separated list of at least 2 types inside parentheses.
  -- commaSepBy2 ensures minimum size of 2. The result is split into head (e)
  -- and tail (exprs) for the TypeTuple constructor.
  (e, exprs) <-
    betweenParentheses
      indent_check
      (commaSepBy2 indent_check (parseAnyType indent_check))
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap the tuple in a TypeTuple node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ TypeTuple i e exprs

-- | Parse a list type, such as @[T1, T2, T3]@. Note that @[]@ is the empty
-- list type.
--
--   This parser handles square-bracket-enclosed comma-separated lists of
--   types. Empty lists (@[]@) and lists with any number of elements are
--   supported. Each element is parsed by 'parseAnyType', which handles
--   atoms, type applications, and nested infix types.
--
--   Indentation is enforced: the opening bracket must appear at the correct
--   indentation level.
--
--   The position is captured automatically by 'parsePositioned', spanning
--   from the opening bracket to the closing bracket.
--
-- @since 26.7
parseListType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseListType indent_check = parsePositioned $ do
  -- Verify correct indentation before parsing the list type.
  _ <- indent_check
  -- Parse a single type inside square brackets. The square brackets are
  -- the list syntax (e.g., @[T]@), so we just parse the element type.
  expr <-
    betweenSquareBrackets
      indent_check
      (parseAnyType indent_check)
  -- Generate a fresh NodeId for this type node.
  i <- getNewNodeId
  -- Wrap the list in a TypeList node. The position is set by
  -- parsePositioned (the outer wrapper).
  return $ TypeList i expr

-- | Parse any type at the atom level: either a nested infix type or a
-- type application.
--
--   This is the top-level entry point used by 'parseParenType' (for operands
--   of infix types) and by 'parseNestedInfixType' (for the right operand).
--   It tries infix types first (via 'P.try'), falling back to type
--   applications if no infix operator is found.
--
--   This function does NOT handle atomic types directly — those are parsed
--   by 'parseParenType', which wraps this function.
--
-- @since 26.7
parseAnyType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseAnyType indent_check =
  P.try (parseNestedInfixType indent_check)
    <|> parseTypeApp indent_check

-- | Parse an infix 'Type' that is part of a larger expression, i.e.,
-- is enclosed in parentheses.
--
--   Unlike 'parseInfixType', this parser allows the colon operator @(:)@
--   since it is only invoked for parenthesized sub-types (via 'parseParenType').
--
--   This function is the recursive heart of infix type parsing. When
--   'parseParenType' encounters a left parenthesis, it tries
--   'parseNestedInfixType' first. If the content starts with an atomic
--   type followed by an infix operator and another type, it parses the
--   infix type application. Otherwise, it falls back to 'parseTypeApp'
--   for type applications.
--
--   This design enables nested infix types with parentheses:
--
--   @
--   (T1 -> T2) -> T3      -- outer infix, left operand is parenthesized infix
--   T1 -> (T2 -> T3)      -- outer infix, right operand is parenthesized infix
--   (T1 : T2)             -- colon inside parentheses (used in type declarations)
--   @
--
--   See 'parseInfixType' for details on the representation and position
--   computation.
--
-- @since 26.7
parseNestedInfixType :: ParserState σ => Parser σ MPos.Pos -> Parser σ Type
parseNestedInfixType indent_check =
  parsePositioned $ do
    -- Step 1: Parse the left operand. Wrapped with 'l' to enforce indentation
    -- at the current nesting level. The operand must be a parenthesized or
    -- atomic type (delegated to parseParenType).
    _ <- indent_check
    lhs <- l (parseParenType indent_check)
    -- Step 2: Parse the infix operator. Also wrapped with 'l' for indentation.
    -- Unlike parseInfixType, the colon (:) is allowed here since we are
    -- inside parentheses.
    _ <- indent_check
    op <- l parseOpIdentifier
    -- Step 3: Parse the right operand. Not wrapped with 'l' because it sits
    -- at the same indentation level as the operator.
    _ <- indent_check
    rhs <- parseParenType indent_check
    -- Step 4: Build the representation. Infix types are encoded as
    -- nested type applications: lhs op rhs → ((op lhs) rhs).
    --
    -- 4a: Create the operator as a TypeName. We generate a fresh NodeId
    --     and inherit the operator's position from when it was originally
    --     parsed.
    op_id <- getNewNodeId
    op_pos <- getPosition op
    let op_expr = TypeName op_id op
    setPosition op_expr op_pos
    -- 4b: Compute the position span for the partial application (op lhs).
    --     This spans from the start of lhs to the end of op, capturing the
    --     first-level application before rhs is applied.
    start_pos <- DPos.begin <$> getPosition lhs
    end_pos <- DPos.end <$> getPosition op
    file_name <- DPos.file <$> getPosition lhs
    let first_app_pos =
          DPos.Position
            { DPos.begin = start_pos
            , DPos.end = end_pos
            , DPos.file = file_name
            }
    -- 4c: Create the partial application node: (op lhs).
    first_app_id <- getNewNodeId
    let first_app = TypeApp first_app_id op_expr lhs
    setPosition first_app first_app_pos
    -- 4d: Create the final application node: ((op lhs) rhs). The position for
    --     this node is set automatically by parsePositioned (which wraps the
    --     entire do-block), capturing the full span of lhs op rhs.
    second_app_id <- getNewNodeId
    return $ TypeApp second_app_id first_app rhs
