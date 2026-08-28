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
-- where infix type applications cannot be chained:
--
-- @
-- type ::= \<atom_type\>+ [\<type symbol\> \<atom_type\>+]
--
-- atom_type ::= '(' \<type\> ')'
--            |  \<ident\>
--            |  \<nat_literal\>
--            |  \<symbol_literal\>
--            |  '()'
--            |  '(' \<type\> ',' \<type\> (',' \<type\>)* ')'
--            |  '[' \<type\> ']'
-- @
--
-- "Atom types" (@\<atom_type\>@) are factored out from the main
-- @\<type\>@ production because they are simpler to parse.
--
-- @since 26.7
module Fixen.Parser.Type where

import Control.Applicative.Combinators (
  (<|>),
 )
import Control.Monad
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Error.Diagnose.Position qualified as DPos
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Error
import Fixen.Parser.Token
import Text.Megaparsec qualified as P

--------------------------------------------------------------------------------

-- * Top-level Types

--------------------------------------------------------------------------------

-- | Parse a 'Type' at the top level.
--
--   This is the primary entry point for the type parser.
--
--   The @indentCheck@ argument is a parser that verifies correct indentation
--   before each token. It is threaded through both branches to enforce that
--   every syntactic element is properly indented relative to its context.
--
-- @since 26.7
parseType :: ParserState σ => Parser σ P.Pos -> Parser σ Type
parseType indentCheck = inContext "sort/type" $ do
  offset_start <- P.getOffset
  -- parse the first type
  t <- parseAtomicType indentCheck
  -- now time to parse a bunch of types/infix ops separated by spaces.
  -- This can be empty. Lefts represent infix ops, Rights represent
  -- types. The two Ints in the tuple components represent start and
  -- end offsets.
  ls <-
    -- we commit to the parse of a type or infix op whenever
    -- we see valid starting characters. this prevents pointless
    -- backtracking to get a successful parse even though it will
    -- fail again when we encounter the same token but give a completely
    -- incomprehensible error message.
    manyICommitted
      ( \c ->
          isValidOpChar c
            || isUpper c
            || isLower c
            || c == '['
            || c == '('
            || isDigit c
      )
      indentCheck
      (parseTypeOrInfixOp indentCheck)
  case getSecondOp False (-1) ls of
    -- too many infix operators
    Just (after, (op_start, op_end, _)) ->
      customErrorWithOffset op_start $
        FixenCustomError
          (Just (op_end - op_start))
          []
          "cannot chain infix type applications"
          (Just (offset_start, after, "infix type application"))
          [ Note $
              "Fixen is not able to determine correct fixities. "
                ++ "Use appropriate parentheses."
          ]
    Nothing ->
      -- check if the last guy is an infix operator
      case getLastOp ls of
        Just (op_start, op_end, _) ->
          customErrorWithOffset op_end $
            FixenCustomError
              Nothing
              []
              "missing right operand in infix type application"
              (Just (op_start, op_end, "infix type"))
              []
        Nothing ->
          -- there is at most one infix guy. get it.
          case partitionByInfix ls of
            Just (lhs, op, rhs) -> do
              -- infix is t : lhs op rhs
              left_ty <- foldM buildAppType t lhs
              right_ty <- case rhs of
                [] -> error "unreachable"
                (y : ys) -> foldM buildAppType y ys
              -- Build the representation. Infix types are encoded as
              -- nested type applications: lhs op rhs → ((op lhs) rhs).
              --
              -- a: Create the operator as a TypeName. We generate a fresh NodeId
              --    and inherit the operator's position from when it was originally
              --    parsed.
              op_id <- getNewNodeId
              op_pos <- getPosition op
              let op_expr = TypeName op_id op
              setPosition op_expr op_pos
              -- b: Compute the position span for the partial application (op lhs).
              --    This spans from the start of lhs to the end of op, capturing the
              --    first-level application before rhs is applied.
              start_pos <- DPos.begin <$> getPosition left_ty
              end_pos <- DPos.end <$> getPosition op
              file_name <- DPos.file <$> getPosition left_ty
              let first_app_pos =
                    DPos.Position
                      { DPos.begin = start_pos
                      , DPos.end = end_pos
                      , DPos.file = file_name
                      }
              -- c: Create the partial application node: (op lhs).
              first_app_id <- getNewNodeId
              let first_app = TypeApp first_app_id op_expr left_ty
              setPosition first_app first_app_pos
              -- d: Create the final application node: ((op lhs) rhs). The position for
              --    this node is set automatically by parsePositioned (which wraps the
              --    entire do-block), capturing the full span of lhs op rhs.
              second_app_id <- getNewNodeId
              return $ TypeApp second_app_id first_app right_ty
            _ ->
              -- no infixes. fold the entire type into type applications
              foldM buildAppType t (forget ls)
  where
    -- parses a type or an infix op. the first argument is
    -- the indent check. the result is either a (Left) infix op
    -- identifier or a (Right) type. the Int components are start and
    -- end offsets of the thing being parsed.
    parseTypeOrInfixOp
      :: ParserState σ
      => Parser σ P.Pos
      -> Parser σ (Either (Int, Int, Identifier) (Int, Int, Type))
    parseTypeOrInfixOp indent_check =
      P.try
        ( do
            s <- P.getOffset
            t <- parseAtomicType indent_check
            en <- P.getOffset
            return $ Right (s, en, t)
        )
        <|> ( do
                s <- P.getOffset
                t <- parseInfixTypeIdentifier indent_check
                en <- P.getOffset
                return $ Left (s, en, t)
            )

    -- get the second operator in the list, if there is any. the reason
    -- we want this is so that we error out and tell the user that we do
    -- not know anything about operator precedence (they are probably defined)
    -- in some random Haskell module, so it is better to just parenthesize
    -- stuff.
    getSecondOp
      :: Bool -- whether we've seen an operator already
      -> Int -- the offset of the last guy (this is where the first infix
      -- app would end)
      -> [Either (Int, Int, Identifier) (Int, Int, Type)]
      -> Maybe (Int, (Int, Int, Identifier))
    getSecondOp _ _ [] = Nothing
    getSecondOp False _ [_] = Nothing
    getSecondOp False _ (Left (_, en, _) : xs) = getSecondOp True en xs
    getSecondOp b _ (Right (_, en, _) : xs) = getSecondOp b en xs
    getSecondOp True en (Left x : _) = Just (en, x)

    -- this gives something as long as the last guy in the list is
    -- a Left. if the last guy in the list is not a Left, it returns
    -- nothing. we use this to error out and tell the user if they forgot
    -- the RHS of an infix app.
    getLastOp :: [Either a b] -> Maybe a
    getLastOp [] = Nothing
    getLastOp [Left x] = Just x
    getLastOp [Right _] = Nothing
    getLastOp (_ : xs) = getLastOp xs

    -- assuming the list has only one infix op (i.e., only one Left in the list)
    -- , this partitions it so that we have (types, op, types).
    partitionByInfix
      :: [Either (Int, Int, Identifier) (Int, Int, Type)]
      -> Maybe ([Type], Identifier, [Type])
    partitionByInfix [] = Nothing
    partitionByInfix (Right (_, _, x) : xs) =
      (\(b, i, a) -> (x : b, i, a)) <$> partitionByInfix xs
    partitionByInfix (Left (_, _, x) : xs) = Just ([], x, forget xs)

    -- forgets all the Lefts and offsets of the list and gets just the
    -- Types.
    forget :: [Either a (Int, Int, Type)] -> [Type]
    forget [] = []
    forget (Left _ : xs) = forget xs
    forget (Right (_, _, t) : xs) = t : forget xs
    -- Combine two types into a type application: @t t'@.
    -- Generates a fresh NodeId and computes the position span from the
    -- start of @t@ to the end of @t'@.
    buildAppType :: ParserState σ => Type -> Type -> Parser σ Type
    buildAppType t t' = do
      new_id <- getNewNodeId
      t_pos <- getPosition t
      t'_pos <- getPosition t'
      let new_pos =
            DPos.Position
              { DPos.begin = DPos.begin t_pos
              , DPos.end = DPos.end t'_pos
              , DPos.file = DPos.file t_pos
              }
      let app = TypeApp new_id t t'
      setPosition app new_pos
      return app

--------------------------------------------------------------------------------

-- * Atomic Types

--------------------------------------------------------------------------------

-- | Parse an atomic (parenthesized) type—types which can be parsed easily as
-- being a single type, e.g., @(X, Y)@, @[Int]@, @String@.
--
--   This is the central fallback parser used throughout the type grammar.
--   Every type ultimately resolves to one of these alternatives.
--
-- @since 26.8
parseAtomicType :: ParserState σ => Parser σ P.Pos -> Parser σ Type
parseAtomicType indent_check = inContext "atomic type" $ do
  tok <- P.lookAhead P.anySingle
  case tok of
    '[' -> parseListType indent_check
    '(' ->
      P.try
        (parseOpTypeVar indent_check)
        <|> parseParenthesizedType indent_check
    '"' -> parseTypeSymbolLit indent_check
    _ ->
      P.try (parseTypeNatLit indent_check)
        <|> parseNonOpTypeVar indent_check

--------------------------------------------------------------------------------

-- * Other 'Type' Parsers

--------------------------------------------------------------------------------

-- | Parse a variable type, which is a capitalized identifier.
--
--   This parser handles type variables and type constructors such as
--   @Int@, @Maybe@, or @Data.Map.Map@.
--
--   We separate this out from 'parseOpTypeVar' for lookahead purposes.
--
-- @since 26.8
parseNonOpTypeVar
  :: ParserState σ
  => Parser σ P.Pos
  -- ^ The indentation check
  -> Parser σ Type
parseNonOpTypeVar indentCheck =
  parsePositioned $ do
    _ <- indentCheck
    ident <- parseCapitalizedIdentifier
    i <- getNewNodeId
    return $ TypeName i ident

-- | Parse a variable type that is a parenthesized type symbol.
--
--   We separate this out from 'parseNonOpTypeVar' for lookahead purposes.
-- @since 26.8
parseOpTypeVar
  :: ParserState σ
  => Parser σ P.Pos
  -- ^ The indentation check
  -> Parser σ Type
parseOpTypeVar indentCheck = parsePositioned $ do
  _ <- indentCheck
  ident <- parseNonInfixTypeOpIdentifier indentCheck
  i <- getNewNodeId
  return $ TypeName i ident

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
parseTypeNatLit :: ParserState σ => Parser σ P.Pos -> Parser σ Type
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
--
-- @since 26.7
parseTypeSymbolLit :: ParserState σ => Parser σ P.Pos -> Parser σ Type
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

parseParenthesizedType :: ParserState σ => Parser σ P.Pos -> Parser σ Type
parseParenthesizedType indent_check = inContext "parenthesized type" $ parsePositioned $ do
  _ <- indent_check
  maybe_unit <- P.observing (P.try $ betweenParentheses indent_check (return ()))
  case maybe_unit of
    Right _ -> TypeUnit <$> getNewNodeId
    Left _ -> do
      e <-
        betweenParentheses
          indent_check
          (commaSepBy1 indent_check (parseType indent_check))
      case e of
        t :| [] -> return t
        (t1 :| t2 : xs) -> do
          i <- getNewNodeId
          return $ TypeTuple i t1 (t2 :| xs)

-- | Parse a list type, such as @[T]@.
--
-- @since 26.7
parseListType :: ParserState σ => Parser σ P.Pos -> Parser σ Type
parseListType indent_check = inContext "list type" $ parsePositioned $ do
  _ <- indent_check
  t <-
    betweenSquareBrackets
      indent_check
      (parseType indent_check)
  i <- getNewNodeId
  return $ TypeList i t
