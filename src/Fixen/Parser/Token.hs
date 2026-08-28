{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser.Token
--     Description : Token-level parsers for the Fixen language
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines all token-level parsers used by the Fixen parser.
--     These are the lowest-level parsers that consume raw 'Text' input and
--     produce either unannotated 'Text' values or annotated identifier
--     types. They are organized into four categories:
--
--     * Raw string parsers — parse unannotated text fragments (identifiers,
--       operator characters) without source-position annotations
--
--     * Identifier parsers — parse various forms of identifiers and fully
--       qualified names, wrapping them in 'SimpleIdentifier',
--       'FullyQualifiedName', 'ModuleName', and 'Identifier'
--       types while recording source positions via 'parsePositioned'
--
--     * Literal parsers — parse string, integer, and natural number literals
--
--     * Keywords and constants — reserved words, operator characters, and
--       helper combinators for parsing keywords and operators with proper
--       trailing-character guards
--
--     The 'parseRaw...' parsers produce unannotated 'Text' values and serve
--     as the foundation for the higher-level 'parse...Identifier' parsers,
--     which wrap results in 'SimpleIdentifier', 'FullyQualifiedName',
--     and 'Identifier' types while recording source positions.
--
-- @since 26.7
module Fixen.Parser.Token where

import Control.Applicative.Combinators (
  manyTill_,
  (<|>),
 )
import Control.Monad.Combinators.NonEmpty qualified as PNE
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Error.Diagnose.Position
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Error
import Fixen.Utils
import GHC.Natural (Natural)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos qualified as MPos

-------------------------------------------------------------------------------

-- * Tokenizers

-- $raw
-- The @parseRaw...@ parsers are the lowest-level parsers that directly parse
-- raw 'Text' input and produce unannotated base types ('Text', 'Char').
-- These parsers do __not__ record source positions; they are building blocks
-- used by the higher-level identifier parsers that wrap results with
-- 'parsePositioned'.

-------------------------------------------------------------------------------

-- | A parser for a generic token. This parser always
-- succeeds on non-empty input. This means that the output must
-- be inspected for this parser to be meaningful. In particular,
-- you must supply an "expected" whenever the parser that uses 'token'
-- fails. This is so that we can properly highlight failing tokens
-- in error messages instead of highlighting only the first character.
--
-- @since 26.8
parseSomeToken :: Parser σ Text
parseSomeToken =
  parseAlphaNumToken
    <|> parseOpToken
    -- at this point, this is just bullshit
    -- used for error messages
    <|> fmap pack (P.some C.digitChar)
    -- some unrecognizable thing
    <|> fmap Text.singleton P.anySingle
  where
    parseAlphaNumToken :: Parser σ Text
    parseAlphaNumToken =
      -- Parse the identifier: first char is lowercase, uppercase, or
      -- underscore; followed by zero or more alphanumeric characters,
      -- underscores, or apostrophes
      fmap pack $
        (:)
          <$> (C.lowerChar <|> P.single '_' <|> C.upperChar) -- first char: any case letter or underscore
          <*> P.many (C.alphaNumChar <|> P.single '_' <|> P.single '\'') -- rest: alphanumeric, underscore, or apostrophe
    parseOpToken :: Parser σ Text
    parseOpToken = P.takeWhile1P (Just "operator character") isValidOpChar

-- | Parses an identifier string whose first character can be lowercase,
-- uppercase, or underscore, with remaining characters being valid Haskell
-- identifier characters. The parsed string must not match any entry in
-- 'reserved'.
--
-- It also rejects holes @_@.
--
-- Examples: @hello@ (accepted), @Hello@ (accepted), @_123@ (accepted),
--           @if@ (rejected—reserved), @_@ (rejected—hole).
--
-- @since 26.7
parseRawAnyCaseHsIdentifierString :: Parser σ Text
parseRawAnyCaseHsIdentifierString = do
  -- Capture the current byte offset so we can report errors at the
  -- correct source position if the result turns out to be a reserved keyword
  offset_start <- P.getOffset
  str <- parseSomeToken
  if
    | Text.any isValidOpChar str ->
        -- operator; disallowed!
        standard_error offset_start str []
    | Nothing <- Text.uncons str ->
        -- parseSomeToken always returns nonempty input, unless
        -- EOF is reached, in which case, parseSomeToken will error out
        -- anyway.
        error "unreachable!"
    | str ∈ reserved ->
        -- disallow keywords
        standard_error
          offset_start
          str
          [Note $ "'" ++ unpack str ++ "' is a reserved keyword"]
    | Just (h, _) <- Text.uncons str
    , (¬) (isUpper h) ∧ (¬) (isLower h) ∧ h ≠ '_' ->
        standard_error offset_start str []
    | str == "_" ->
        standard_error
          offset_start
          str
          [Note $ "hole '" ++ unpack str ++ "' is not allowed in this position"]
    | otherwise -> return str
  where
    standard_error offset s h =
      customErrorWithOffset offset $
        FixenTrivialParseError
          (Just (textToTokens s))
          (Set.singleton (P.Label $ 'i' :| "dentifier"))
          []
          h

-- | Parses an identifier string whose first character is a lowercase letter
-- or underscore (@_@), with the remaining characters being valid Haskell
-- identifier characters (alphanumeric, underscore, apostrophe). The parsed
-- string must not match any entry in 'reserved'.
--
-- It also rejects holes @_@.
--
-- Examples: @hello@ (accepted), @x'@ (accepted), @_123X@ (accepted),
--           @Int@ (rejected—starts with uppercase), @if@ (rejected — reserved),
--           @_@ (rejected—hole)
--
-- @since 26.7
parseRawLowerHsIdentifierString :: Parser σ Text
parseRawLowerHsIdentifierString = P.try $ do
  -- Capture the current byte offset so we can report errors at the
  -- correct source position if the result turns out to be a reserved keyword
  offset_start <- P.getOffset
  str <- parseRawAnyCaseHsIdentifierString
  if
    | Just (h, _) <- Text.uncons str
    , (¬) (isLower h) ∧ h ≠ '_' ->
        customErrorWithOffset offset_start $
          FixenTrivialParseError
            (Just (textToTokens str))
            ( Set.singleton
                (P.Label $ 'i' :| "dentifier starting with lowercase letter")
            )
            []
            []
    | otherwise -> return str

-- | Same as 'parseRawLowerHsIdentifierString', except that it also accepts
-- the hole @_@.
--
-- @since 26.7
parseRawLowerHsIdentifierStringOrHole :: Parser σ Text
parseRawLowerHsIdentifierStringOrHole =
  P.try parseRawLowerHsIdentifierString <|> C.string "_"

-- | Parses an identifier string whose first character is an uppercase letter,
-- with the remaining characters being valid Haskell type/constructor identifier
-- characters (alphanumeric, underscore, apostrophe).
--
-- Examples: @Int@ (accepted), @A'@ (accepted), @int@ (rejected — starts lowercase)
--
-- @since 26.7
parseRawCapitalizedHsIdentifierString :: Parser σ Text
parseRawCapitalizedHsIdentifierString = do
  -- Capture the current byte offset so we can report errors at the
  -- correct source position if the result turns out to be a reserved keyword
  offset_start <- P.getOffset
  str <- parseRawAnyCaseHsIdentifierString
  if
    | Just (h, _) <- Text.uncons str
    , (¬) (isUpper h) ->
        customErrorWithOffset offset_start $
          FixenTrivialParseError
            (Just (textToTokens str))
            ( Set.singleton
                ( P.Label $
                    'i'
                      :| "dentifier starting with uppercase/titlecase letter"
                )
            )
            []
            []
    | otherwise -> return str

-- | Parses a non-empty string of operator characters.
-- The resulting string must not match any entry in 'reservedOps'.
-- Requires at least one operator character.
--
-- Examples: @++@ (accepted), @<=@ (accepted), @=@ (rejected — reserved)
--
-- @since 26.7
parseRawOpIdentifierString :: Parser σ Text
parseRawOpIdentifierString = do
  offset_start <- P.getOffset
  str <- parseSomeToken
  if
    | (¬) (Text.all isValidOpChar str) ->
        standard_error offset_start str []
    | str ∈ reservedOps ->
        standard_error
          offset_start
          str
          [Note $ "'" ++ unpack str ++ "' is a reserved symbol"]
    | Text.all (== '-') str ∧ Text.length str >= 2 ->
        standard_error
          offset_start
          str
          [Note "dashes start comments"]
    | otherwise -> return str
  where
    standard_error offset s h =
      customErrorWithOffset offset $
        FixenTrivialParseError
          (Just $ textToTokens s)
          (Set.singleton $ P.Label $ 'o' :| "perator")
          []
          h

-------------------------------------------------------------------------------

-- * Identifier parsers

-- $id
-- The @parse...Identifier@ parsers parse '-annotated strings. These parsers
-- __do not consume trailing whitespace__ after parsing. Different kinds of
-- identifiers are valid in different syntactic contexts: for instance,
-- operators can appear without parentheses in infix position, while regular
-- identifiers must be surrounded by backticks when used infix.

-------------------------------------------------------------------------------

-- | Parses a module name as a series of one or more capitalized simple
-- identifiers separated by dots. For example, @Data.List@ or
-- @MyCompany.MyProject.MyModule@.
--
-- The result is wrapped in 'ModuleName' with a fresh node ID and
-- source position recorded via 'parsePositioned'.
--
-- @since 26.7
parseModuleName :: ParserState σ => Parser σ ModuleName
parseModuleName = parsePositioned $ do
  -- Parse one or more capitalized identifiers separated by dots,
  -- producing a NonEmpty list of SimpleIdentifiers
  ls <- PNE.sepBy1 parseCapitalizedSimpleIdentifier (exactMatchNoLookahead ".")
  -- Allocate a fresh node ID for the module name node
  i <- getNewNodeId
  -- Construct and return the ModuleName value
  return $ ModuleName i ls

-- | Parses a simple identifier starting with a lowercase letter (or underscore)
-- and wraps it in 'SimpleIdentifier'. These represent non-constructor
-- variables that are __not__ used in infix position.
--
-- Source position and a fresh node ID are recorded via 'parsePositioned'.
--
-- @since 26.7
parseLowerFirstSimpleIdentifier :: ParserState σ => Parser σ SimpleIdentifier
parseLowerFirstSimpleIdentifier = parsePositioned $ do
  -- Parse the raw identifier string (lowercase-first, not reserved)
  str <- parseRawLowerHsIdentifierString
  -- Allocate a fresh node ID for the node
  i <- getNewNodeId
  -- Construct and return the SimpleIdentifier node
  return $ SimpleIdentifier i str

-- | Same as 'parseLowerFirstSimpleIdentifier', except that it accepts
-- the hole @_@.
--
-- @since 26.7
parseLowerFirstSimpleIdentifierOrHole :: ParserState σ => Parser σ SimpleIdentifier
parseLowerFirstSimpleIdentifierOrHole = parsePositioned $ do
  -- Parse the raw identifier string (lowercase-first, not reserved)
  str <- parseRawLowerHsIdentifierStringOrHole
  -- Allocate a fresh node ID for the node
  i <- getNewNodeId
  -- Construct and return the SimpleIdentifier node
  return $ SimpleIdentifier i str

-- | Parses a fully qualified name whose last component starts with a lowercase
-- letter. For example, @Data.List.map@ or @MyModule.myFunction@.
--
-- @since 26.7
parseLowerFirstFQN :: ParserState σ => Parser σ FullyQualifiedName
parseLowerFirstFQN = parsePositioned $ do
  -- Parse the module prefix: one or more capitalized identifiers separated by dots
  module_name <- parsePrefix
  -- Consume the dot separator between module name and final identifier
  _ <- exactMatchNoLookahead "."
  -- Parse the final lowercase-first simple identifier
  ident <- parseLowerFirstSimpleIdentifier
  -- Allocate a fresh node ID and construct the FullyQualifiedName node
  i <- getNewNodeId
  return $ FullyQualifiedName i module_name ident
  where
    -- Parse a module name as one or more capitalized identifiers separated by dots
    parsePrefix :: ParserState σ => Parser σ ModuleName
    parsePrefix = parsePositioned $ do
      hd <- parseCapitalizedSimpleIdentifier
      tl <-
        P.many $
          P.try $
            exactMatchNoLookahead "." *> parseCapitalizedSimpleIdentifier
      i <- getNewNodeId
      return $ ModuleName i (hd NE.:| tl)

-- | Parses a simple identifier starting with an uppercase letter and wraps
-- it in 'SimpleIdentifier'. These represent constructor names or
-- capitalized variables that are __not__ used in infix position.
--
-- @since 26.7
parseCapitalizedSimpleIdentifier :: ParserState σ => Parser σ SimpleIdentifier
parseCapitalizedSimpleIdentifier = parsePositioned $ do
  str <- parseRawCapitalizedHsIdentifierString
  i <- getNewNodeId
  return $ SimpleIdentifier i str

-- | Parses a fully qualified name whose last component starts with an uppercase
-- letter. For example, @Data.List@ or @MyModule.MyType@.
--
-- @since 26.7
parseCapitalizedFQN :: ParserState σ => Parser σ FullyQualifiedName
parseCapitalizedFQN = do
  x <- parseCapitalizedSimpleIdentifier
  _ <- exactMatchNoLookahead "."
  x' <- parseCapitalizedSimpleIdentifier
  ls <- P.many (exactMatchNoLookahead "." *> parseCapitalizedSimpleIdentifier)
  -- Parse one or more capitalized identifiers separated by dots
  let -- Split into first component and the remaining non-empty tail
      (first_ident, remaining_idents) = (x, x' NE.:| ls)
      -- The module name comprises all components except the last one
      mod_name = first_ident NE.:| NE.init remaining_idents
      -- The final name component is the last element
      name = NE.last remaining_idents
  -- Get source positions for the first and last module name components
  m_init_pos <- getPosition first_ident
  m_last_pos <- getPosition (NE.last mod_name)
  -- Get source position for the final name component
  name_pos <- getPosition name
  let -- The module name position spans from the start of the first
      -- component to the end of the last module name component
      m_pos =
        Position
          { begin = begin m_init_pos
          , end = end m_last_pos
          , file = file m_init_pos
          }
      -- The full FQN position spans from the start of the first
      -- component to the end of the final name component
      fqn_pos =
        Position
          { begin = begin m_init_pos
          , end = end name_pos
          , file = file m_init_pos
          }
  -- Allocate fresh node IDs for the ModuleName and FullyQualifiedName nodes
  i <- getNewNodeId
  i' <- getNewNodeId
  -- Construct the nodes
  let parsed_mod_name = ModuleName i mod_name
      parsed_fqn = FullyQualifiedName i' parsed_mod_name name
  -- Attach source positions to the ModuleName and FullyQualifiedName nodes
  setPosition parsed_mod_name m_pos
  setPosition parsed_fqn fqn_pos
  -- Return the FullyQualifiedName node
  return $ FullyQualifiedName i' (ModuleName i mod_name) name

-- | Parses either a capitalized fully qualified name (e.g. @Data.List@) or
-- a capitalized simple identifier (e.g. @Just@). Returns the result wrapped
-- in the corresponding 'Identifier' constructor.
--
-- @since 26.7
parseCapitalizedIdentifier :: ParserState σ => Parser σ Identifier
parseCapitalizedIdentifier =
  -- Attempt to parse a capitalized fully qualified name first
  ( IdentifierFullyQualifiedName
      <$> P.try
        ( parseCapitalizedFQN
            <* P.notFollowedBy (P.single '.')
        )
  )
    -- Fall back to parsing a capitalized simple identifier
    <|> ( IdentifierSimpleIdentifier
            <$> parseCapitalizedSimpleIdentifier
            <* P.notFollowedBy (P.single '.')
        )

-- | Parses a simple identifier (any case for the first letter) and wraps
-- it in 'SimpleIdentifier'. These represent normal terms or constructors
-- that are __not__ used in infix position.
--
-- @since 26.7
parseAnyCasedLetterSimpleIdentifier :: ParserState σ => Parser σ SimpleIdentifier
parseAnyCasedLetterSimpleIdentifier = parsePositioned $ do
  str <- parseRawAnyCaseHsIdentifierString
  i <- getNewNodeId
  return $ SimpleIdentifier i str

-- | Parses a fully qualified name regardless of the case of its last component.
-- Attempts 'parseLowerFirstFQN' first (for names like @Data.List.map@),
-- then falls back to 'parseCapitalizedFQN' (for names like @Data.List@).
--
-- @since 26.7
parseAnyCasedLetterFQN :: ParserState σ => Parser σ FullyQualifiedName
parseAnyCasedLetterFQN = P.try parseLowerFirstFQN <|> parseCapitalizedFQN

-- | Parses either a fully qualified name (any case last component, e.g.
-- @Data.List@ or @Data.List.map@) or a simple identifier (any case, e.g.
-- @Just@ or @myFunction@).
--
-- @since 26.7
parseAnyCasedLetterIdentifier :: ParserState σ => Parser σ Identifier
parseAnyCasedLetterIdentifier =
  -- Attempt to parse a fully qualified name first (with backtracking)
  ( IdentifierFullyQualifiedName
      <$> P.try
        ( parseAnyCasedLetterFQN
            <* P.notFollowedBy (exactMatchNoLookahead ".")
        )
  )
    -- Fall back to parsing a simple identifier
    <|> ( IdentifierSimpleIdentifier
            <$> parseAnyCasedLetterSimpleIdentifier
            <* P.notFollowedBy (exactMatchNoLookahead ".")
        )

-- | Parses a simple operator identifier and wraps it in 'SimpleIdentifier'.
--
-- @since 26.7
parseOpSimpleIdentifier :: ParserState σ => Parser σ SimpleIdentifier
parseOpSimpleIdentifier = parsePositioned $ do
  str <- parseRawOpIdentifierString
  i <- getNewNodeId
  return $ SimpleIdentifier i str

-- | Parses a simple type operator identifier and wraps it in 'SimpleIdentifier'.
-- The parsed string must not match any entry in 'reservedOps'. Must start
-- with @:@ and have at least two characters.
--
-- @since 26.8
parseTypeOpSimpleIdentifier :: ParserState σ => Parser σ SimpleIdentifier
parseTypeOpSimpleIdentifier = parsePositioned $ do
  offset_start <- P.getOffset
  str <- parseRawOpIdentifierString
  case Text.uncons str of
    Just (':', "") ->
      standard_error offset_start str [Note "':' is a reserved keyword"]
    Just (':', _) -> do
      i <- getNewNodeId
      return $ SimpleIdentifier i str
    _ ->
      standard_error offset_start str [Note "type symbols must begin with ':'"]
  where
    standard_error offset s h =
      customErrorWithOffset offset $
        FixenTrivialParseError
          (Just (textToTokens s))
          (Set.singleton (P.Label $ 't' :| "ype symbol"))
          []
          h

-- | Parses a fully qualified type operator name, such as @Data.List.:++@.
-- The identifier portion must start with @:@.
--
-- @since 26.8
parseTypeOpFQN :: ParserState σ => Parser σ FullyQualifiedName
parseTypeOpFQN = parsePositioned $ do
  module_name <- parsePrefix
  _ <- exactMatchNoLookahead "."
  ident <- parseTypeOpSimpleIdentifier
  i <- getNewNodeId
  return $ FullyQualifiedName i module_name ident
  where
    parsePrefix :: ParserState σ => Parser σ ModuleName
    parsePrefix = parsePositioned $ do
      hd <- parseCapitalizedSimpleIdentifier
      tl <- P.many $ P.try $ exactMatchNoLookahead "." *> parseCapitalizedSimpleIdentifier
      i <- getNewNodeId
      return $ ModuleName i (hd NE.:| tl)

-- | Parses a fully qualified operator name, such as @Data.List.++@.
-- The structure is: capitalized module path followed by a dot and an
-- operator identifier. The result is wrapped in 'FullyQualifiedName'
-- with source position recorded via 'parsePositioned'.
--
-- @since 26.7
parseOpFQN :: ParserState σ => Parser σ FullyQualifiedName
parseOpFQN = parsePositioned $ do
  module_name <- parsePrefix
  _ <- exactMatchNoLookahead "."
  ident <- parseOpSimpleIdentifier
  i <- getNewNodeId
  return $ FullyQualifiedName i module_name ident
  where
    parsePrefix :: ParserState σ => Parser σ ModuleName
    parsePrefix = parsePositioned $ do
      hd <- parseCapitalizedSimpleIdentifier
      tl <- P.many $ P.try $ exactMatchNoLookahead "." *> parseCapitalizedSimpleIdentifier
      i <- getNewNodeId
      return $ ModuleName i (hd NE.:| tl)

-- | Parses either a fully qualified operator name (e.g. @Data.List.++@) or
-- a simple operator identifier (e.g. @++@). Returns the result wrapped in
-- the corresponding 'Identifier' constructor.
--
-- @since 26.7
parseOpIdentifier :: ParserState σ => Parser σ Identifier
parseOpIdentifier =
  ( IdentifierFullyQualifiedName
      <$> P.try (parseOpFQN <* P.notFollowedBy (exactMatchNoLookahead "."))
  )
    <|> ( IdentifierSimpleIdentifier
            <$> ( parseOpSimpleIdentifier
                    <* P.notFollowedBy (exactMatchNoLookahead ".")
                )
        )

-- | Parses either a fully qualified operator name (e.g. @Data.List.:+@) or
-- a simple operator identifier (e.g. @++@). Returns the result wrapped in
-- the corresponding 'Identifier' constructor.
--
-- @since 26.8
parseTypeOpIdentifier :: ParserState σ => Parser σ Identifier
parseTypeOpIdentifier =
  -- Attempt to parse a fully qualified operator name first
  ( IdentifierFullyQualifiedName
      <$> P.try
        ( parseTypeOpFQN
            <* P.notFollowedBy (exactMatchNoLookahead ".")
        )
  )
    -- Fall back to parsing a simple operator identifier
    <|> ( IdentifierSimpleIdentifier
            <$> ( parseTypeOpSimpleIdentifier
                    <* P.notFollowedBy (exactMatchNoLookahead ".")
                )
        )

-- | Parses a term-level identifier usable in infix position. This covers:
--
--   1. Letter-based identifiers surrounded by backticks (e.g. @`elem`@, @`Just`@)
--   2. Operator identifiers (e.g. @++@, @<=@)
--
-- The 'indent_check' argument is used by 'parseInfixLetterIdentifier' to
-- verify proper indentation around the backticks.
--
-- @since 26.7
parseInfixTermIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseInfixTermIdentifier indent_check =
  P.try (parseInfixLetterIdentifier indent_check)
    <|> parseOpIdentifier

-- | Parses a type-level identifier usable in infix position. This covers:
--
--   1. Letter-based identifiers surrounded by backticks (e.g. @`List`@, @`Data.Text`@)
--   2. Operator identifiers (e.g. @:++:@, @Data.List.:@)
--
-- The 'indent_check' argument is used by 'parseInfixTypeLetterIdentifier' to
-- verify proper indentation around the backticks.
--
-- @since 26.8
parseInfixTypeIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseInfixTypeIdentifier indent_check =
  -- Attempt to parse a backtick-wrapped letter identifier (with backtracking)
  P.try (parseInfixTypeLetterIdentifier indent_check)
    -- Fall back to parsing an operator identifier
    <|> parseTypeOpIdentifier

-- | Parses a letter-based identifier used in infix position, surrounded
-- by backticks (e.g. @`elem`@, @`notElem`@, @`Just`@). The annotated
-- source positions stored in the do __not__ include the backtick
-- characters themselves — only the inner identifier is position-annotated.
--
-- The 'indentCheck' argument verifies that both the opening and closing
-- backticks are at proper indentation levels.
--
-- @since 26.7
parseInfixLetterIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseInfixLetterIdentifier indentCheck = do
  _ <- exactMatchNoLookahead "`"
  _ <- indentCheck
  ident <- parseAnyCasedLetterIdentifier
  _ <- indentCheck
  _ <- exactMatchNoLookahead "`"
  return ident

-- | Parses a identifier for types used in infix position, surrounded
-- by backticks (e.g. @`List`@, @`Data.Text`@). The annotated
-- source positions stored in the do __not__ include the backtick
-- characters themselves — only the inner identifier is position-annotated.
--
-- The 'indentCheck' argument verifies that both the opening and closing
-- backticks are at proper indentation levels.
--
-- @since 26.8
parseInfixTypeLetterIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseInfixTypeLetterIdentifier indentCheck = do
  _ <- exactMatchNoLookahead "`"
  _ <- indentCheck
  ident <- parseCapitalizedIdentifier
  _ <- indentCheck
  _ <- exactMatchNoLookahead "`"
  return ident

-- | Parses a term-level identifier that is __not__ used in infix position.
-- This covers:
--
--   1. Lowercase-starting identifiers (e.g. @myFunction@, @MyModule.var@)
--   2. Capitalized-starting identifiers for constructors (e.g. @Just@, @Data.Maybe.Just@)
--   3. Parenthesized operators (e.g. @(++)@, @(Data.List.++)@)
--
-- The 'indent_check' argument is passed to 'parseNonInfixOpIdentifier' to
-- verify proper indentation of the parentheses.
--
-- @since 26.7
parseNonInfixTermIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseNonInfixTermIdentifier indent_check =
  P.try parseAnyCasedLetterIdentifier
    <|> parseNonInfixOpIdentifier indent_check

-- | Parses a type operator identifier used in non-infix (prefix) notation,
-- wrapped in parentheses (e.g. @(:+)@, @(Data.List.:|)@). Essentially
-- 'parseTypeOpIdentifier' surrounded by parentheses.
--
-- The 'indent_check' argument verifies proper indentation of both the
-- opening and closing parentheses.
--
-- @since 26.8
parseNonInfixTypeOpIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseNonInfixTypeOpIdentifier indent_check =
  betweenParentheses indent_check parseTypeOpIdentifier

-- | Parses an operator identifier used in non-infix (prefix) notation,
-- wrapped in parentheses (e.g. @(++)@, @(Data.List.++)@). Essentially
-- 'parseOpIdentifier' surrounded by parentheses.
--
-- The 'indent_check' argument verifies proper indentation of both the
-- opening and closing parentheses.
--
-- @since 26.7
parseNonInfixOpIdentifier :: ParserState σ => Parser σ MPos.Pos -> Parser σ Identifier
parseNonInfixOpIdentifier indent_check =
  betweenParentheses indent_check (indent_check >> parseOpIdentifier)

-------------------------------------------------------------------------------
--
-- Literals
--
-------------------------------------------------------------------------------

-- | Parses a string literal: a double-quoted sequence of characters.
-- The content is parsed using 'L.charLiteral' which handles escape sequences
-- (e.g. @\"\\n\"@, @\"\\t\"@, @\"\\\\\"@). The result is an unannotated
-- 'Text' value containing the string content (without the surrounding quotes).
--
-- Examples: @\"hello\"@ (accepted, yields @\"hello\"@),
--           @\"line1\\nline2\"@ (accepted, yields @\"line1\\nline2\"@)
--
-- @since 26.7
parseRawString :: Parser σ Text
parseRawString = do
  start <- P.getOffset
  (str, end) <- exactMatchNoLookahead "\"" >> manyTill_ L.charLiteral (C.char '"' <|> C.char '\n')
  if end == '\n'
    then
      customErrorWithOffset start $
        FixenTrivialParseError
          (Just (textToTokens $ Text.cons '"' $ pack str))
          (Set.singleton (P.Label $ 's' :| "tring literal"))
          []
          [Note "unclosed string literal"]
    else
      return $ pack str

-- | Parses a signed integer literal (with optional leading @+@ or @-@ sign).
-- The result is an unannotated 'Integer' value.
--
-- Uses 'L.signed' with 'sc' (whitespace consumer) for the sign and
-- 'L.decimal' for the decimal integer digits.
--
-- Examples: @42@ (accepted), @-7@ (accepted), @+3@ (accepted)
--
-- @since 26.7
parseRawInteger :: Parser σ Integer
parseRawInteger = L.signed sc L.decimal -- optional sign followed by decimal digits

-- | Parses an unsigned natural number literal (no sign allowed).
-- The result is an unannotated 'Natural' value.
--
-- Uses 'L.decimal' to parse non-negative decimal digits.
--
-- Examples: @0@ (accepted), @123@ (accepted), @-5@ (rejected — no sign allowed)
--
-- @since 26.7
parseRawNatural :: Parser σ Natural
parseRawNatural = L.decimal -- decimal digits only, no sign

-------------------------------------------------------------------------------
--
-- Keywords and constants
--
-- These define reserved words, operator characters, and helper combinators
-- for parsing keywords with proper trailing-character guards.
--
-- IMPORTANT: If any of these values change, update their Haddock docs above!
--
-------------------------------------------------------------------------------

-- | Reserved keywords that cannot be used as identifier names.
--
--   If a raw identifier string matches any entry in this list, the
--   'parseRawLowerHsIdentifierString' and 'parseRawAnyCaseHsIdentifierString'
--   parsers will reject it with a parse error.
--
--   Currently contains all Haskell keywords
--
-- @since 26.7
reserved :: [Text]
reserved =
  [ "if"
  , "then"
  , "else"
  , "data"
  , "type"
  , "family"
  , "import"
  , "module"
  , "where"
  , "class"
  , "instance"
  , "case"
  , "of"
  , "let"
  , "in"
  , "newtype"
  , "deriving"
  , "qualified"
  , "hiding"
  , "do"
  , "infix"
  , "infixl"
  , "infixr"
  , "foreign"
  , "proc"
  , "rec"
  ]

-- | Reserved operator symbols that cannot be used as operator identifiers.
--
--   If a raw operator string matches any entry in this list, the
--   'parseRawOpIdentifierString' parser will reject it with a parse error.
--
--   Currently contains: @["=", "..", "::", "\\", "|", "<-", "@", "~", "=>", "->"]@
--
--   The values changed since 26.8
--
-- @since 26.7
reservedOps :: [Text]
reservedOps = ["=", "..", "::", "\\", "|", "<-", "@", "~", "=>", "->"]

-- | Parses a keyword string, ensuring it is not immediately followed by
-- identifier-continuation characters (alphanumeric, underscore, apostrophe).
--
-- This guard prevents partial keyword matches — for example, parsing the
-- keyword @if@ will reject input like @ifdef@ because the 'd' after @if@
-- is a valid identifier continuation character.
--
-- @since 26.7
keyword :: Text -> Parser σ Text
keyword s = do
  start <- P.getOffset
  x <- parseSomeToken P.<?> show (Text.unpack s)
  if x ≠ s
    then
      customErrorWithOffset start $
        FixenTrivialParseError
          (Just (textToTokens x))
          (Set.singleton (textToTokens s))
          []
          []
    else return x

-- | Parses a turnstile symbol used in Fixen's sequent notation.
-- Accepts either the Unicode turnstile (@⊢@) or the ASCII variant (@|-@).
--
-- @since 26.7
turnstile :: Parser σ Text
turnstile =
  -- Attempt the Unicode turnstile first (with backtracking)
  P.try (keyword "⊢")
    -- Fall back to the ASCII turnstile variant
    <|> keyword "|-"

-- | Parses a less-than-or-equal symbol used for partial order relations.
-- Accepts either the ASCII less-than sign (@<@) or the Unicode
-- subset-of-or-equal symbol (@⊏@).
--
-- @since 26.7
ltOrSqSubsetEq :: Parser σ Text
ltOrSqSubsetEq =
  -- Attempt the ASCII less-than first (with backtracking)
  P.try (keyword "<")
    -- Fall back to the Unicode subset-of-or-equal symbol
    <|> keyword "⊏"
