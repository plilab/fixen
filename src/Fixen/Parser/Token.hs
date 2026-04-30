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
--     produce either unannotated 'Text' values or AST-annotated identifier
--     types. They are organized into four categories:
--
--     * Raw string parsers — parse unannotated text fragments (identifiers,
--       operator characters) without source-position annotations
--
--     * Identifier parsers — parse various forms of identifiers and fully
--       qualified names, wrapping them in 'AST.SimpleIdentifier',
--       'AST.FullyQualifiedName', 'AST.ModuleName', and 'AST.Identifier'
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
--     which wrap results in 'AST.SimpleIdentifier', 'AST.FullyQualifiedName',
--     and 'AST.Identifier' types while recording source positions.
module Fixen.Parser.Token (
  -- * Raw string parsers
  -- $raw

  -- ** Letter identifier strings
  parseRawLowerHsIdentifierString,
  parseRawCapitalizedHsIdentifierString,
  parseRawAnyCaseHsIdentifierString,

  -- ** Operator identifier strings
  parseRawOpChar,
  parseRawOpIdentifierString,

  -- * Identifier parsers
  -- $id
  parseLowerFirstSimpleIdentifier,
  parseCapitalizedSimpleIdentifier,
  parseCapitalizedFQN,
  parseLowerFirstFQN,
  parseCapitalizedIdentifier,
  parseAnyCasedLetterSimpleIdentifier,
  parseAnyCasedLetterFQN,
  parseAnyCasedLetterIdentifier,
  parseOpSimpleIdentifier,
  parseOpFQN,
  parseOpIdentifier,
  parseInfixTermIdentifier,
  parseNonInfixTermIdentifier,
  parseNonInfixOpIdentifier,
  parseModuleName,

  -- * Literals
  parseRawString,
  parseRawInteger,
  parseRawNatural,

  -- * Miscellaneous
  opChars,
  reserved,
  reservedOps,
  keyword,
  keywordOp,
  turnstile,
  ltOrSqSubsetEq,
) where

import Control.Applicative.Combinators (
  choice,
  manyTill,
  some,
  (<|>),
 )
import Control.Monad.Combinators.NonEmpty qualified as PNE
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Error.Diagnose.Position
import Fixen.IR.AST qualified as AST
import Fixen.Monad
import Fixen.Parser.Common
import GHC.Natural (Natural)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))
import Text.Megaparsec.Pos qualified as MPos

-------------------------------------------------------------------------------
--
-- Tokenizers
--
-------------------------------------------------------------------------------

-- $raw
-- The @parseRaw...@ parsers are the lowest-level parsers that directly parse
-- raw 'Text' input and produce unannotated base types ('Text', 'Char').
-- These parsers do __not__ record source positions; they are building blocks
-- used by the higher-level identifier parsers that wrap results with
-- 'parsePositioned'.

-- | Parses an identifier string whose first character is a lowercase letter
-- or underscore (@_@), with the remaining characters being valid Haskell
-- identifier characters (alphanumeric, underscore, apostrophe). The parsed
-- string must not match any entry in 'reserved'.
--
-- Examples: @hello@ (accepted), @x'@ (accepted), @_123X@ (accepted),
--           @Int@ (rejected — starts with uppercase), @if@ (rejected — reserved)
parseRawLowerHsIdentifierString :: Parser Text
parseRawLowerHsIdentifierString = do
  -- Capture the current byte offset so we can report errors at the
  -- correct source position if the result turns out to be a reserved keyword
  offset_start <- P.getOffset
  -- Parse the identifier: first character must be lowercase or underscore,
  -- followed by zero or more alphanumeric characters, underscores, or apostrophes
  -- Then convert the resulting [Char] to a Text value
  str <-
    fmap pack $
      (:)
        <$> (C.lowerChar <|> P.single '_') -- first char: lowercase letter or underscore
        <*> P.many (C.alphaNumChar <|> P.single '_' <|> P.single '\'') -- rest: alphanumeric, underscore, or apostrophe
        -- Reject the parse if the resulting string matches a reserved keyword
  if str `elem` reserved
    then -- emit a parse error indicating the keyword was unexpected

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved keyword '" ++ unpack str ++ "'")
            )
        )
    else return str -- accept the parsed string

-- | Parses an identifier string whose first character is an uppercase letter,
-- with the remaining characters being valid Haskell type/constructor identifier
-- characters (alphanumeric, underscore, apostrophe). Unlike
-- 'parseRawLowerHsIdentifierString', this parser does __not__ reject reserved
-- keywords because type/constructor names cannot conflict with keywords.
--
-- Examples: @Int@ (accepted), @A'@ (accepted), @int@ (rejected — starts lowercase)
parseRawCapitalizedHsIdentifierString :: Parser Text
parseRawCapitalizedHsIdentifierString =
  -- Parse the capitalized identifier string and convert [Char] to Text
  fmap pack $
    (:)
      <$> C.upperChar -- first character MUST be an uppercase letter
      <*> P.many (C.alphaNumChar <|> P.single '_' <|> P.single '\'') -- remaining: alphanumeric, underscore, or apostrophe

-- | Parses an identifier string whose first character can be lowercase,
-- uppercase, or underscore, with remaining characters being valid Haskell
-- identifier characters. The parsed string must not match any entry in
-- 'reserved'.
--
-- Examples: @hello@ (accepted), @Hello@ (accepted), @_123@ (accepted),
--           @if@ (rejected — reserved)
parseRawAnyCaseHsIdentifierString :: Parser Text
parseRawAnyCaseHsIdentifierString = do
  -- Capture the current byte offset for error reporting
  offset_start <- P.getOffset
  -- Parse the identifier: first char is lowercase, uppercase, or underscore;
  -- followed by zero or more alphanumeric characters, underscores, or apostrophes
  str <-
    fmap pack $
      (:)
        <$> (C.lowerChar <|> P.single '_' <|> C.upperChar) -- first char: any case letter or underscore
        <*> P.many (C.alphaNumChar <|> P.single '_' <|> P.single '\'') -- rest: alphanumeric, underscore, or apostrophe
        -- Reject if the string matches a reserved keyword
  if str `elem` reserved
    then -- emit a parse error for the reserved keyword

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved keyword '" ++ unpack str ++ "'")
            )
        )
    else return str -- accept the parsed string

-- | Parses a single character from the set of valid Haskell operator
-- characters defined in 'opChars'. Each character in 'opChars' is wrapped
-- in 'P.single' and combined with 'choice' to try each in turn.
parseRawOpChar :: Parser Char
parseRawOpChar = choice (P.single <$> opChars) -- try each operator character

-- | Parses a non-empty string of operator characters (from 'opChars').
-- The resulting string must not match any entry in 'reservedOps'.
-- Requires at least one operator character.
--
-- Examples: @++@ (accepted), @<=@ (accepted), @=@ (rejected — reserved)
parseRawOpIdentifierString :: Parser Text
parseRawOpIdentifierString = do
  -- Capture the current byte offset for error reporting
  offset_start <- P.getOffset
  -- Parse one or more operator characters, then convert to Text
  -- Note: uses 'some' (not 'many') to require at least one operator char
  str <- pack <$> some parseRawOpChar
  -- Reject if the string matches a reserved operator
  if str `elem` reservedOps
    then -- emit a parse error for the reserved operator

      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail $ "unexpected reserved operator (" ++ unpack str ++ ")")
            )
        )
    else return str -- accept the parsed operator string

-------------------------------------------------------------------------------
--
-- Identifier parsers
--
-------------------------------------------------------------------------------

-- $id
-- The @parse...Identifier@ parsers parse 'AST'-annotated strings. These parsers
-- __do not consume trailing whitespace__ after parsing. Different kinds of
-- identifiers are valid in different syntactic contexts: for instance,
-- operators can appear without parentheses in infix position, while regular
-- identifiers must be surrounded by backticks when used infix.

-- | Parses a module name as a series of one or more capitalized simple
-- identifiers separated by dots. For example, @Data.List@ or
-- @MyCompany.MyProject.MyModule@.
--
-- The result is wrapped in 'AST.ModuleName' with a fresh node ID and
-- source position recorded via 'parsePositioned'.
parseModuleName :: Parser AST.ModuleName
parseModuleName = parsePositioned $ do
  -- Parse one or more capitalized identifiers separated by dots,
  -- producing a NonEmpty list of SimpleIdentifiers
  ls <- PNE.sepBy1 parseCapitalizedSimpleIdentifier (P.single '.')
  -- Allocate a fresh node ID for the module name AST node
  i <- fixenGetNewNodeId
  -- Construct and return the AST.ModuleName value
  return $ AST.ModuleName i ls

-- | Parses a simple identifier starting with a lowercase letter (or underscore)
-- and wraps it in 'AST.SimpleIdentifier'. These represent non-constructor
-- variables that are __not__ used in infix position.
--
-- Source position and a fresh node ID are recorded via 'parsePositioned'.
parseLowerFirstSimpleIdentifier :: Parser AST.SimpleIdentifier
parseLowerFirstSimpleIdentifier = parsePositioned $ do
  -- Parse the raw identifier string (lowercase-first, not reserved)
  str <- parseRawLowerHsIdentifierString
  -- Allocate a fresh node ID for the AST node
  i <- fixenGetNewNodeId
  -- Construct and return the SimpleIdentifier AST node
  return $ AST.SimpleIdentifier i str

-- | Parses a fully qualified name whose last component starts with a lowercase
-- letter. For example, @Data.List.map@ or @MyModule.myFunction@.
--
-- The structure is: capitalized module path (e.g. @Data.List@) followed by
-- a dot and a lowercase-starting simple identifier. The result is wrapped in
-- 'AST.FullyQualifiedName' with source position recorded via 'parsePositioned'.
parseLowerFirstFQN :: Parser AST.FullyQualifiedName
parseLowerFirstFQN = parsePositioned $ do
  -- Parse the module prefix: one or more capitalized identifiers separated by dots
  module_name <- parsePrefix
  -- Consume the dot separator between module name and final identifier
  _ <- P.single '.'
  -- Parse the final lowercase-first simple identifier
  ident <- parseLowerFirstSimpleIdentifier
  -- Allocate a fresh node ID and construct the FullyQualifiedName AST node
  i <- fixenGetNewNodeId
  return $ AST.FullyQualifiedName i module_name ident
  where
    -- Parse a module name as one or more capitalized identifiers separated by dots
    parsePrefix :: Parser AST.ModuleName
    parsePrefix = parsePositioned $ do
      -- Parse the first capitalized identifier (head of the module path)
      hd <- parseCapitalizedSimpleIdentifier
      -- Parse zero or more additional capitalized identifiers, each preceded by a dot
      tl <- manyNonFailing (P.single '.' *> parseCapitalizedSimpleIdentifier)
      -- Allocate a fresh node ID and construct the ModuleName AST node
      i <- fixenGetNewNodeId
      return $ AST.ModuleName i (hd NE.:| tl)
    -- Parse zero or more repetitions of a parser without failing if zero matches
    -- Uses 'P.observing' to backtrack: if the parser fails, returns empty list
    -- without consuming input; if it succeeds, recurses to find more
    manyNonFailing :: Parser a -> Parser [a]
    manyNonFailing p = do
      -- Attempt to parse with backtracking on failure
      m <- P.observing (P.try p)
      case m of
        Left _ -> return [] -- parser failed — no more items, return empty list
        Right e -> (:) e <$> manyNonFailing p -- got an item — cons and recurse

-- | Parses a simple identifier starting with an uppercase letter and wraps
-- it in 'AST.SimpleIdentifier'. These represent constructor names or
-- capitalized variables that are __not__ used in infix position.
--
-- Source position and a fresh node ID are recorded via 'parsePositioned'.
parseCapitalizedSimpleIdentifier :: Parser AST.SimpleIdentifier
parseCapitalizedSimpleIdentifier = parsePositioned $ do
  -- Parse the raw capitalized identifier string
  str <- parseRawCapitalizedHsIdentifierString
  -- Allocate a fresh node ID for the AST node
  i <- fixenGetNewNodeId
  -- Construct and return the SimpleIdentifier AST node
  return $ AST.SimpleIdentifier i str

-- | Parses a fully qualified name whose last component starts with an uppercase
-- letter. For example, @Data.List@ or @MyModule.MyType@.
--
-- The structure is: one or more capitalized identifiers separated by dots.
-- The result is split into a 'AST.ModuleName' (all but the last component)
-- and a final 'AST.SimpleIdentifier' (the last component). Source positions
-- are manually computed and attached via 'fixenSetPosition'.
--
-- Requires at least two dot-separated identifiers; a single identifier
-- (e.g. @Just@) is rejected here and should be parsed by
-- 'parseCapitalizedSimpleIdentifier' instead.
parseCapitalizedFQN :: Parser AST.FullyQualifiedName
parseCapitalizedFQN = do
  -- Capture the current byte offset for error reporting
  offset_start <- P.getOffset
  -- Parse one or more capitalized identifiers separated by dots
  ls <- P.sepBy1 parseCapitalizedSimpleIdentifier (P.single '.')
  case ls of
    -- Accept only if there are at least two components (module + name)
    (x : x' : ls') -> do
      let -- Split into first component and the remaining non-empty tail
          (first_ident, remaining_idents) = (x, x' NE.:| ls')
          -- The module name comprises all components except the last one
          mod_name = first_ident NE.:| NE.init remaining_idents
          -- The final name component is the last element
          name = NE.last remaining_idents
      -- Get source positions for the first and last module name components
      m_init_pos <- fixenGetPosition first_ident
      m_last_pos <- fixenGetPosition (NE.last mod_name)
      -- Get source position for the final name component
      name_pos <- fixenGetPosition name
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
      i <- fixenGetNewNodeId
      i' <- fixenGetNewNodeId
      -- Construct the AST nodes
      let parsed_mod_name = AST.ModuleName i mod_name
          parsed_fqn = AST.FullyQualifiedName i' parsed_mod_name name
      -- Attach source positions to the ModuleName and FullyQualifiedName nodes
      fixenSetPosition parsed_mod_name m_pos
      fixenSetPosition parsed_fqn fqn_pos
      -- Return the FullyQualifiedName AST node
      return $ AST.FullyQualifiedName i' (AST.ModuleName i mod_name) name
    -- If fewer than two components, emit an error requiring a module name
    _ ->
      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                (ErrorFail "expected module name")
            )
        )

-- | Parses either a capitalized fully qualified name (e.g. @Data.List@) or
-- a capitalized simple identifier (e.g. @Just@). Returns the result wrapped
-- in the corresponding 'AST.Identifier' constructor.
--
-- Uses 'P.try' on 'parseCapitalizedFQN' to backtrack if the FQN parse
-- fails, falling through to 'parseCapitalizedSimpleIdentifier'.
parseCapitalizedIdentifier :: Parser AST.Identifier
parseCapitalizedIdentifier =
  -- Attempt to parse a capitalized fully qualified name first
  (AST.IdentifierFullyQualifiedName <$> P.try parseCapitalizedFQN)
    -- Fall back to parsing a capitalized simple identifier
    <|> (AST.IdentifierSimpleIdentifier <$> parseCapitalizedSimpleIdentifier)

-- | Parses a simple identifier (any case for the first letter) and wraps
-- it in 'AST.SimpleIdentifier'. These represent normal terms or constructors
-- that are __not__ used in infix position.
--
-- Source position and a fresh node ID are recorded via 'parsePositioned'.
parseAnyCasedLetterSimpleIdentifier :: Parser AST.SimpleIdentifier
parseAnyCasedLetterSimpleIdentifier = parsePositioned $ do
  -- Parse the raw identifier string (any case, not reserved)
  str <- parseRawAnyCaseHsIdentifierString
  -- Allocate a fresh node ID for the AST node
  i <- fixenGetNewNodeId
  -- Construct and return the SimpleIdentifier AST node
  return $ AST.SimpleIdentifier i str

-- | Parses a fully qualified name regardless of the case of its last component.
-- Attempts 'parseLowerFirstFQN' first (for names like @Data.List.map@),
-- then falls back to 'parseCapitalizedFQN' (for names like @Data.List@).
--
-- Uses 'P.try' on the lower-first variant to backtrack on failure.
parseAnyCasedLetterFQN :: Parser AST.FullyQualifiedName
parseAnyCasedLetterFQN = P.try parseLowerFirstFQN <|> parseCapitalizedFQN

-- | Parses either a fully qualified name (any case last component, e.g.
-- @Data.List@ or @Data.List.map@) or a simple identifier (any case, e.g.
-- @Just@ or @myFunction@). Returns the result wrapped in the corresponding
-- 'AST.Identifier' constructor.
--
-- These represent normal terms or constructors that are __not__ used in infix.
parseAnyCasedLetterIdentifier :: Parser AST.Identifier
parseAnyCasedLetterIdentifier =
  -- Attempt to parse a fully qualified name first (with backtracking)
  (AST.IdentifierFullyQualifiedName <$> P.try parseAnyCasedLetterFQN)
    -- Fall back to parsing a simple identifier
    <|> (AST.IdentifierSimpleIdentifier <$> parseAnyCasedLetterSimpleIdentifier)

-- | Parses a simple operator identifier (a non-empty string of operator
-- characters from 'opChars') and wraps it in 'AST.SimpleIdentifier'.
-- The parsed string must not match any entry in 'reservedOps'.
--
-- Source position and a fresh node ID are recorded via 'parsePositioned'.
parseOpSimpleIdentifier :: Parser AST.SimpleIdentifier
parseOpSimpleIdentifier = parsePositioned $ do
  -- Parse the raw operator identifier string (non-empty, not reserved)
  str <- parseRawOpIdentifierString
  -- Allocate a fresh node ID for the AST node
  i <- fixenGetNewNodeId
  -- Construct and return the SimpleIdentifier AST node
  return $ AST.SimpleIdentifier i str

-- | Parses a fully qualified operator name, such as @Data.List.++@.
-- The structure is: capitalized module path followed by a dot and an
-- operator identifier. The result is wrapped in 'AST.FullyQualifiedName'
-- with source position recorded via 'parsePositioned'.
parseOpFQN :: Parser AST.FullyQualifiedName
parseOpFQN = parsePositioned $ do
  -- Parse the module prefix: one or more capitalized identifiers separated by dots
  module_name <- parsePrefix
  -- Consume the dot separator between module name and operator
  _ <- P.single '.'
  -- Parse the operator identifier
  ident <- parseOpSimpleIdentifier
  -- Allocate a fresh node ID and construct the FullyQualifiedName AST node
  i <- fixenGetNewNodeId
  return $ AST.FullyQualifiedName i module_name ident
  where
    -- Parse a module name as one or more capitalized identifiers separated by dots
    parsePrefix :: Parser AST.ModuleName
    parsePrefix = parsePositioned $ do
      -- Parse the first capitalized identifier (head of the module path)
      hd <- parseCapitalizedSimpleIdentifier
      -- Parse zero or more additional capitalized identifiers, each preceded by a dot
      tl <- manyNonFailing (P.single '.' *> parseCapitalizedSimpleIdentifier)
      -- Allocate a fresh node ID and construct the ModuleName AST node
      i <- fixenGetNewNodeId
      return $ AST.ModuleName i (hd NE.:| tl)
    -- Parse zero or more repetitions of a parser without failing if zero matches
    -- Uses 'P.observing' to backtrack: if the parser fails, returns empty list
    -- without consuming input; if it succeeds, recurses to find more
    manyNonFailing :: Parser a -> Parser [a]
    manyNonFailing p = do
      -- Attempt to parse with backtracking on failure
      m <- P.observing (P.try p)
      case m of
        Left _ -> return [] -- parser failed — no more items, return empty list
        Right e -> (:) e <$> manyNonFailing p -- got an item — cons and recurse

-- | Parses either a fully qualified operator name (e.g. @Data.List.++@) or
-- a simple operator identifier (e.g. @++@). Returns the result wrapped in
-- the corresponding 'AST.Identifier' constructor.
--
-- Uses 'P.try' on 'parseOpFQN' to backtrack if the FQN parse fails.
parseOpIdentifier :: Parser AST.Identifier
parseOpIdentifier =
  -- Attempt to parse a fully qualified operator name first
  (AST.IdentifierFullyQualifiedName <$> P.try parseOpFQN)
    -- Fall back to parsing a simple operator identifier
    <|> (AST.IdentifierSimpleIdentifier <$> parseOpSimpleIdentifier)

-- | Parses a term-level identifier usable in infix position. This covers:
--
--   1. Letter-based identifiers surrounded by backticks (e.g. @`elem`@, @`Just`@)
--   2. Operator identifiers (e.g. @++@, @<=@)
--
-- The 'indent_check' argument is used by 'parseInfixLetterIdentifier' to
-- verify proper indentation around the backticks.
parseInfixTermIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseInfixTermIdentifier indent_check =
  -- Attempt to parse a backtick-wrapped letter identifier (with backtracking)
  P.try (parseInfixLetterIdentifier indent_check)
    -- Fall back to parsing an operator identifier
    <|> parseOpIdentifier

-- | Parses a letter-based identifier used in infix position, surrounded
-- by backticks (e.g. @`elem`@, @`notElem`@, @`Just`@). The annotated
-- source positions stored in the AST do __not__ include the backtick
-- characters themselves — only the inner identifier is position-annotated.
--
-- The 'indentCheck' argument verifies that both the opening and closing
-- backticks are at proper indentation levels.
parseInfixLetterIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseInfixLetterIdentifier indentCheck = do
  -- Consume the opening backtick
  _ <- P.single '`'
  -- Verify proper indentation before the opening backtick
  _ <- indentCheck
  -- Parse the inner letter-based identifier (any case, FQN or simple)
  ident <- parseAnyCasedLetterIdentifier
  -- Verify proper indentation after the closing backtick
  _ <- indentCheck
  -- Consume the closing backtick
  _ <- P.single '`'
  -- Return the parsed identifier (backticks are not part of the AST node)
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
parseNonInfixTermIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseNonInfixTermIdentifier indent_check =
  -- Attempt to parse any cased letter identifier first (FQN or simple)
  P.try parseAnyCasedLetterIdentifier
    -- Fall back to parsing a parenthesized operator
    <|> parseNonInfixOpIdentifier indent_check

-- | Parses an operator identifier used in non-infix (prefix) notation,
-- wrapped in parentheses (e.g. @(++)@, @(Data.List.++)@). Essentially
-- 'parseOpIdentifier' surrounded by parentheses.
--
-- The 'indent_check' argument verifies proper indentation of both the
-- opening and closing parentheses.
parseNonInfixOpIdentifier :: Parser MPos.Pos -> Parser AST.Identifier
parseNonInfixOpIdentifier indent_check =
  -- Parse the operator between parentheses, checking indentation after the
  -- opening paren and before the closing paren
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
parseRawString :: Parser Text
parseRawString =
  -- Consume the opening double quote, then parse character literals until
  -- the closing double quote, converting the resulting [Char] to Text
  fmap pack $
    C.char '"' >> manyTill L.charLiteral (C.char '"')

-- | Parses a signed integer literal (with optional leading @+@ or @-@ sign).
-- The result is an unannotated 'Integer' value.
--
-- Uses 'L.signed' with 'sc' (whitespace consumer) for the sign and
-- 'L.decimal' for the decimal integer digits.
--
-- Examples: @42@ (accepted), @-7@ (accepted), @+3@ (accepted)
parseRawInteger :: Parser Integer
parseRawInteger = L.signed sc L.decimal -- optional sign followed by decimal digits

-- | Parses an unsigned natural number literal (no sign allowed).
-- The result is an unannotated 'Natural' value.
--
-- Uses 'L.decimal' to parse non-negative decimal digits.
--
-- Examples: @0@ (accepted), @123@ (accepted), @-5@ (rejected — no sign allowed)
parseRawNatural :: Parser Natural
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

-- | The set of valid Haskell operator characters.
--
--   This is the union of:
--
--   * Symbol characters: @:!#$%&*+./<=>?@\\^|-~@
--   * The colon character @:@ (used for constructors like @:Cons@)
--
--   Used by 'parseRawOpChar' and 'keywordOp' to recognize operator tokens.
opChars :: [Char]
opChars = ":!#$%&*+./<=>?@\\^|-~"

-- | Reserved keywords that cannot be used as identifier names.
--
--   If a raw identifier string matches any entry in this list, the
--   'parseRawLowerHsIdentifierString' and 'parseRawAnyCaseHsIdentifierString'
--   parsers will reject it with a parse error.
--
--   Currently contains all Haskell keywords
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
  , "as"
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
--   Currently contains: @\"=\"@
reservedOps :: [Text]
reservedOps = ["="]

-- | Parses a keyword string, ensuring it is not immediately followed by
-- identifier-continuation characters (alphanumeric, underscore, apostrophe).
--
-- This guard prevents partial keyword matches — for example, parsing the
-- keyword @if@ will reject input like @ifdef@ because the 'd' after @if@
-- is a valid identifier continuation character.
keyword :: Text -> Parser Text
keyword s = do
  -- Consume the keyword string
  x <- C.string s
  -- Ensure the keyword is not followed by identifier-continuation characters
  P.notFollowedBy (C.alphaNumChar <|> C.char '_' <|> C.char '\'')
  -- Return the parsed keyword string
  return x

-- | Parses a keyword operator string, ensuring it is not immediately followed
-- by another operator character.
--
-- This guard prevents partial operator matches — for example, parsing the
-- operator @<=@ will reject input like @<=<@ because the trailing '<'
-- is a valid operator character.
keywordOp :: Text -> Parser Text
keywordOp s = do
  -- Consume the operator string
  x <- C.string s
  -- Ensure the operator is not followed by another operator character
  P.notFollowedBy parseRawOpChar
  -- Return the parsed operator string
  return x

-- | Parses a turnstile symbol used in Fixen's sequent notation.
-- Accepts either the Unicode turnstile (@⊢@) or the ASCII variant (@|-@).
--
-- Uses 'P.try' on the Unicode variant to backtrack if it fails, then
-- falls through to the ASCII variant.
turnstile :: Parser Text
turnstile =
  -- Attempt the Unicode turnstile first (with backtracking)
  P.try (keywordOp "⊢")
    -- Fall back to the ASCII turnstile variant
    <|> keywordOp "|-"

-- | Parses a less-than-or-equal symbol used for partial order relations.
-- Accepts either the ASCII less-than sign (@<@) or the Unicode
-- subset-of-or-equal symbol (@⊏@).
--
-- Uses 'P.try' on the ASCII variant to backtrack if it fails, then
-- falls through to the Unicode variant.
ltOrSqSubsetEq :: Parser Text
ltOrSqSubsetEq =
  -- Attempt the ASCII less-than first (with backtracking)
  P.try (keywordOp "<")
    -- Fall back to the Unicode subset-of-or-equal symbol
    <|> keywordOp "⊏"
