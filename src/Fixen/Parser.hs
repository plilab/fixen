{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.Parser
-- Description : Parser for Fixen programs
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides the top-level parser entry point for Fixen programs.
-- It coordinates the parsing of all program constructs — module declarations,
-- imports, extern declarations, relations, rules, priority declarations,
-- queries, partial order declarations, include statements, Haskell code
-- blocks, and phase declarations — into a complete 'Program'.
--
-- The main entry point is 'parse', which takes a file path and contents
-- and returns a fully constructed 'Program'. Internally, it:
--
-- 1. Runs 'parseProgram' to get a module declaration and a flat list of
--    top-level declarations ('TopLevel')
-- 2. Calls 'partitionTopLevels' to distribute those declarations into the
--    appropriate fields of 'Program', producing the final result
--
-- @since 26.7
module Fixen.Parser where

import Control.Applicative.Combinators (
  some,
  (<|>),
 )
import Control.Monad.State.Strict qualified as State
import Data.List
import Data.List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Set qualified as Set
import Data.Text (Text, pack, unpack)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad
import Fixen.Parser.Common
import Fixen.Parser.Expr
import Fixen.Parser.Token
import Fixen.Parser.Type
import Text.Megaparsec (eof)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))

--------------------------------------------------------------------------------

-- * Main entry point

--------------------------------------------------------------------------------

-- | Parses a complete Fixen program from raw file contents.
--
--   This is the top-level entry point for parsing. It:
--
--   1. Runs 'parseProgram' to extract the module declaration and a flat list
--      of top-level declarations ('TopLevel')
--   2. Calls 'partitionTopLevels' to distribute those declarations into the
--      appropriate fields of 'Program'
--
--   The result is a fully constructed 'Program' with all declarations
--   organized into their respective categories (relations, rules, priorities,
--   etc.).
--
--   If any parse errors occur, they are accumulated and reported via the
--   'FixenPass' monad.
--
-- @since 26.7
parse
  :: ParserState σ
  => FilePath
  -- ^ The file path of the program (used for error reporting)
  -> Text
  -- ^ The contents of the file to parse
  -> FixenPass σ Program
parse file_path file_contents = do
  -- Parse the module declaration and top-level declarations
  (mod_decl, top_levels) <- fixenParse parseProgram file_path file_contents
  -- Distribute the flat list of top-level declarations into the
  -- appropriate fields of AST.Program (relations, rules, etc.)
  partitionTopLevels mod_decl top_levels

--------------------------------------------------------------------------------

-- * Running parsers in FixenM

--------------------------------------------------------------------------------

-- | Runs a 'Parser' in the 'FixenPass' monad, converting parse errors
-- into 'FixenPass' failures.
--
-- This function:
--
-- 1. Captures the current 'ParserState' (including error accumulator)
-- 2. Runs the Megaparsec parser via 'P.runParserT'
-- 3. Restores the updated state (including any position tracking)
-- 4. On success, returns the parsed value
-- 5. On failure, converts the Megaparsec error bundle into a 'Diagnostic'
--    and fails the pass via 'failD'
--
-- This is the bridge between the Megaparsec parser layer and the
-- 'FixenPass' compiler pass layer.
--
-- @since 26.7
fixenParse
  :: ParserState σ
  => Parser σ a
  -- ^ The 'Parser' to run
  --
  -- @since 26.7
  -> FilePath
  -- ^ The file path of the program (used as the Megaparsec source name)
  --
  -- @since 26.7
  -> Text
  -- ^ The contents of the file to parse
  --
  -- @since 26.7
  -> FixenPass σ a
fixenParse parser file_path file_contents = do
  -- Capture the current parser state (position env, node ID counter, errors)
  st <- State.get
  -- Run the Megaparsec parser with the given file path and contents
  let e = P.runParserT parser file_path file_contents
  -- Extract the result and updated state from the parser monad stack
  let (r, st') = State.runState e st
  -- Restore the updated state (including position tracking and node IDs)
  State.put st'
  -- Handle success or failure
  case r of
    Right p -> return p -- success — return the parsed value
    Left p ->
      -- Parse failure — convert Megaparsec error bundle to a Diagnostic
      -- and fail the pass with it
      failD $ errorDiagnosticFromBundle Nothing "syntax error" Nothing p

--------------------------------------------------------------------------------
--

-- * Program parsing

--
--------------------------------------------------------------------------------

-- | Parses a complete Fixen program: a module declaration followed by
-- zero or more top-level declarations, with mandatory end-of-file.
--
-- Returns a tuple of:
--
-- * 'ModuleDeclaration' — the parsed module declaration
-- * ['TopLevel'] — the flat list of all top-level declarations found
--   (before partitioning into 'Program' fields)
--
-- The 'eof' parser ensures that the entire file is consumed; any trailing
-- content after the last declaration will cause a parse error.
--
-- @since 26.7
parseProgram :: ParserState σ => Parser σ (ModuleDeclaration, [TopLevel])
parseProgram = do
  -- Parse the module declaration (e.g. "module Foo where") and consume trailing whitespace
  mod_head <- l parseModuleDeclaration
  -- Parse all remaining top-level declarations, consuming whitespace after each
  top_levels <- l parseTopLevels
  -- Ensure no trailing content after the last declaration
  _ <- eof
  return (mod_head, top_levels)

-- | Parses a list of top-level declarations. Each declaration is one of:
--
-- * @extern@ declarations
-- * @rel@ (relation) declarations
-- * @rule@ declarations
-- * @partial ord@ declarations
-- * @priority@ declarations
-- * @query@ declarations
-- * @include@ statements
-- * @import@ statements
-- * Haskell code blocks (```hs ... ```)
-- * @phases@ declarations
--
-- Each item is parsed independently with whitespace consumed after it.
-- At least one top-level declaration is required.
--
-- @since 26.7
parseTopLevels :: ParserState σ => Parser σ [TopLevel]
parseTopLevels =
  -- Parse one or more top-level declarations, consuming whitespace after each
  some $
    l $
      -- Try each declaration type in order; the first match wins
      (TLRelation <$> parseRelation)
        <|> (TLRule <$> parseRule)
        <|> (TLPartialOrd <$> parsePartialOrd)
        <|> (TLLattice <$> parseLattice)
        <|> (TLPriority <$> parsePriority)
        <|> (TLQuery <$> parseQuery)
        <|> (TLInclude <$> parseInclude)
        <|> (TLImport <$> parseImport)
        <|> (TLHsBlock <$> parseHaskellCodeBlock)
        <|> (TLPhases <$> parsePhases)

-- | Represents a top-level declaration in a Fixen program.
--
--   Each constructor wraps the corresponding AST type. The 'TopLevel' type
--   is used during the initial parsing phase before declarations are
--   partitioned into the structured 'Program' type.
--
-- @since 26.7
data TopLevel
  = -- | A @rel@ declaration defining a relation (fact type)
    --
    -- @since 26.7
    TLRelation RelationDeclaration
  | -- | A @rule@ declaration defining inference rules
    --
    -- @since 26.7
    TLRule Rule
  | -- | A @partial ord@ declaration defining a partial order on a type
    --
    -- @since 26.7
    TLPartialOrd PartialOrdDeclaration
  | -- | A @lattice@ declaration defining a lattice on a type
    --
    -- @since 26.7
    TLLattice LatticeDeclaration
  | -- | A Haskell code block (```hs … ```)
    --
    -- @since 26.7
    TLHsBlock HsBlock
  | -- | A @priority@ declaration defining ordering between rule instances
    --
    -- @since 26.7
    TLPriority Priority
  | -- | A @query@ declaration specifying a query mode for a relation
    --
    -- @since 26.7
    TLQuery Query
  | -- | An @include@ statement importing another Fixen file
    --
    -- @since 26.7
    TLInclude Include
  | -- | An @import@ statement importing a Haskell module
    --
    -- @since 26.7
    TLImport HsImport
  | -- | A @phases@ declaration defining multi-phase rule execution
    --
    -- @since 26.7
    TLPhases PhasesDeclaration

-- | Distributes a flat list of 'TopLevel' declarations into the structured
-- 'Program' type, placing each declaration into its appropriate field.
--
--   This function processes the list in order, accumulating declarations
--   into the correct 'Program' fields. It also performs validation:
--
--   * /Multiple extern declarations/ — produces a /warning/ (the last one wins)
--   * /Multiple phase declarations/ — produces a /fatal error/ (only one allowed)
--
--   The result is an 'Program' with all fields populated from the
--   parsed top-level declarations.
--
-- @since 26.7
partitionTopLevels :: ParserState σ => ModuleDeclaration -> [TopLevel] -> FixenPass σ Program
partitionTopLevels mod_decl [] =
  -- Base case: no more top-level declarations — return Program with
  -- all optional fields set to empty/Nothing
  return
    Program
      { programHsBlocks = []
      , programPriorities = []
      , programQueries = []
      , programModuleName = mod_decl
      , programImports = []
      , programRelationDeclarations = []
      , programRules = []
      , programIncludes = []
      , programPhases = Nothing
      , programPartialOrdDeclarations = []
      , programLatticeDeclarations = []
      }
partitionTopLevels mod_decl (x : xs) = do
  -- Recursively partition the rest of the list
  rest <- partitionTopLevels mod_decl xs
  -- Place the current declaration into the correct field
  case x of
    TLRelation r -> return $ rest & relationDeclarations %~ (r :)
    TLRule r -> return $ rest & rules %~ (r :)
    TLPartialOrd po -> return $ rest & partialOrdDeclarations %~ (po :)
    TLPriority p -> return $ rest & priorities %~ (p :)
    TLQuery q -> return $ rest & queries %~ (q :)
    TLInclude i -> return $ rest & includes %~ (i :)
    TLImport i -> return $ rest & imports %~ (i :)
    TLLattice i -> return $ rest & latticeDeclarations %~ (i :)
    TLPhases p ->
      -- Phases: validate that there's at most one (fatal error if multiple)
      case programPhases rest of
        Nothing -> return rest {programPhases = Just p} -- first phases — just add it
        Just p' -> do
          -- multiple phases — fatal error
          p_pos <- getPosition p
          p'_pos <- getPosition p'
          failErr
            Nothing
            "syntax error"
            [ (p_pos, Where "a phase definition")
            , (p'_pos, This "another phase definition")
            ]
            [Note "each program can only have one phase declaration"]
    TLHsBlock h -> return $ rest & hsBlocks %~ (h :)

--------------------------------------------------------------------------------

-- * Individual declaration parsers

--------------------------------------------------------------------------------

-- ** Module-Declaration Parser

-- | Parses a module declaration: @module My.Haskell.Module where@.
--
-- The module name is parsed as a 'ModuleName' (a series of capitalized
-- identifiers separated by dots). The @module@ keyword must
-- not be indented.
--
-- This declaration is compulsory in every Fixen program.
--
-- @since 26.7
parseModuleDeclaration :: ParserState σ => Parser σ ModuleDeclaration
parseModuleDeclaration = parsePositioned $ do
  -- Parse the 'module' keyword (must not be indented)
  -- No need to 'try' here — module declarations are compulsory
  _ <- l $ L.nonIndented sc $ keyword "module"
  -- Parse the module name (e.g. Data.List)
  m <- indented *> l parseModuleName
  -- Parse the 'where' keyword
  _ <- indented *> keyword "where"
  -- Allocate a fresh node ID and construct the ModuleDeclaration AST node
  i <- getNewNodeId
  return $ ModuleDeclaration i m

-- ** Relation-Declaration Parser

-- | Parse a named relation parameter of the form @(name: Type)@.
--
-- @since 0.0.1
parseNamedRelationParameter
  :: ParserState σ
  => Parser σ RelationParameter
parseNamedRelationParameter = parsePositioned $ do
  _ <- indented
  (parameterName, parameterType) <-
    betweenParentheses indented $ do
      parameterName <- parseLowerFirstSimpleIdentifier
      _ <- indented *> keywordOp ":"
      _ <- indented
      parameterType <- parseType indented
      return (parameterName, parameterType)
  parameterId <- getNewNodeId
  return $ RelationParameter parameterId (Just parameterName) parameterType

-- | Parse an unnamed relation parameter consisting only of a type.
--
-- @since 0.0.1
parseUnnamedRelationParameter
  :: ParserState σ
  => Parser σ RelationParameter
parseUnnamedRelationParameter = parsePositioned $ do
  parameterType <- parseType indented
  parameterId <- getNewNodeId
  return $ RelationParameter parameterId Nothing parameterType

-- | Parse either a named or unnamed relation parameter.
--
-- The named form is tried first because it and an ordinary parenthesized type
-- both begin with @(@. Backtracking preserves existing positional syntax such
-- as @(Maybe Int)@.
--
-- @since 0.0.1
parseRelationParameter
  :: ParserState σ
  => Parser σ RelationParameter
parseRelationParameter =
  P.try parseNamedRelationParameter
    <|> parseUnnamedRelationParameter

-- | Parses a relation declaration:
--
-- @
-- rel RelationName: Type1, (parameterName: Type2)
-- @
--
-- The @rel@ keyword must not be indented. The relation name must be
-- capitalized (since relations are constructor-like). Arguments are
-- optional — if present, they follow a colon and are comma-separated. Each
-- parameter may independently have a name, so named and unnamed parameters
-- may be mixed.
--
-- Example without arguments:
--
-- @
-- rel MyFact
-- @
--
-- Example with arguments:
--
-- @
-- rel Dist: Integer, Integer
-- rel Counter: (label: String), (num: Int)
-- rel Mixed: String, (num: Int)
-- @
--
-- @since 26.7
parseRelation :: ParserState σ => Parser σ RelationDeclaration
parseRelation = parsePositioned $ do
  -- Parse the 'rel' keyword (must not be indented)
  -- Use 'P.try' so we can backtrack when parsing top-level declarations
  _ <- P.try $ L.nonIndented sc $ keyword "rel"
  -- Verify proper indentation before the relation name
  _ <- indented
  -- Parse the capitalized relation name (relations are constructor-like)
  rel_name <- parseCapitalizedSimpleIdentifier
  -- Attempt to parse optional arguments: a colon followed by comma-separated types
  -- 'P.observing' allows backtracking — if no colon, args are empty
  colon <- P.observing $ P.try $ indented *> keywordOp ":"
  params <- case colon of
    Left _ -> return [] -- no colon — no arguments
    Right _ -> do
      -- Verify proper indentation before the first argument
      _ <- indented
      -- Parse one or more comma-separated types with indentation checking
      params <- commaSepBy1' parseRelationParameter
      return $ toList params -- convert NonEmpty list to regular list
      -- Allocate a fresh node ID and construct the Relation AST node
  i <- getNewNodeId
  return $ RelationDeclaration i rel_name params

-- ** Rule-Declaration Parsers

-- | Parses a rule declaration:
--
-- @
-- rule [ruleName boundVar1 boundVar2 ...]:
--     assumption1
--   , assumption2
--   , if condition
--  |- conclusion
-- @
--
-- The @rule@ keyword must not be indented. A rule consists of:
--
-- 1. An optional rule name (lowercase identifier) followed by optional
--    parameters
-- 2. A colon @:@
-- 3. Zero or more premises (assumptions and conditions), separated by commas
-- 4. A turnstile (@|-@ or @⊢@)
-- 5. A conclusion (a capitalized fact name followed by expression arguments)
--
-- The turnstile (@|-@) is a keyword operator, not a regular operator,
-- so it can be used in expressions only when parenthesized.
-- For example: @if (a |- b) |- Fact a (b |- a)@
--
-- @since 26.7
parseRule :: ParserState σ => Parser σ Rule
parseRule = parsePositioned $ do
  -- Parse the 'rule' keyword (must not be indented)
  -- Definitely need 'try' here since rules are among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "rule"
  -- Verify proper indentation before the rule name
  _ <- indented
  -- Parse the optional rule name and bound variables (all lowercase identifiers)
  -- If no identifiers are found, the rule is unnamed with no bound variables
  (rule_name, bound_vars) <- do
    idents <- manyI' parseLowerFirstSimpleIdentifier
    case idents of
      [] -> return (Nothing, []) -- no identifiers — unnamed rule
      (x : xs) -> return (Just x, xs) -- first is name, rest are bound variables
      -- Parse the colon separator with indentation checks on both sides
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the premises (assumptions and conditions), separated by commas
  -- 'partitionPremises' separates them into assumptions and conditions
  (asms, conds) <- do
    premises <- commaSepBy' parsePremise
    return $ partitionPremises premises
  -- Parse the turnstile (@|-@ or @⊢@) with indentation checks
  _ <- indented *> turnstile *> indented
  -- Parse the conclusion (capitalized fact name + expression arguments)
  concl <- parseConclusion
  -- Allocate a fresh node ID and construct the Rule AST node
  i <- getNewNodeId
  return $ Rule i rule_name bound_vars asms conds concl

-- | Represents a premise within a rule body.
--
-- A premise is either:
--
-- * An /assumption/ — a relation applied to variable arguments
--   (e.g. @MyFact a b@)
-- * A /condition/ — an expression guarded by the 'if' keyword
--   (e.g. @if a <= b@)
--
-- The 'partitionPremises' function separates a flat list of 'RulePremise'
-- values into two groups: assumptions first, then conditions.
--
-- @since 26.7
data RulePremise
  = RPAssumption Assumption
  | RPCondition Condition

-- | Separates a list of 'RulePremise' values into assumptions and conditions.
--
-- @since 26.7
partitionPremises
  :: [RulePremise]
  -> ([Assumption], [Condition])
partitionPremises [] = ([], []) -- base case: empty list
partitionPremises (x : xs) =
  -- Recurse on the tail, then prepend the current premise to the appropriate group
  let (as, cs) = partitionPremises xs
   in case x of
        RPAssumption a -> (a : as, cs) -- add to assumptions
        RPCondition c -> (as, c : cs) -- add to conditions

-- | Parses a single premise within a rule body. A premise is either:
--
-- * An /assumption/ — a relation applied to variable arguments
--   (e.g. @MyFact a b@)
-- * A /condition/ — an expression guarded by the 'if' keyword
--   (e.g. @if a <= b@)
--
-- No 'try' is used here because the first token of each branch is
-- distinct: assumptions start with a capitalized identifier, while
-- conditions start with the 'if' keyword. Once one matches, we commit
-- to it.
--
-- @since 26.7
parsePremise :: ParserState σ => Parser σ RulePremise
parsePremise =
  -- Try assumption first (starts with capitalized identifier)
  RPAssumption <$> parseAssumption
    -- Fall back to condition (starts with 'if' keyword)
    <|> RPCondition <$> parseCondition

-- | Parses the conclusion of a rule: a capitalized fact name followed by
-- zero or more expression arguments.
--
-- @
-- MyFact arg1
--    arg2
-- @
--
-- The conclusion must start with a capitalized identifier (since facts
-- are constructor-like), followed by expression arguments.
--
-- @since 26.7
parseConclusion :: ParserState σ => Parser σ Conclusion
parseConclusion = parsePositioned $ do
  -- Verify proper indentation before the conclusion
  _ <- indented
  -- Parse the capitalized fact name (conclusions are constructor-like)
  header <- parseCapitalizedSimpleIdentifier
  -- Verify proper indentation before the arguments
  -- _ <- indented
  -- Parse zero or more expression arguments with indentation checking
  arguments <- manyI' (parseParenExpr indented)
  -- Allocate a fresh node ID and construct the Relation AST node
  i <- getNewNodeId
  return $ Conclusion i header arguments

-- | Parses an assumption within a rule body: a relation name applied to
-- variable arguments.
--
-- @
-- MyFact var1
--  var2
-- @
--
-- The relation name must be capitalized (since relations are
-- constructor-like), and the arguments must be lowercase-starting
-- simple identifiers.
--
-- A 'try' is used here so that if the premise is actually a condition
-- (starting with 'if'), we can backtrack and try 'parseCondition' instead.
--
-- @since 26.7
parseAssumption :: ParserState σ => Parser σ Assumption
parseAssumption = parsePositioned $ do
  -- Verify proper indentation before the assumption
  _ <- indented
  -- Parse the capitalized relation name
  -- Use 'P.try' so we can backtrack if this is actually a condition
  header <- P.try parseCapitalizedSimpleIdentifier
  -- Verify proper indentation before the arguments
  _ <- indented
  -- Parse zero or more lowercase-starting simple identifiers as arguments.
  -- This is the **only** place that holes are accepted.
  arguments <- manyI' parseLowerFirstSimpleIdentifierOrHole
  -- Allocate a fresh node ID and construct the Assumption AST node
  i <- getNewNodeId
  return $ Assumption i header arguments

-- | Parses a condition within a rule body: an expression guarded by
-- the @if@ keyword.
--
-- @
-- if a <= b
-- @
--
-- The @if@ keyword must not be immediately followed by identifier
-- continuation characters (guaranteed by 'keyword'). The expression
-- is parsed with indentation checking.
--
-- @since 26.7
parseCondition :: ParserState σ => Parser σ Condition
parseCondition = parsePositioned $ do
  -- Parse the 'if' keyword with indentation checks on both sides
  _ <-
    indented
      *> keyword "if"
      *> indented
  -- Parse the condition expression with indentation checking
  e <- parseExpr indented
  -- Allocate a fresh node ID and construct the Condition AST node
  i <- getNewNodeId
  return $ Condition i e

-- ** Partial-Order-Declaration Parsers

-- | Parses a partial order declaration:
--
-- @
-- partial ord Dist
--     where
--     type = Dist
--     leq = (<=)
--     mlbs = meet
-- @
--
-- This defines a partial order (reflexive, transitive, antisymmetric
-- relation) on a given type. It specifies:
--
-- * The base type (@type@ field)
-- * The less-than-or-equal function (@leq@ field)
-- * The maximal lower bounds function (@mlbs@ field)
--
-- The @partial@ keyword must not be indented. The @where@
-- block contains exactly the three fields above, in that order.
--
-- @since 26.7
parsePartialOrd :: ParserState σ => Parser σ PartialOrdDeclaration
parsePartialOrd = parsePositioned $ do
  -- Parse the 'partial' keyword (must not be indented)
  -- Need 'try' since partial ord is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "partial"
  -- Verify proper indentation before 'ord'
  _ <- indented
  -- Parse the 'ord' keyword
  _ <- keyword "ord"
  -- Verify proper indentation before the type name
  _ <- indented
  -- Parse the type name being defined (e.g. Dist)
  pord_name <- parseCapitalizedSimpleIdentifier
  -- Verify proper indentation before 'where'
  _ <- indented
  -- Parse the 'where' keyword
  _ <- keyword "where"
  -- Verify proper indentation before the fields
  _ <- indented
  -- Parse the three required fields: type, leq, mlbs
  -- Each field must be indented relative to the 'partial ord' line
  type_expr <- parsePartialOrdField "type" (parseType indented)
  leq_func <- parsePartialOrdField "leq" (parseNonInfixTermIdentifier indented)
  mlbs_func <- parsePartialOrdField "mlbs" (parseNonInfixTermIdentifier indented)
  -- Allocate a fresh node ID and construct the PartialOrdDeclaration AST node
  i <- getNewNodeId
  return $ PartialOrdDeclaration i pord_name type_expr leq_func mlbs_func

-- | Parses a single field within a @partial ord@ declaration.
--
-- Each field has the form: @fieldName = value@, where:
--
-- * @fieldName@ is one of @type@, @leq@, or @mlbs@
-- * @value@ is parsed by the provided 'valueParser'
-- * Proper indentation is verified before and after each component
--
-- For example, the field @type = Dist@ is parsed as:
--
-- @
-- keyword "type"  ->  keywordOp "="  ->  parseType indented
-- @
--
-- @since 26.7
parsePartialOrdField
  :: ParserState σ
  => Text
  -- ^ Field name (@type@, @leq@, or @mlbs@)
  --
  -- @since 26.7
  -> Parser σ a
  -- ^ Parser for the field value
  --
  -- @since 26.7
  -> Parser σ a
parsePartialOrdField fieldName valueParser = do
  -- Parse: fieldName = value, with indentation checks between all components
  _ <- indented *> keyword fieldName *> indented *> keywordOp "=" *> indented
  -- Parse the value (type expression, identifier, etc.)
  valueParser

-- | Parses a lattice declaration:
--
-- @
-- lat Dist
--     where
--     type = Dist
--     leq = (<=)
--     join = joinD
--     meet = meetD
-- @
--
-- This defines a lattice on a given type. It specifies:
--
-- * The base type (@type@ field)
-- * The less-than-or-equal function (@leq@ field)
-- * The join function (@join@ field)
-- * The meet function (@meet@ field)
--
-- The @lat@ keyword must not be indented. The @where@
-- block contains exactly the three fields above, in that order.
--
-- @since 26.7
parseLattice :: ParserState σ => Parser σ LatticeDeclaration
parseLattice = parsePositioned $ do
  -- Parse the 'partial' keyword (must not be indented)
  -- Need 'try' since partial ord is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "lat"
  -- Verify proper indentation before the type name
  _ <- indented
  -- Parse the type name being defined (e.g. Dist)
  pord_name <- parseCapitalizedSimpleIdentifier
  -- Verify proper indentation before 'where'
  _ <- indented
  -- Parse the 'where' keyword
  _ <- keyword "where"
  -- Verify proper indentation before the fields
  _ <- indented
  -- Parse the three required fields: type, leq, mlbs
  -- Each field must be indented relative to the 'partial ord' line
  type_expr <- parsePartialOrdField "type" (parseType indented)
  leq_func <- parsePartialOrdField "leq" (parseNonInfixTermIdentifier indented)
  join_func <- parsePartialOrdField "join" (parseNonInfixTermIdentifier indented)
  meet_func <- parsePartialOrdField "meet" (parseNonInfixTermIdentifier indented)
  -- Allocate a fresh node ID and construct the PartialOrdDeclaration AST node
  i <- getNewNodeId
  return $ LatticeDeclaration i pord_name type_expr leq_func join_func meet_func

-- ** Priority-Declaration Parsers

-- | Parses a priority declaration:
--
-- @
-- priority: a <= b |- addDist { a = a, b = b' } <= addDist { a = a', b = b' }
-- @
--
-- A priority declaration specifies an ordering between two rule instances.
-- It consists of:
--
-- 1. The @priority@ keyword (must not be indented)
-- 2. A colon separator
-- 3. A premise expression (left side of the turnstile)
-- 4. A turnstile (@|-@ or @⊢@)
-- 5. A priority conclusion (two rule instances connected by @<=@ or @⊏@)
--
-- Each priority declaration applies to exactly one priority rule. For
-- multiple rules, create multiple priority declarations. This avoids
-- indentation ambiguity.
--
-- @since 26.7
parsePriority :: ParserState σ => Parser σ Priority
parsePriority = parsePositioned $ do
  -- Parse the 'priority' keyword (must not be indented)
  -- Need 'try' since priority is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "priority"
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the premise expression (left side of the turnstile)
  prem <- parseExpr indented
  -- Parse the turnstile with indentation checks
  _ <- indented *> turnstile *> indented
  -- Parse the priority conclusion (two rule instances with ordering)
  concl <- parsePriorityConclusion
  -- Allocate a fresh node ID and construct the Priority AST node
  i <- getNewNodeId
  return $ Priority i prem concl

-- | Parses the conclusion of a priority declaration: two rule instances
-- connected by an ordering symbol (@<=@ or @⊏@).
--
-- @
-- addDist { a = a } <= addDist { a = a' }
-- @
--
-- Each rule instance consists of a rule name (lowercase identifier)
-- followed by optional variable substitutions in curly braces.
--
-- @since 26.7
parsePriorityConclusion :: ParserState σ => Parser σ PriorityConclusion
parsePriorityConclusion = parsePositioned $ do
  -- Parse the left-hand side rule instance
  left <- parseRuleInstance
  -- Parse the ordering symbol (<= or ⊏) with indentation checks
  _ <- indented *> ltOrSqSubsetEq *> indented
  -- Parse the right-hand side rule instance
  right <- parseRuleInstance
  -- Allocate a fresh node ID and construct the PriorityConclusion AST node
  i <- getNewNodeId
  return $ PriorityConclusion i left right

-- | Parses a rule instance: a rule name optionally followed by variable
-- substitutions in curly braces.
--
-- @
-- addDist { a = a, b = b' }
-- assignI { }
-- myRule
-- @
--
-- The rule name is a lowercase-starting simple identifier. Substitutions
-- are comma-separated pairs of identifiers separated by @=@.
--
-- @since 26.7
parseRuleInstance :: ParserState σ => Parser σ RuleInstance
parseRuleInstance = parsePositioned $ do
  -- record the offset so as to throw errors
  offset_start <- P.getOffset
  -- Parse the rule name (lowercase identifier), consuming trailing whitespace
  rule_name <- l parseLowerFirstSimpleIdentifier
  -- Parse optional variable substitutions in curly braces
  -- (empty braces {} are allowed)
  subst <- betweenCurlyBraces indented $ commaSepBy' parseSubstitution
  let keys = simpleIdentifier <$> fst <$> subst
      key_ct = Map.keys $ Map.filter (> (1 :: Int)) $ foldl' (\m t -> Map.insert t ((Map.findWithDefault 0 t m) + 1) m) Map.empty keys
  if not (null key_ct)
    then
      P.parseError
        ( P.FancyError
            offset_start
            ( Set.singleton
                ( ErrorFail $
                    "duplicate rule parameter instantiations: "
                      ++ Data.List.intercalate ", " (unpack <$> key_ct)
                )
            )
        )
    else do
      -- Allocate a fresh node ID and construct the RuleInstance AST node
      i <- getNewNodeId
      return $ RuleInstance i rule_name (Map.fromList subst)

-- | Parses a variable substitution: @left = right@, where both sides
-- are lowercase-starting simple identifiers.
--
-- @
-- a = a'
-- @
--
-- Used within rule instance declarations to specify how variables
-- are mapped.
--
-- @since 26.7
parseSubstitution :: ParserState σ => Parser σ (SimpleIdentifier, SimpleIdentifier)
parseSubstitution = do
  -- Parse the left-hand side identifier
  left <- parseLowerFirstSimpleIdentifier
  -- Parse the '=' operator with indentation checks on both sides
  _ <- indented *> keywordOp "=" *> indented
  -- Parse the right-hand side identifier
  right <- parseLowerFirstSimpleIdentifier
  return (left, right)

-- ** Query-Declaration Parsers

-- | Parses a query declaration:
--
-- @
-- query DistTo as distTo - +
-- @
--
-- A query declaration specifies a query mode for a relation. It consists of:
--
-- 1. The @query@ keyword (must not be indented)
-- 2. A query name (lowercase identifier)
-- 3. A colon
-- 4. A relation name (capitalized identifier)
-- 5. Some number of query mode symbols (@+@ for input, @-@ for output)
--
-- Modes indicate whether each argument of the relation is an input or
-- output variable for the query.
--
-- @since 26.7
parseQuery :: ParserState σ => Parser σ Query
parseQuery = parsePositioned $ do
  -- Parse the 'query' keyword (must not be indented)
  -- Need 'try' since query is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "query"
  -- Verify proper indentation before the relation name
  _ <- indented
  -- Parse the query name (lowercase identifier)
  query_name <- parseLowerFirstSimpleIdentifier
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the relation with modes
  rel <- parseQueriedRelation
  -- Allocate a fresh node ID and construct the Query AST node
  i <- getNewNodeId
  return $ Query i rel query_name

-- | Parses the relation part within a query declaration: a relation name
-- applied to 'QueryMode's.
--
-- @
-- MyFact - +
-- @
--
-- The relation name must be capitalized (since relations are
-- constructor-like), and the arguments must be query modes.
--
-- @since 26.7
parseQueriedRelation :: ParserState σ => Parser σ QueriedRelation
parseQueriedRelation = parsePositioned $ do
  -- Verify proper indentation before the assumption
  _ <- indented
  -- Parse the capitalized relation name
  -- Use 'P.try' so we can backtrack if this is actually a condition
  header <- P.try parseCapitalizedSimpleIdentifier
  -- Parse zero or more lowercase-starting simple identifiers as arguments
  modes <- manyI' parseQueryMode
  -- Allocate a fresh node ID and construct the Assumption AST node
  i <- getNewNodeId
  return $ QueriedRelation i header modes

-- | Parses a single mode symbol: @+@ (input) or @-@ (output).
--
-- Mode symbols indicate whether a relation argument is an input or
-- output variable for a query.
--
-- * @+@ → 'Input' — the argument is an input
-- * @-@ → 'Output' — the argument is an output
--
-- @since 26.7
parseQueryMode :: ParserState σ => Parser σ QueryMode
parseQueryMode = parsePositioned $ do
  -- Parse the mode symbol with indentation check, consuming trailing whitespace
  m <- indented *> P.try (C.char '+' >> return Input) <|> (C.char '-' >> return Output)
  -- Allocate a fresh node ID and wrap in the QueryMode constructor
  i <- getNewNodeId
  return $ m i

-- ** Include-Statement Parser

-- | Parses an include statement:
--
-- @
-- include \"path\/to\/Fixen.fix\"
-- @
--
-- The @include@ keyword must not be indented. The path is a string
-- literal (double-quoted, with escape sequences supported).
--
-- Include statements allow one Fixen program to import and reuse
-- declarations from another Fixen file.
--
-- @since 26.7
parseInclude :: ParserState σ => Parser σ Include
parseInclude = parsePositioned $ do
  -- Parse the 'include' keyword (must not be indented)
  -- Need 'try' since include is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "include"
  -- Verify proper indentation before the path string
  _ <- indented
  -- Parse the file path as a string literal
  include_path <- parseRawString
  -- Allocate a fresh node ID and construct the Include AST node
  i <- getNewNodeId
  return $ Include i include_path

-- ** Explicit Import Specifications Parser

--

-- | Parses the specifications in an explicit import
--
-- @
-- import Data.Map (Map)
-- @
parseImportSpecs :: Parser σ Text
parseImportSpecs = pack <$> parens
  where
    -- Convert String returned from parens back into Text

    parens = do
      -- Parse the opening parens
      _ <- C.char '('
      -- Parse chunks until the closing parens is hit
      chunks <- P.manyTill chunk (C.char ')')
      -- Return the resulting string enclosed by parens
      return $ "(" <> concat chunks <> ")"

    chunk =
      -- Try to parse a nested parens group
      P.try parens
        -- Otherwise, parse a character that is not ')', and convert it to a single element list
        <|> ((: []) <$> P.satisfy (/= ')'))

-- ** Import-Statement Parser

-- | Parses an import statement:
--
-- @
-- import My.Haskell.Module
-- @
--
-- The @import@ keyword must not be indented. The module name is parsed
-- as a 'ModuleName' (a series of capitalized identifiers separated
-- by dots).
--
-- Import statements allow a Fixen program to reference Haskell symbols
-- defined in external modules.
--
-- @since 26.7
parseImport :: ParserState σ => Parser σ HsImport
parseImport = parsePositioned $ do
  -- Parse the 'import' keyword (must not be indented)
  -- Need 'try' since import is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "import"
  -- Verify proper indentation before the qualified keyword
  _ <- indented

  -- Parse the 'qualified' keyword if it exists
  qualifiedImport <-
    either (const False) (const True)
      <$> P.observing (P.try (l $ keyword "qualified"))

  -- Verify proper indentation before the module name
  _ <- indented

  -- Parse the Haskell module name (e.g. Data.List, MyCompany.MyModule)
  mod_name <- parseModuleName

  -- Parse the import alias if it exists
  alias <-
    either (const Nothing) Just
      <$> P.observing
        ( P.try $ do
            _ <- indented
            _ <- l $ keyword "as"
            _ <- indented
            parseModuleName
        )

  -- Parse explicit import specifications if it exists
  importSpecs <-
    either (const Nothing) Just
      <$> P.observing
        ( P.try $ do
            _ <- indented
            parseImportSpecs
        )

  -- Allocate a fresh node ID and construct the HsImport AST node
  i <- getNewNodeId
  return $ HsImport i mod_name qualifiedImport alias importSpecs

-- ** Phases-Declaration Parsers

-- | Parses a phases declaration:
--
-- @
-- phases:
--     [ { rule1, rule2 }, { rule3 }, * ]
-- @
--
-- A phases declaration defines multi-phase rule execution for the
-- FPOP solver. It consists of:
--
-- 1. The @phases@ keyword (must not be indented)
-- 2. A colon separator
-- 3. A square-bracket-delimited list of rulesets
--
-- Each ruleset is either:
--
-- * An /explicit ruleset/ — curly-brace-delimited list of rule names
--   (e.g. @\{ rule1, rule2 @})
-- * The wildcard @*@ — representing \"all remaining rules\"
--
-- Only one @phases@ declaration is allowed per program.
--
-- @since 26.7
parsePhases :: ParserState σ => Parser σ PhasesDeclaration
parsePhases = parsePositioned $ do
  -- Parse the 'phases' keyword (must not be indented)
  -- No 'try' needed — phases is the last syntactic category
  _ <- L.nonIndented sc $ keyword "phases"
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the rulesets in square brackets with indentation checking
  rulesets <- betweenSquareBrackets indented $ commaSepBy1' parsePhaseRuleset
  -- Allocate a fresh node ID and construct the Phases AST node
  i <- getNewNodeId
  return $ PhasesDeclaration i rulesets

-- | Parses a single ruleset within a phases declaration. A ruleset is
-- either an explicit list of rule names or the wildcard @*@.
--
-- @
-- { rule1, rule2 }    -- explicit ruleset
-- *                    -- wildcard (all remaining rules)
-- @
--
-- @since 26.7
parsePhaseRuleset :: ParserState σ => Parser σ RulesetOrEverythingElse
parsePhaseRuleset =
  -- Try explicit ruleset first (starts with '{'), fall back to wildcard
  (Left <$> P.try parseExplicitRuleset) <|> (Right <$> parseEverythingElseRuleset)

-- | Parses an explicit ruleset: a curly-brace-delimited list of one or
-- more rule names (lowercase identifiers).
--
-- @
-- { rule1, rule2, rule3 }
-- @
--
-- Rule names are separated by commas with indentation checking.
--
-- @since 26.7
parseExplicitRuleset :: ParserState σ => Parser σ Ruleset
parseExplicitRuleset = parsePositioned $ do
  -- Parse the rule names in curly braces with indentation checking
  ruleset_rules <- betweenCurlyBraces indented $ commaSepBy1' parseLowerFirstSimpleIdentifier
  -- Allocate a fresh node ID and construct the Ruleset AST node
  i <- getNewNodeId
  return $ Ruleset i ruleset_rules

-- | Parses the wildcard ruleset (@*@), representing \"all remaining rules\"
-- in a phases declaration.
--
-- The wildcard must appear at most once and typically comes last in
-- the phases list, capturing any rules not assigned to explicit rulesets.
--
-- @since 26.7
parseEverythingElseRuleset :: ParserState σ => Parser σ EverythingElseRuleset
parseEverythingElseRuleset = parsePositioned $ do
  -- Parse the wildcard operator
  _ <- keywordOp "*"
  -- Allocate a fresh node ID and construct the EverythingElseRuleset AST node
  i <- getNewNodeId
  return $ EverythingElseRuleset i

-- ** Haskell-Code-Block Parser

-- | Parses a Haskell code block delimited by triple backticks with @hs@
-- language annotation:
--
-- @
-- ```hs
-- myHaskellFunction :: Int -> Int
-- myHaskellFunction x = x + 1
-- ```
-- @
--
-- The opening fence (@```hs@) must not be indented. The block contents
-- are captured as raw 'Text' until the closing fence (@```@) is found.
--
-- Haskell code blocks allow embedding Haskell source directly in Fixen
-- programs, typically for defining external symbols referenced by
-- @extern@ declarations.
--
-- @since 26.7
parseHaskellCodeBlock :: ParserState σ => Parser σ HsBlock
parseHaskellCodeBlock = parsePositioned $ do
  -- Parse the opening fence (```hs) — must not be indented
  -- Use 'try' since Haskell blocks are among many top-level alternatives
  _ <- P.try $ L.nonIndented sc $ keyword "```hs"
  -- Capture all characters until the closing fence (```)
  code_contents <- P.manyTill P.anySingle (keyword "```")
  -- Allocate a fresh node ID and construct the HsBlock AST node
  i <- getNewNodeId
  -- Convert the character list to a Text chunk
  let pxy :: Proxy Text = Proxy
  return $ HsBlock i (P.tokensToChunk pxy code_contents)
