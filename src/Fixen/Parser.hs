{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser
--     Description : High-level parser for Fixen programs
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides the top-level parser entry point for Fixen programs.
--     It coordinates the parsing of all program constructs — module declarations,
--     imports, extern declarations, relations, rules, priority declarations,
--     queries, partial order declarations, include statements, Haskell code
--     blocks, and phase declarations — into a complete 'AST.Program'.
--
--     The main entry point is 'parse', which takes a file path and contents
--     and returns a fully constructed 'AST.Program'. Internally, it:
--
--     1. Runs 'parseProgram' to get a module declaration and a flat list of
--        top-level declarations ('TopLevel')
--     2. Calls 'partitionTopLevels' to distribute those declarations into the
--        appropriate fields of 'AST.Program', producing the final result
--
--     Individual declaration parsers (e.g. 'parseRelation', 'parseRule') are
--     also exported for reuse by other modules.
module Fixen.Parser (
  -- * Main entry points
  parse,
  fixenParse,
  parseProgram,

  -- * Top-level declaration parsing
  parseAST,

  -- * Individual declaration parsers
  parseRelation,
  parseRule,
  parsePremise,
  parseConclusion,
  parseAssumption,
  parseCondition,
  parsePartialOrd,
  parsePriority,
  parseQuery,
  parseInclude,
  parseImport,
  parsePhases,
  parseHaskellCodeBlock,
) where

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
import Data.Text (Text, unpack)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Report
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
--
-- Main entry point
--
--------------------------------------------------------------------------------

-- | Parses a complete Fixen program from raw file contents.
--
--   This is the top-level entry point for parsing. It:
--
--   1. Runs 'parseProgram' to extract the module declaration and a flat list
--      of top-level declarations ('TopLevel')
--   2. Calls 'partitionTopLevels' to distribute those declarations into the
--      appropriate fields of 'AST.Program'
--
--   The result is a fully constructed 'AST.Program' with all declarations
--   organized into their respective categories (relations, rules, priorities,
--   etc.).
--
--   If any parse errors occur, they are accumulated and reported via the
--   'FixenPass' monad.
parse
  :: FilePath
  -- ^ The file path of the program (used for error reporting)
  -> Text
  -- ^ The contents of the file to parse
  -> FixenPass ParserState Program
parse file_path contents = do
  -- Parse the module declaration and top-level declarations
  (mod_decl, top_levels) <- fixenParse parseProgram file_path contents
  -- Distribute the flat list of top-level declarations into the
  -- appropriate fields of AST.Program (relations, rules, etc.)
  partitionTopLevels mod_decl top_levels

--------------------------------------------------------------------------------
--
-- Running parsers in FixenM
--
--------------------------------------------------------------------------------

-- | Runs a 'Parser' in the 'FixenPass' monad, converting parse errors
-- into 'FixenPass' failures.
--
--   This function:
--
--   1. Captures the current 'ParserState' (including error accumulator)
--   2. Runs the Megaparsec parser via 'P.runParserT'
--   3. Restores the updated state (including any position tracking)
--   4. On success, returns the parsed value
--   5. On failure, converts the Megaparsec error bundle into a 'Diagnostic'
--      and fails the pass via 'failD'
--
--   This is the bridge between the Megaparsec parser layer and the
--   'FixenPass' compiler pass layer.
fixenParse
  :: Parser a
  -- ^ The 'Parser' to run
  -> FilePath
  -- ^ The file path of the program (used as the Megaparsec source name)
  -> Text
  -- ^ The contents of the file to parse
  -> FixenPass (PositionEnv :*: NodeId :*: FixenErrors) a
fixenParse parser file_path contents = do
  -- Capture the current parser state (position env, node ID counter, errors)
  st <- State.get
  -- Run the Megaparsec parser with the given file path and contents
  let e = P.runParserT parser file_path contents
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
-- Program-level parsing
--
--------------------------------------------------------------------------------

-- | Parses a complete Fixen program: a module declaration followed by
-- zero or more top-level declarations, with mandatory end-of-file.
--
--   Returns a tuple of:
--
--   * 'AST.ModuleDeclaration' — the parsed module declaration
--   * '[TopLevel]' — the flat list of all top-level declarations found
--     (before partitioning into AST.Program fields)
--
--   The 'eof' parser ensures that the entire file is consumed; any trailing
--   content after the last declaration will cause a parse error.
parseProgram :: Parser (ModuleDeclaration, [TopLevel])
parseProgram = do
  -- Parse the module declaration (e.g. "module Foo where") and consume trailing whitespace
  mod_head <- l parseModuleDeclaration
  -- Parse all remaining top-level declarations, consuming whitespace after each
  top_levels <- l parseAST
  -- Ensure no trailing content after the last declaration
  _ <- eof
  return (mod_head, top_levels)

-- | Parses a list of top-level declarations. Each declaration is one of:
--
--   * @extern@ declarations
--   * @rel@ (relation) declarations
--   * @rule@ declarations
--   * @partial ord@ declarations
--   * @priority@ declarations
--   * @query@ declarations
--   * @include@ statements
--   * @import@ statements
--   * Haskell code blocks (```hs … ```)
--   * @phases@ declarations
--
--   Each item is parsed independently with whitespace consumed after it.
--   At least one top-level declaration is required.
parseAST :: Parser [TopLevel]
parseAST =
  -- Parse one or more top-level declarations, consuming whitespace after each
  some $
    l $
      -- Try each declaration type in order; the first match wins
      (TLRelation <$> parseRelation)
        <|> (TLRule <$> parseRule)
        <|> (TLPartialOrd <$> parsePartialOrd)
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
--   partitioned into the structured 'AST.Program' type.
data TopLevel
  = -- | A 'rel' declaration defining a relation (fact type)
    TLRelation Relation
  | -- | A 'rule' declaration defining inference rules
    TLRule Rule
  | -- | A 'partial ord' declaration defining a partial order on a type
    TLPartialOrd PartialOrdDeclaration
  | -- | A Haskell code block (```hs … ```)
    TLHsBlock HsBlock
  | -- | A 'priority' declaration defining ordering between rule instances
    TLPriority Priority
  | -- | A 'query' declaration specifying a query mode for a relation
    TLQuery Query
  | -- | An 'include' statement importing another Fixen file
    TLInclude Include
  | -- | An 'import' statement importing a Haskell module
    TLImport HsImport
  | -- | A 'phases' declaration defining multi-phase rule execution
    TLPhases Phases

-- | Distributes a flat list of 'TopLevel' declarations into the structured
-- 'AST.Program' type, placing each declaration into its appropriate field.
--
--   This function processes the list in order, accumulating declarations
--   into the correct 'AST.Program' fields. It also performs validation:
--
--   * /Multiple extern declarations/ — produces a /warning/ (the last one wins)
--   * /Multiple phase declarations/ — produces a /fatal error/ (only one allowed)
--
--   The result is an 'AST.Program' with all fields populated from the
--   parsed top-level declarations.
partitionTopLevels :: ModuleDeclaration -> [TopLevel] -> FixenPass ParserState Program
partitionTopLevels mod_decl [] =
  -- Base case: no more top-level declarations — return Program with
  -- all optional fields set to empty/Nothing
  return
    Program
      { hsBlocks = []
      , priorities = []
      , queries = []
      , moduleName = mod_decl
      , hsImports = []
      , relations = []
      , rules = []
      , includes = []
      , phases = Nothing
      , partialOrdDeclarations = []
      }
partitionTopLevels mod_decl (x : xs) = do
  -- Recursively partition the rest of the list
  rest <- partitionTopLevels mod_decl xs
  -- Place the current declaration into the correct field
  case x of
    TLRelation r -> return rest {relations = r : relations rest} -- add relation
    TLRule r -> return rest {rules = r : rules rest} -- add rule
    TLPartialOrd po ->
      return rest {partialOrdDeclarations = po : partialOrdDeclarations rest} -- add partial order
    TLPriority p -> return rest {priorities = p : priorities rest} -- add priority
    TLQuery q -> return rest {queries = q : queries rest} -- add query
    TLInclude i -> return rest {includes = i : includes rest} -- add include
    TLImport i -> return rest {hsImports = i : hsImports rest} -- add import
    TLPhases p ->
      -- Phases: validate that there's at most one (fatal error if multiple)
      case phases rest of
        Nothing -> return rest {phases = Just p} -- first phases — just add it
        Just p' -> do
          -- multiple phases — fatal error
          p_pos <- fixenGetPosition p
          p'_pos <- fixenGetPosition p'
          failErr
            Nothing
            "syntax error"
            [ (p_pos, Where "a phase definition")
            , (p'_pos, This "another phase definition")
            ]
            [Note "each program can only have one phase declaration"]
    TLHsBlock h -> return rest {hsBlocks = h : hsBlocks rest} -- add Haskell block

--------------------------------------------------------------------------------
--
-- Individual declaration parsers
--
--------------------------------------------------------------------------------

-- | Parses a module declaration: @module@ @My.Haskell.Module@ @where@.
--
--   The module name is parsed as a 'AST.ModuleName' (a series of capitalized
--   identifiers separated by dots). The 'module' and 'where' keywords must
--   not be indented.
--
--   This declaration is compulsory in every Fixen program.
parseModuleDeclaration :: Parser ModuleDeclaration
parseModuleDeclaration = parsePositioned $ do
  -- Parse the 'module' keyword (must not be indented)
  -- No need to 'try' here — module declarations are compulsory
  _ <- l $ L.nonIndented sc $ keyword "module"
  -- Parse the module name (e.g. Data.List)
  m <- l parseModuleName
  -- Parse the 'where' keyword
  _ <- keyword "where"
  -- Allocate a fresh node ID and construct the ModuleDeclaration AST node
  i <- fixenGetNewNodeId
  return $ ModuleDeclaration i m

-- | Parses a relation declaration:
--
--   @
--   rel RelationName: Type1, Type2
--   @
--
--   The 'rel' keyword must not be indented. The relation name must be
--   capitalized (since relations are constructor-like). Arguments are
--   optional — if present, they follow a colon and are comma-separated types.
--
--   Example without arguments:
--
--   @
--   rel MyFact
--   @
--
--   Example with arguments:
--
--   @
--   rel Dist: Integer, Integer
--   @
parseRelation :: Parser Relation
parseRelation = parsePositioned $ do
  -- Parse the 'rel' keyword (must not be indented)
  -- Use 'P.try' so we can backtrack when parsing top-level declarations
  _ <- P.try $ L.nonIndented sc $ keyword "rel"
  -- Verify proper indentation before the relation name
  _ <- indented
  -- Parse the capitalized relation name (relations are constructor-like)
  name <- parseCapitalizedSimpleIdentifier
  -- Attempt to parse optional arguments: a colon followed by comma-separated types
  -- 'P.observing' allows backtracking — if no colon, args are empty
  colon <- P.observing $ P.try $ indented *> keywordOp ":"
  args <- case colon of
    Left _ -> return [] -- no colon — no arguments
    Right _ -> do
      -- Verify proper indentation before the first argument
      _ <- indented
      -- Parse one or more comma-separated types with indentation checking
      args <- commaSepBy1' (parseType indented)
      return $ toList args -- convert NonEmpty list to regular list
      -- Allocate a fresh node ID and construct the Relation AST node
  i <- fixenGetNewNodeId
  return $ Relation i name args

-- | Parses a rule declaration:
--
--   @
--   rule [ruleName boundVar1 boundVar2 ...]:
--       assumption1
--     , assumption2
--     , if condition
--    |- conclusion
--   @
--
--   The 'rule' keyword must not be indented. A rule consists of:
--
--   1. An optional rule name (lowercase identifier) followed by optional
--      bound variables
--   2. A colon separator
--   3. Zero or more premises (assumptions and conditions), separated by commas
--   4. A turnstile (@|-@ or @⊢@)
--   5. A conclusion (a capitalized fact name followed by expression arguments)
--
--   The turnstile (@|-@) is a keyword operator, not a regular operator,
--   so it can be used in expressions only when parenthesized.
--   For example: @if (a |- b) |- Fact a (b |- a)@
parseRule :: Parser Rule
parseRule = parsePositioned $ do
  -- Parse the 'rule' keyword (must not be indented)
  -- Definitely need 'try' here since rules are among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "rule"
  -- Verify proper indentation before the rule name
  _ <- indented
  -- Parse the optional rule name and bound variables (all lowercase identifiers)
  -- If no identifiers are found, the rule is unnamed with no bound variables
  (name, bound_vars) <- do
    idents <- manyI' parseLowerFirstSimpleIdentifier
    case idents of
      [] -> return (Nothing, []) -- no identifiers — unnamed rule
      (x : xs) -> return (Just x, xs) -- first is name, rest are bound variables
      -- Parse the colon separator with indentation checks on both sides
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the premises (assumptions and conditions), separated by commas
  -- 'partitionPremises' separates them into assumptions and conditions
  (assumptions, conditions) <- do
    premises <- commaSepBy' parsePremise
    return $ partitionPremises premises
  -- Parse the turnstile (@|-@ or @⊢@) with indentation checks
  _ <- indented *> turnstile *> indented
  -- Parse the conclusion (capitalized fact name + expression arguments)
  concl <- parseConclusion
  -- Allocate a fresh node ID and construct the Rule AST node
  i <- fixenGetNewNodeId
  return $ Rule i name bound_vars assumptions conditions concl

-- | Represents a premise within a rule body.
--
--   A premise is either:
--
--   * An /assumption/ — a relation applied to variable arguments
--     (e.g. @MyFact a b@)
--   * A /condition/ — an expression guarded by the 'if' keyword
--     (e.g. @if a <= b@)
--
--   The 'partitionPremises' function separates a flat list of 'RulePremise'
--   values into two groups: assumptions first, then conditions.
data RulePremise
  = RPAssumption Assumption -- a relation assumption (e.g. MyFact a b)
  | RPCondition Condition -- an 'if' condition (e.g. if a <= b)

-- | Separates a list of 'RulePremise' values into assumptions and conditions.
--
--   Assumptions always come before conditions in the output lists.
--   This function processes the list in reverse order (from the end) to
--   maintain the original ordering within each group.
--
--   For example, given @[RPAssumption a, RPCondition c, RPAssumption b]@,
--   the result is @([b, a], [c])@ — assumptions @b@ and @a@ first (in
--   reverse input order), then condition @c@.
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
--   * An /assumption/ — a relation applied to variable arguments
--     (e.g. @MyFact a b@)
--   * A /condition/ — an expression guarded by the 'if' keyword
--     (e.g. @if a <= b@)
--
--   No 'try' is used here because the first token of each branch is
--   distinct: assumptions start with a capitalized identifier, while
--   conditions start with the 'if' keyword. Once one matches, we commit
--   to it.
parsePremise :: Parser RulePremise
parsePremise =
  -- Try assumption first (starts with capitalized identifier)
  RPAssumption <$> parseAssumption
    -- Fall back to condition (starts with 'if' keyword)
    <|> RPCondition <$> parseCondition

-- | Parses the conclusion of a rule: a capitalized fact name followed by
-- zero or more expression arguments.
--
--   @
--   |- MyFact arg1,
--      arg2
--   @
--
--   The conclusion must start with a capitalized identifier (since facts
--   are constructor-like), followed by expression arguments. The arguments
--   are separated by commas with indentation checking.
parseConclusion :: Parser Conclusion
parseConclusion = parsePositioned $ do
  -- Verify proper indentation before the conclusion
  _ <- indented
  -- Parse the capitalized fact name (conclusions are constructor-like)
  header <- parseCapitalizedSimpleIdentifier
  -- Verify proper indentation before the arguments
  -- _ <- indented
  -- Parse zero or more expression arguments with indentation checking
  args <- manyI' (parseParenExpr indented)
  -- Allocate a fresh node ID and construct the Relation AST node
  i <- fixenGetNewNodeId
  return $ Relation i header args

-- | Parses an assumption within a rule body: a relation name applied to
-- variable arguments.
--
--   @
--       MyFact var1,
--       var2
--   @
--
--   The relation name must be capitalized (since relations are
--   constructor-like), and the arguments must be lowercase-starting
--   simple identifiers.
--
--   A 'try' is used here so that if the premise is actually a condition
--   (starting with 'if'), we can backtrack and try 'parseCondition' instead.
parseAssumption :: Parser Assumption
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
  args <- manyI' parseLowerFirstSimpleIdentifierOrHole
  -- Allocate a fresh node ID and construct the Assumption AST node
  i <- fixenGetNewNodeId
  return $ Relation i header args

-- | Parses a condition within a rule body: an expression guarded by
-- the 'if' keyword.
--
--   @
--       if a <= b
--   @
--
--   The 'if' keyword must not be immediately followed by identifier
--   continuation characters (guaranteed by 'keyword'). The expression
--   is parsed with indentation checking.
parseCondition :: Parser Condition
parseCondition = parsePositioned $ do
  -- Parse the 'if' keyword with indentation checks on both sides
  _ <-
    indented
      *> keyword "if"
      *> indented
  -- Parse the condition expression with indentation checking
  e <- parseExpr indented
  -- Allocate a fresh node ID and construct the Condition AST node
  i <- fixenGetNewNodeId
  return $ Condition i e

-- | Parses a partial order declaration:
--
--   @
--   partial ord Dist
--       where
--       type = Dist
--       leq = (<=)
--       mlbs = meet
--   @
--
--   This defines a partial order (reflexive, transitive, antisymmetric
--   relation) on a given type. It specifies:
--
--   * The base type ('type' field)
--   * The less-than-or-equal function ('leq' field)
--   * The maximal lower bounds function ('mlbs' field)
--
--   The 'partial' and 'ord' keywords must not be indented. The 'where'
--   block contains exactly three fields, each indented relative to the
--   'partial ord' line.
parsePartialOrd :: Parser PartialOrdDeclaration
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
  name <- parseCapitalizedSimpleIdentifier
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
  i <- fixenGetNewNodeId
  return $ PartialOrdDeclaration i name type_expr leq_func mlbs_func

-- | Parses a single field within a 'partial ord' declaration.
--
--   Each field has the form: @fieldName = value@, where:
--
--   * @fieldName@ is one of @type@, @leq@, or @mlbs@
--   * @value@ is parsed by the provided 'valueParser'
--   * Proper indentation is verified before and after each component
--
--   For example, the field @type = Dist@ is parsed as:
--
--   @
--   keyword "type"  ->  keywordOp "="  ->  parseType indented
--   @
parsePartialOrdField
  :: Text
  -- ^ Field name ("type", "leq", or "mlbs")
  -> Parser a
  -- ^ Parser for the field value
  -> Parser a
parsePartialOrdField fieldName valueParser = do
  -- Parse: fieldName = value, with indentation checks between all components
  _ <- indented *> keyword fieldName *> indented *> keywordOp "=" *> indented
  -- Parse the value (type expression, identifier, etc.)
  valueParser

-- | Parses a priority declaration:
--
--   @
--   priority:
--       a <= b |- addDist { a = a, b = b' } <= addDist { a = a', b = b' }
--   @
--
--   A priority declaration specifies an ordering between two rule instances.
--   It consists of:
--
--   1. The 'priority' keyword (must not be indented)
--   2. A colon separator
--   3. A premise expression (left side of the turnstile)
--   4. A turnstile (@|-@ or @⊢@)
--   5. A priority conclusion (two rule instances connected by @<=@ or @⊏@)
--
--   Each priority declaration applies to exactly one priority rule. For
--   multiple rules, create multiple priority declarations. This avoids
--   indentation ambiguity.
parsePriority :: Parser Priority
parsePriority = parsePositioned $ do
  -- Parse the 'priority' keyword (must not be indented)
  -- Need 'try' since priority is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "priority"
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the premise expression (left side of the turnstile)
  expr <- parseExpr indented
  -- Parse the turnstile with indentation checks
  _ <- indented *> turnstile *> indented
  -- Parse the priority conclusion (two rule instances with ordering)
  concl <- parsePriorityConclusion
  -- Allocate a fresh node ID and construct the Priority AST node
  i <- fixenGetNewNodeId
  return $ Priority i expr concl

-- | Parses the conclusion of a priority declaration: two rule instances
-- connected by an ordering symbol (@<=@ or @⊏@).
--
--   @
--       addDist { a = a } <= addDist { a = a' }
--   @
--
--   Each rule instance consists of a rule name (lowercase identifier)
--   followed by optional variable substitutions in curly braces.
parsePriorityConclusion :: Parser PriorityConclusion
parsePriorityConclusion = parsePositioned $ do
  -- Parse the left-hand side rule instance
  left <- parseRuleInstance
  -- Parse the ordering symbol (<= or ⊏) with indentation checks
  _ <- indented *> ltOrSqSubsetEq *> indented
  -- Parse the right-hand side rule instance
  right <- parseRuleInstance
  -- Allocate a fresh node ID and construct the PriorityConclusion AST node
  i <- fixenGetNewNodeId
  return $ PriorityConclusion i left right

-- | Parses a rule instance: a rule name optionally followed by variable
-- substitutions in curly braces.
--
--   @
--       addDist { a = a, b = b' }
--       assignI { }
--       myRule
--   @
--
--   The rule name is a lowercase-starting simple identifier. Substitutions
--   are comma-separated pairs of identifiers separated by '='.
parseRuleInstance :: Parser RuleInstance
parseRuleInstance = parsePositioned $ do
  -- record the offset so as to throw errors
  offset_start <- P.getOffset
  -- Parse the rule name (lowercase identifier), consuming trailing whitespace
  name <- l parseLowerFirstSimpleIdentifier
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
      i <- fixenGetNewNodeId
      return $ RuleInstance i name (Map.fromList subst)

-- | Parses a variable substitution: @left = right@, where both sides
-- are lowercase-starting simple identifiers.
--
--   @
--       a = a'
--   @
--
--   Used within rule instance declarations to specify how variables
--   are mapped.
parseSubstitution :: Parser (SimpleIdentifier, SimpleIdentifier)
parseSubstitution = do
  -- Parse the left-hand side identifier
  left <- parseLowerFirstSimpleIdentifier
  -- Parse the '=' operator with indentation checks on both sides
  _ <- indented *> keywordOp "=" *> indented
  -- Parse the right-hand side identifier
  right <- parseLowerFirstSimpleIdentifier
  return (left, right)

-- | Parses a query declaration:
--
--   @
--   query DistTo as distTo - +
--   @
--
--   A query declaration specifies a query mode for a relation. It consists of:
--
--   1. The 'query' keyword (must not be indented)
--   2. A relation name (capitalized identifier)
--   3. The 'as' keyword
--   4. A query name (lowercase identifier)
--   5. One or more mode symbols (@+@ for input, @-@ for output)
--
--   Modes indicate whether each argument of the relation is an input or
--   output variable for the query.
parseQuery :: Parser Query
parseQuery = parsePositioned $ do
  -- Parse the 'query' keyword (must not be indented)
  -- Need 'try' since query is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "query"
  -- Verify proper indentation before the relation name
  _ <- indented
  -- Parse the query name (lowercase identifier)
  name <- parseLowerFirstSimpleIdentifier
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the relation with modes
  relation <- parseQueriedRelation
  -- Allocate a fresh node ID and construct the Query AST node
  i <- fixenGetNewNodeId
  return $ Query i relation name

-- | Parses the relation part within a query declaration: a relation name
-- applied to 'QueryMode's.
--
-- @
-- MyFact - +
-- @
--
-- The relation name must be capitalized (since relations are
-- constructor-like), and the arguments must be query modes.
parseQueriedRelation :: Parser QueriedRelation
parseQueriedRelation = parsePositioned $ do
  -- Verify proper indentation before the assumption
  _ <- indented
  -- Parse the capitalized relation name
  -- Use 'P.try' so we can backtrack if this is actually a condition
  header <- P.try parseCapitalizedSimpleIdentifier
  -- Parse zero or more lowercase-starting simple identifiers as arguments
  args <- manyI' parseQueryMode
  -- Allocate a fresh node ID and construct the Assumption AST node
  i <- fixenGetNewNodeId
  return $ Relation i header args

-- | Parses a single mode symbol: @+@ (input) or @-@ (output).
--
--   Mode symbols indicate whether a relation argument is an input or
--   output variable for a query.
--
--   * @+@ → 'Input' — the argument is an input
--   * @-@ → 'Output' — the argument is an output
parseQueryMode :: Parser QueryMode
parseQueryMode = parsePositioned $ do
  -- Parse the mode symbol with indentation check, consuming trailing whitespace
  m <- indented *> P.try (C.char '+' >> return Input) <|> (C.char '-' >> return Output)
  -- Allocate a fresh node ID and wrap in the QueryMode constructor
  i <- fixenGetNewNodeId
  return $ m i

-- | Parses an include statement:
--
--   @
--   include "Path/To/Fixen.fix"
--   @
--
--   The 'include' keyword must not be indented. The path is a string
--   literal (double-quoted, with escape sequences supported).
--
--   Include statements allow one Fixen program to import and reuse
--   declarations from another Fixen file.
parseInclude :: Parser Include
parseInclude = parsePositioned $ do
  -- Parse the 'include' keyword (must not be indented)
  -- Need 'try' since include is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "include"
  -- Verify proper indentation before the path string
  _ <- indented
  -- Parse the file path as a string literal
  path <- parseRawString
  -- Allocate a fresh node ID and construct the Include AST node
  i <- fixenGetNewNodeId
  return $ Include i path

-- | Parses an import statement:
--
--   @
--   import My.Haskell.Module
--   @
--
--   The 'import' keyword must not be indented. The module name is parsed
--   as a 'AST.ModuleName' (a series of capitalized identifiers separated
--   by dots).
--
--   Import statements allow a Fixen program to reference Haskell symbols
--   defined in external modules.
parseImport :: Parser HsImport
parseImport = parsePositioned $ do
  -- Parse the 'import' keyword (must not be indented)
  -- Need 'try' since import is among many top-level alternatives
  _ <- P.try $ l $ L.nonIndented sc $ keyword "import"
  -- Verify proper indentation before the module name
  _ <- indented
  -- Parse the Haskell module name (e.g. Data.List, MyCompany.MyModule)
  mod_name <- parseModuleName
  -- Allocate a fresh node ID and construct the HsImport AST node
  i <- fixenGetNewNodeId
  return $ HsImport i mod_name

-- | Parses a phases declaration:
--
--   @
--   phases:
--       [ { rule1, rule2 }, { rule3 }, * ]
--   @
--
--   A phases declaration defines multi-phase rule execution for the
--   FPOP (Fixed Point Over lattices) solver. It consists of:
--
--   1. The 'phases' keyword (must not be indented)
--   2. A colon separator
--   3. A square-bracket-delimited list of rulesets
--
--   Each ruleset is either:
--
--   * An /explicit ruleset/ — curly-brace-delimited list of rule names
--     (e.g. @\{ rule1, rule2 @})
--   * The wildcard @*@ — representing "all remaining rules"
--
--   Only one phases declaration is allowed per program.
parsePhases :: Parser Phases
parsePhases = parsePositioned $ do
  -- Parse the 'phases' keyword (must not be indented)
  -- No 'try' needed — phases is the last syntactic category
  _ <- L.nonIndented sc $ keyword "phases"
  -- Parse the colon separator with indentation checks
  _ <- indented *> keywordOp ":" *> indented
  -- Parse the rulesets in square brackets with indentation checking
  rulesets <- betweenSquareBrackets indented $ commaSepBy1' parsePhaseRuleset
  -- Allocate a fresh node ID and construct the Phases AST node
  i <- fixenGetNewNodeId
  return $ Phases i rulesets

-- | Parses a single ruleset within a phases declaration. A ruleset is
-- either an explicit list of rule names or the wildcard @*@.
--
--   @
--       { rule1, rule2 }    -- explicit ruleset
--       *                    -- wildcard (all remaining rules)
--   @
parsePhaseRuleset :: Parser RulesetOrEverythingElse
parsePhaseRuleset =
  -- Try explicit ruleset first (starts with '{'), fall back to wildcard
  (Left <$> P.try parseExplicitRuleset) <|> (Right <$> parseEverythingElseRuleset)

-- | Parses an explicit ruleset: a curly-brace-delimited list of one or
-- more rule names (lowercase identifiers).
--
--   @
--       { rule1, rule2, rule3 }
--   @
--
--   Rule names are separated by commas with indentation checking.
parseExplicitRuleset :: Parser ExplicitRuleset
parseExplicitRuleset = parsePositioned $ do
  -- Parse the rule names in curly braces with indentation checking
  rules <- betweenCurlyBraces indented $ commaSepBy1' parseLowerFirstSimpleIdentifier
  -- Allocate a fresh node ID and construct the Ruleset AST node
  i <- fixenGetNewNodeId
  return $ Ruleset i rules

-- | Parses the wildcard ruleset (@*@), representing "all remaining rules"
-- in a phases declaration.
--
--   @
--       *
--   @
--
--   The wildcard must appear at most once and typically comes last in
--   the phases list, capturing any rules not assigned to explicit rulesets.
parseEverythingElseRuleset :: Parser EverythingElseRuleset
parseEverythingElseRuleset = parsePositioned $ do
  -- Parse the wildcard operator
  _ <- keywordOp "*"
  -- Allocate a fresh node ID and construct the EverythingElseRuleset AST node
  i <- fixenGetNewNodeId
  return $ EverythingElseRuleset i

-- | Parses a Haskell code block delimited by triple backticks with @hs@
-- language annotation:
--
--   @
--   ```hs
--   myHaskellFunction :: Int -> Int
--   myHaskellFunction x = x + 1
--   ```
--   @
--
--   The opening fence (```hs) must not be indented. The block contents
--   are captured as raw 'Text' until the closing fence (```) is found.
--
--   Haskell code blocks allow embedding Haskell source directly in Fixen
--   programs, typically for defining external symbols referenced by
--   'extern' declarations.
parseHaskellCodeBlock :: Parser HsBlock
parseHaskellCodeBlock = parsePositioned $ do
  -- Parse the opening fence (```hs) — must not be indented
  -- Use 'try' since Haskell blocks are among many top-level alternatives
  _ <- P.try $ L.nonIndented sc $ keyword "```hs"
  -- Capture all characters until the closing fence (```)
  contents <- P.manyTill P.anySingle (keyword "```")
  -- Allocate a fresh node ID and construct the HsBlock AST node
  i <- fixenGetNewNodeId
  -- Convert the character list to a Text chunk
  let pxy :: Proxy Text = Proxy
  return $ HsBlock i (P.tokensToChunk pxy contents)
