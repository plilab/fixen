{-# LANGUAGE OverloadedStrings #-}

-- |
--     Module      : Fixen.Parser
--     Description : Parser for Fixen programs
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the parser for Fixen programs.
module Fixen.Parser (
  -- * Parser
  parse,
  fixenParse,
  parseProgram,

  -- * Parsing AST nodes
  parseAST,
  parseExtern,
  parseRelation,
  parseRule,
  parsePremise,
  parseConclusion,
  parseAssumption,
  parseCondition,
) where

import Control.Applicative.Combinators (
  -- optional,
  some,
  (<|>),
 )
import Data.List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Report
import Fixen.IR.AST qualified as AST
import Fixen.IR.Core
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
-- Main parser
--
--------------------------------------------------------------------------------

-- | Parses a Fixen program.
parse
  :: FilePath
  -- ^ The file path of the program
  -> Text
  -- ^ The contents of the file
  -> FixenPass FixenErrors AST.Program
parse file_path contents = do
  top_levels <- fixenParse parseProgram file_path contents
  -- TODO: We probably need to parse the module header, then the imports, then
  -- the rest of the program; otherwise, we probably can't instantiate the
  -- AST.Program type.
  partitionTopLevels undefined top_levels

--------------------------------------------------------------------------------
--
-- Parsing in FixenM
--
--------------------------------------------------------------------------------

-- | Runs a parser in the 'FixenM' monad
fixenParse
  :: Parser a
  -- ^ The 'Parser' to run
  -> FilePath
  -- ^ The file path of the program
  -> Text
  -- ^ The contents of the file
  -> FixenPass FixenErrors a
fixenParse parser file_path contents = do
  let e = P.parse parser file_path contents
  case e of
    Right p -> return p
    Left p -> do
      failD $ errorDiagnosticFromBundle Nothing "syntax error" Nothing p

--------------------------------------------------------------------------------
--
-- Parsing AST nodes
--
--------------------------------------------------------------------------------

-- | Parses the program with whitespaces and eof.
parseProgram :: Parser [TopLevel]
parseProgram = sc *> parseAST <* eof

-- | Parses a 'AST.Program'.
parseAST :: Parser [TopLevel]
parseAST =
  some $
    l $
      (TLExtern <$> parseExtern)
        <|> (TLRelation <$> parseRelation)
        <|> (TLRule <$> parseRule)
        <|> (TLPartialOrd <$> parsePartialOrd)
        <|> (TLPriority <$> parsePriority)
        <|> (TLQuery <$> parseQuery)
        <|> (TLInclude <$> parseInclude)
        <|> (TLImport <$> parseImport)
        <|> (TLPhases <$> parsePhases)

data TopLevel
  = TLExtern AST.Extern
  | TLRelation AST.Relation
  | TLRule AST.Rule
  | TLPartialOrd AST.PartialOrdDeclaration
  | TLPriority AST.Priority
  | TLQuery AST.Query
  | TLInclude AST.Include
  | TLImport AST.HsImport
  | TLPhases AST.Phases

-- TODO: Might wanna deal with this portion too.
partitionTopLevels :: AST.ModuleDeclaration -> [TopLevel] -> FixenPass FixenErrors AST.Program
partitionTopLevels mod_decl [] =
  return
    AST.Program
      { AST.hsBlocks = []
      , AST.priorities = []
      , AST.queries = []
      , AST.moduleName = mod_decl
      , AST.hsImports = []
      , AST.extern = Nothing
      , AST.relations = []
      , AST.rules = []
      , AST.includes = []
      , AST.phases = Nothing
      , AST.partialOrdDeclarations = []
      }
partitionTopLevels mod_decl (x : xs) = do
  rest <- partitionTopLevels mod_decl xs
  case x of
    TLExtern e ->
      -- only one extern declaration should be allowed; similar structure as Priority
      -- Note: change one priority with multiple rules to multiple priorities with one
      -- rule each.
      case AST.extern rest of
        Nothing -> return rest {AST.extern = Just e}
        Just e' ->
          failR $
            Err
              Nothing
              "syntax error"
              [ (AST.getPosition e, Where "an extern definition")
              , (AST.getPosition e', This "another extern definition")
              ]
              [Note "each program can only have one extern declaration"]
    TLRelation r -> return rest {AST.relations = r : AST.relations rest}
    TLRule r -> return rest {AST.rules = r : AST.rules rest}
    TLPartialOrd po -> return rest {AST.partialOrdDeclarations = po : AST.partialOrdDeclarations rest}
    TLPriority p -> return rest {AST.priorities = p : AST.priorities rest}
    TLQuery q -> return rest {AST.queries = q : AST.queries rest}
    TLInclude i -> return rest {AST.includes = i : AST.includes rest}
    TLImport i -> return rest {AST.hsImports = i : AST.hsImports rest}
    TLPhases p ->
      -- only one phase declaration should be allowed; similar structure as extern
      case AST.phases rest of
        Nothing -> return rest {AST.phases = Just p}
        Just p' ->
          failR $
            Err
              Nothing
              "syntax error"
              [ (AST.getPosition p, Where "a phase definition")
              , (AST.getPosition p', This "another phase definition")
              ]
              [Note "each program can only have one phase declaration"]

-- | Parses a 'AST.Extern'.
parseExtern :: Parser AST.Extern
parseExtern = do
  offset_start <- P.getOffset
  (pos, ls) <- parsePositioned $ do
    -- Parse the 'extern' keyword. extern must not be indented. We try
    -- here so we do not have to try when parsing top-level declarations
    _ <- P.try $ l $ L.nonIndented sc $ keyword "extern"
    -- make sure if it is indented. Otherwise, it means that the extern
    -- declaration is empty!
    r <- P.observing indented
    case r of
      Left _ ->
        -- re-throw more informative parse error
        P.parseError
          ( P.FancyError
              offset_start
              ( Set.singleton
                  (ErrorFail "extern declaration cannot be empty!")
              )
          )
      Right _ -> someI' parseLowerFirstSimpleIdentifier
  return $ Extern pos ls

-- | Parses a 'AST.Relation'.
parseRelation :: Parser AST.Relation
parseRelation = do
  (pos, (name, args)) <- parsePositioned $ do
    -- parse the 'rel' keyword. rel must not be indented. we try here so we
    -- can avoid trying when parsing top level declaration
    _ <- P.try $ L.nonIndented sc $ keyword "rel"
    -- parse the name of the relation. must be capitalized since these are
    -- constructors
    _ <- indented
    name <- parseCapitalizedSimpleIdentifier
    -- now try parsing the arguments. these are optional. however, if there
    -- are no arguments, we should not parse ':'. i.e., we cannot have
    -- a relation declaration like
    --    rel MyEmptyRel :
    colon <- P.observing $ P.try $ indented *> keywordOp ":"
    case colon of
      Left _ -> return (name, [])
      Right _ -> do
        _ <- indented
        args <- commaSepBy1' (parseType indented)
        return (name, toList args)
  return $ Relation pos name args

-- | Parses a 'AST.Rule'. Rules are not indented. The turnstile @|-@ is not an
-- operator keyword, thus can (and should) be usable in expressions. Uses of
-- the turnstile in expressions should be parenthesized, e.g.
--
-- @rule myRule: Fact a b, if (a |- b) |- Fact a (b |- a)@
parseRule :: Parser AST.Rule
parseRule = do
  (pos, (name, bv, asm, cond, cnc)) <- parsePositioned $ do
    -- parse the rule keyword. rules must not be indented.
    -- definitely need a try here
    _ <- P.try $ l $ L.nonIndented sc $ keyword "rule"
    _ <- indented
    -- try parsing the name and bound variables.
    (name, bound_vars) <- do
      idents <- manyI' parseLowerFirstSimpleIdentifier
      case idents of
        [] -> return (Nothing, Nothing)
        [x] -> return (Just x, Nothing)
        (x : x' : xs) -> return (Just x, Just $ x' : xs)
    -- all rules have a ':' symbol.
    _ <- indented *> keywordOp ":" *> indented
    (assumptions, conditions) <- do
      -- parse the premises
      premises <- commaSepBy' parsePremise
      return $ partitionPremises premises
    -- parse the turnstile
    _ <- indented *> turnstile *> indented
    -- parse the conclusion
    concl <- parseConclusion
    return (name, bound_vars, assumptions, conditions, concl)
  return $ Rule pos name bv asm cond cnc

data RulePremise
  = RPAssumption AST.Assumption -- order of assumptions and conditions
  | RPCondition AST.Condition

partitionPremises
  :: [RulePremise]
  -> ([AST.Assumption], [AST.Condition])
partitionPremises [] = ([], [])
partitionPremises (x : xs) =
  let (as, cs) = partitionPremises xs
  in  case x of
        RPAssumption a -> (a : as, cs)
        RPCondition c -> (as, c : cs)

-- | Parses a 'AST.Premise' of a rule.
parsePremise :: Parser RulePremise
-- There is no 'try' here because we try in the first identifier in
-- parseAssumption. This is because the first token of each branch are
-- obviously distinct, and once one matches, we should commit to it.
parsePremise =
  RPAssumption <$> parseAssumption
    <|> RPCondition <$> parseCondition

-- | Parses the 'AST.Conclusion' of a rule.
parseConclusion :: Parser AST.Conclusion
parseConclusion = do
  (pos, (header, args)) <- parsePositioned $ do
    -- must be indented
    _ <- indented
    -- must conclude fact, hence the first thing in the conclusion is a
    -- capitalized fact name
    header <- parseCapitalizedSimpleIdentifier
    _ <- indented
    -- try parse the arguments. these can be expressions.
    args <- manyI' (parseExpr indented)
    return (header, args)
  return $ Fact pos header args

-- | Parses an 'AST.Assumption' (a fact). The arguments to facts must all be
-- variables as pattern matching on relation arguments is not supported.
parseAssumption :: Parser AST.Assumption
parseAssumption = do
  (pos, (header, real_args)) <- parsePositioned $ do
    -- must be indented
    _ <- indented
    -- facts are all constructors, thus must be capitalized. we 'try' here so
    -- that we can backtrack if the rule premise is actually an 'if' condition.
    header <- P.try parseCapitalizedSimpleIdentifier
    _ <- indented
    -- try parse the arguments.
    args <- manyI' parseLowerFirstSimpleIdentifier
    return (header, args)
  return $ Fact pos header real_args

-- | Parses a 'AST.Condition' 'if' \<expr\>.
parseCondition :: Parser AST.Condition
parseCondition = do
  (pos, e) <-
    parsePositioned $
      indented
        *> keyword "if"
        *> indented
        *> parseExpr indented
  return $ Condition pos e

-- | Parses a partial order declaration
parsePartialOrd :: Parser AST.PartialOrdDeclaration
parsePartialOrd = do
  (pos, (name, type_expr, leq_func, mlbs_func)) <- parsePositioned $ do
    -- Parse 'partial' and 'ord' keywords (must not be indented)
    -- definitely need a try here.
    _ <- P.try $ l $ L.nonIndented sc $ keyword "partial"
    _ <- indented
    _ <- keyword "ord"
    _ <- indented

    -- Parse the type name (Dist)
    name <- parseCapitalizedSimpleIdentifier
    _ <- indented

    -- Parse 'where' keyword
    _ <- keyword "where"
    _ <- indented

    -- Parse the indented block of fields
    -- Each field must be indented relative to the 'partial ord' line
    type_expr <- parsePartialOrdField "type" (parseType indented)
    leq_func <- parsePartialOrdField "leq" (parseNonInfixTermIdentifier indented)
    mlbs_func <- parsePartialOrdField "mlbs" (parseNonInfixTermIdentifier indented)

    return (name, type_expr, leq_func, mlbs_func)

  return $ PartialOrdDeclaration pos name type_expr leq_func mlbs_func

-- | Helper to parse a field like: "type = Dist"
parsePartialOrdField
  :: Text -- Field name ("type", "leq", "mlbs")
  -> Parser a -- Parser for the value
  -> Parser a
parsePartialOrdField fieldName valueParser = do
  _ <- indented *> keyword fieldName *> indented *> keywordOp "=" *> indented -- / simplications
  valueParser

-- | Parses priority declarations:
--   priority:
--     a <= a' |- addDist { a = a } <= addDist { a = a' }
--
-- Note that this should be just a single priority rule, i.e., if we want multiple rules,
-- we should create another entire priority declaration. This is so that we do not have
-- to deal with the indentation mess.
parsePriority :: Parser AST.Priority
parsePriority = do
  (pos, (expr, concl)) <- parsePositioned $ do
    -- Parse 'priority' keyword (must not be indented)
    -- definitely need a try here.
    _ <- P.try $ l $ L.nonIndented sc $ keyword "priority"
    _ <- indented *> keywordOp ":" *> indented
    _ <- indented
    -- Parse premises (left side of |-)
    expr <- parseExpr indented
    -- parse the turnstile
    _ <- indented *> turnstile *> indented
    -- Parse priority conclusion
    concl <- parsePriorityConclusion
    return (expr, concl)

  return $ Priority pos expr concl

-- | Parses priority conclusion like:
--   "addDist { a = a } < addDist { a = a' }" or
--   "addDist { } < assignI { }"
parsePriorityConclusion :: Parser AST.PriorityConclusion
parsePriorityConclusion = do
  (pos, (lhs, rhs)) <- parsePositioned $ do
    left <- parseRuleInstance
    _ <- indented *> ltOrSqSubsetEq *> indented
    right <- parseRuleInstance
    return (left, right)
  return $ PriorityConclusion pos lhs rhs

-- | Parses a rule instance like: "addDist { a = a }" or "assignI { }"
--   Returns (name, maybe substitutions)
parseRuleInstance :: Parser AST.RuleInstantiation
parseRuleInstance = do
  (pos, (name, subs)) <- parsePositioned $ do
    name <- parseLowerFirstSimpleIdentifier
    -- Parse optional substitutions
    subst <- betweenCurlyBraces indented $ commaSepBy' parseSubstitution
    return (name, Map.fromList subst) -- Using Left/Right as a simple tagged union
  return $ RuleInstantiation pos name subs

-- | Parses a substitution like: "a = a"
parseSubstitution :: Parser (AST.SimpleIdentifier, AST.SimpleIdentifier)
parseSubstitution = do
  left <- parseLowerFirstSimpleIdentifier
  _ <- indented *> keywordOp "=" *> indented
  right <- parseLowerFirstSimpleIdentifier
  return (left, right)

-- | Parses query declarations like:
--   query DistTo as distTo - +
--   query DistTo as distLessThan + -
parseQuery :: Parser AST.Query
parseQuery = do
  (pos, (relation, name, modes)) <- parsePositioned $ do
    -- Parse 'query' keyword (must not be indented)
    -- definitely need a try here.
    _ <- P.try $ l $ L.nonIndented sc $ keyword "query"
    _ <- indented

    -- Parse relation name (capitalized)
    relation <- parseCapitalizedSimpleIdentifier
    _ <- indented

    -- Parse 'as' keyword
    _ <- keyword "as"
    _ <- indented

    -- Parse query name (lowercase)
    name <- parseLowerFirstSimpleIdentifier
    _ <- indented

    -- Parse mode list (like - + or + -)
    modes <- someI' parseQueryMode

    return (relation, name, modes)

  return $ Query pos relation name modes

-- | Parses a single mode: '+' or '-'
parseQueryMode :: Parser AST.QueryMode
parseQueryMode = do
  (pos, m) <- parsePositioned $ indented *> P.try (C.char '+' >> return Input) <|> (C.char '-' >> return Output)
  return $ QueryMode pos m

-- | Parses an include "Path/To/Fixen.fix" statement
parseInclude :: Parser AST.Include
parseInclude = do
  (pos, path) <- parsePositioned $ do
    -- parse the include keyword. include statements must not be indented.
    -- definitely need a try here.
    _ <- P.try $ l $ L.nonIndented sc $ keyword "include"
    _ <- indented
    parseRawString
  return $ Include pos path

-- | Parses an import My.Haskell.Module statement
parseImport :: Parser AST.HsImport
parseImport = do
  (pos, mod_name) <- parsePositioned $ do
    -- parse the include keyword. include statements must not be indented.
    -- definitely need a try here.
    _ <- P.try $ l $ L.nonIndented sc $ keyword "import"
    _ <- indented
    parseModuleName
  return $ HsImport pos mod_name

-- | Parses a phases declaration
parsePhases :: Parser AST.Phases
parsePhases = do
  (pos, rulesets) <- parsePositioned $ do
    -- parse the include keyword. include statements must not be indented.
    -- do not need a try here. This is the last syntactic category in the
    -- program.
    _ <- L.nonIndented sc $ keyword "phases"
    -- all phases have a ':' symbol.
    _ <- indented *> keywordOp ":" *> indented
    -- parse the rulesets
    betweenSquareBrackets indented $ commaSepBy1' parsePhaseRuleset
  -- parseModuleName
  return $ Phases pos rulesets

parsePhaseRuleset :: Parser AST.RulesetOrEverythingElse
parsePhaseRuleset = do
  (Left <$> P.try parseExplicitRuleset) <|> (Right <$> parseEverythingElseRuleset)

parseExplicitRuleset :: Parser AST.ExplicitRuleset
parseExplicitRuleset = do
  (pos, rules) <- parsePositioned $ do
    betweenCurlyBraces indented $ commaSepBy1' parseLowerFirstSimpleIdentifier
  return $ Ruleset pos rules

parseEverythingElseRuleset :: Parser AST.EverythingElseRuleset
parseEverythingElseRuleset = do
  (pos, _) <- parsePositioned $ keywordOp "*"
  return $ AST.EverythingElseRuleset pos
