-- |
--     Module      : Mozzarella.Parser
--     Description : Parser for Mozzarella programs
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the parser for Mozzarella programs.
module Mozzarella.Parser (
  -- * Parser
  parse,
  mozzarellaParse,
  parseProgram,

  -- * Parsing AST nodes
  parseAST,
  parseTopLevel,
  parseRelation,
  parseRule,
  parsePremise,
  parseConclusion,
  parseAssumption,
  parseCondition,
) where

import Control.Applicative.Combinators (
  optional,
  some,
  (<|>),
 )
import Data.Maybe (fromMaybe)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (addFile)
import Mozzarella.IR.AST qualified as AST
import Mozzarella.IR.Core
import Mozzarella.Monad (
  MozzarellaM,
  mozzarellaError,
 )
import Mozzarella.Parser.Common
import Mozzarella.Parser.Expr
import Mozzarella.Parser.Token
import Mozzarella.Parser.Type
import Text.Megaparsec (eof)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------
--
-- Main parser
--
--------------------------------------------------------------------------------

-- | Parses a Mozzarella program.
parse
  :: FilePath
  -- ^ The file path of the program
  -> String
  -- ^ The contents of the file
  -> MozzarellaM AST.Program
parse = mozzarellaParse parseProgram

--------------------------------------------------------------------------------
--
-- Parsing in MozzarellaM
--
--------------------------------------------------------------------------------

-- | Runs a parser in the 'MozzarellaM' monad
mozzarellaParse
  :: Parser a
  -- ^ The 'Parser' to run
  -> FilePath
  -- ^ The file path of the program
  -> String
  -- ^ The contents of the file
  -> MozzarellaM a
mozzarellaParse parser file_path contents =
  let e = P.parse parser file_path contents
  in  case e of
        Right p -> return p
        Left err ->
          let d = errorDiagnosticFromBundle Nothing "syntax error" Nothing err
          in  mozzarellaError $ addFile d file_path contents

--------------------------------------------------------------------------------
--
-- Parsing AST nodes
--
--------------------------------------------------------------------------------

-- | Parses the program with whitespaces and eof.
parseProgram :: Parser AST.Program
parseProgram = sc *> parseAST <* eof

-- | Parses a 'AST.Program'.
parseAST :: Parser AST.Program
parseAST = do
  top_levels <- some parseTopLevel
  return AST.Program {AST.topLevels = top_levels}

-- | Parses a 'AST.TopLevel' declaration.
parseTopLevel :: Parser AST.TopLevel
-- no try here. the first tokens in each branch is distinct and once one
-- matches we should commit to it.
parseTopLevel = ((↑) <$> l parseRelation) <|> ((↑) <$> l parseRule)

-- | Parses a 'AST.Relation'.
parseRelation :: Parser AST.Relation
parseRelation = do
  (pos, (a, b, c)) <- l $ parsePositioned $ do
    -- parse the 'rel' keyword. rel must not be indented. we try here so we
    -- can avoid trying when parsing top level declaration
    _ <- P.try $ l $ L.nonIndented sc $ keyword "rel"
    -- parse the name of the relation. must be capitalized since these are
    -- constructors
    _ <- indented
    name <- l parseCapitalizedIdentifier
    -- now try parsing the arguments. these are optional. however, if there
    -- are no arguments, we should not parse ':'. i.e., we cannot have
    -- a relation declaration like
    --    rel MyEmptyRel :
    (pos, args) <- l $ parsePositioned $ optional $ do
      _ <- l $ indented *> keywordOp ":"
      (pos, types) <- l $ parsePositioned $ commaSepBy1 parseType
      return $ AST.mkRelationSignature pos types
    -- parse a completion clause. The reason why we use the completion
    -- keyword instead of [myCompletion] is because we may want list types
    -- in relation arguments.
    comp <- optional $ do
      (pos', ident) <- parsePositioned $ do
        _ <- indented
        _ <- l $ keyword "completion"
        _ <- indented
        l parseLowerFirstIdentifier
      return $ AST.mkCompletion pos' ident
    let real_args =
          fromMaybe
            (AST.mkRelationSignature pos [])
            args
    return (name, real_args, comp)
  return $ AST.mkRelation pos a b c

-- | Parses a 'AST.Rule'. Rules are not indented. The turnstile @|-@ is not an
-- operator keyword, thus can (and should) be usable in expressions. Uses of
-- the turnstile in expressions should be parenthesized, e.g.
--
-- @rule myRule: Fact a b, if (a |- b) |- Fact a (b |- a)@
parseRule :: Parser AST.Rule
parseRule = do
  (pos, (name, premises, concl)) <- l $ parsePositioned $ do
    -- parse the rule keyword. rules must not be indented.
    _ <- l $ L.nonIndented sc $ keyword "rule"
    -- try parsing the name. rule names cannot be capitalized
    _ <- indented
    name <- l $ optional parseLowerFirstIdentifier
    -- all rules have a ':' symbol.
    _ <- indented
    _ <- l $ keywordOp ":"
    premises <- do
      -- parse the premises
      (pos, premises) <- l $ parsePositioned $ commaSepBy parsePremise
      return $ AST.mkRulePremises pos premises
    -- parse the turnstile
    _ <- indented
    _ <- l turnstile
    -- parse the conclusion
    concl <- l parseConclusion
    return (name, premises, concl)
  return $ AST.mkRule pos name premises concl

-- | Parses a 'AST.Premise' of a rule.
parsePremise :: Parser AST.Premise
-- There is no 'try' here because we try in the first identifier in
-- parseAssumption. This is because the first token of each branch are
-- obviously distinct, and once one matches, we should commit to it.
parsePremise = ((↑) <$> parseAssumption) <|> ((↑) <$> parseCondition)

-- | Parses the 'AST.Conclusion' of a rule.
parseConclusion :: Parser AST.Conclusion
parseConclusion = do
  (pos, (header, real_args)) <- l $ parsePositioned $ do
    -- must be indented
    _ <- indented
    -- must conclude fact, hence the first thing in the conclusion is a
    -- capitalized fact name
    header <- l parseCapitalizedIdentifier
    _ <- indented
    -- try parse the arguments. the reason we use optional and some instead
    -- of many is so that we don't get nonsense source positions for
    -- non-existent arguments. the arguments are atom expressions, i.e. this
    -- fact is just an expression application.
    (pos, args) <- l $ parsePositioned $ optional $ do
      (pos, ls) <- l $ parsePositioned $ some (indented *> parseParenExpr)
      return $ AST.mkConclusionArguments pos ls
    -- pos should be a fixed point (not a range) if args is empty; use that to
    -- our advantage
    let real_args = fromMaybe (AST.mkConclusionArguments pos []) args
    -- return the header (fact name) and arguments
    return (header, real_args)
  return $ AST.mkConclusion pos header real_args

-- | Parses an 'AST.Assumption' (a fact). The arguments to facts must all be
-- variables as pattern matching on relation arguments is not supported.
parseAssumption :: Parser AST.Assumption
parseAssumption = do
  (pos, (header, real_args)) <- l $ parsePositioned $ do
    -- must be indented
    _ <- indented
    -- facts are all constructors, thus must be capitalized. we 'try' here so
    -- that we can backtrack if the rule premise is actually an 'if' condition.
    header <- l $ P.try parseCapitalizedIdentifier
    _ <- indented
    -- try parse the arguments. the reason we use optional and some instead
    -- of many is so that we don't get nonsense source positions for
    -- non-existent arguments.
    (pos, args) <- l $ parsePositioned $ optional $ do
      -- try parsing a nonempty argument list
      (pos, ls) <-
        l $
          parsePositioned $
            some (indented *> l parseLowerFirstIdentifier)
      return $ AST.mkAssumptionArguments pos ls -- Annotated {annotation = pos, unAnnotate = ls}
      -- pos should be a fixed point (not a range) if args is empty; use that to our
      -- advantage
    let real_args = fromMaybe (AST.mkAssumptionArguments pos []) args
    -- return the header (fact name) and arguments
    return (header, real_args)
  return $ AST.mkAssumption pos header real_args

-- | Parses a 'AST.Condition' 'if' \<expr\>.
parseCondition :: Parser AST.Condition
parseCondition = do
  (pos, e) <- l $ parsePositioned $ do
    _ <- indented
    _ <- l $ keyword "if"
    parseExpr
  return $ AST.mkCondition pos e
