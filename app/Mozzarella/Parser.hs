{-# LANGUAGE OverloadedStrings #-}

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
  parseExtern,
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
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import Error.Diagnose.Diagnostic (addFile)
import Mozzarella.IR.AST qualified as AST
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
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))

--------------------------------------------------------------------------------
--
-- Main parser
--
--------------------------------------------------------------------------------

-- | Parses a Mozzarella program.
parse
  :: FilePath
  -- ^ The file path of the program
  -> Text
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
  -> Text
  -- ^ The contents of the file
  -> MozzarellaM a
mozzarellaParse parser file_path contents =
  let e = P.parse parser file_path contents
  in  case e of
        Right p -> return p
        Left err ->
          let d = errorDiagnosticFromBundle Nothing "syntax error" Nothing err
          in  mozzarellaError $ addFile d file_path (unpack contents)

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
  top_levels <- some (l parseTopLevel)
  return AST.Program {AST.topLevels = top_levels}

-- | Parses a 'AST.TopLevel' declaration.
parseTopLevel :: Parser AST.TopLevel
-- no try here. the first tokens in each branch is distinct and once one
-- matches we should commit to it.
parseTopLevel =
  (AST.TopLevelExtern <$> parseExtern)
    <|> (AST.TopLevelRelation <$> parseRelation)
    <|> (AST.TopLevelRule <$> parseRule)

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
      Right _ -> indentedWhiteSpaceConsumingSome parseLowerFirstIdentifier
  return $ AST.Extern pos ls

-- TODO: This needs to be cleaned up way more.

-- | Parses a 'AST.Relation'.
parseRelation :: Parser AST.Relation
parseRelation = do
  (pos, (a, b, c)) <- parsePositioned $ do
    -- parse the 'rel' keyword. rel must not be indented. we try here so we
    -- can avoid trying when parsing top level declaration
    _ <- P.try $ l $ L.nonIndented sc $ keyword "rel"
    -- parse the name of the relation. must be capitalized since these are
    -- constructors
    _ <- indented
    name <- parseCapitalizedIdentifier
    -- now try parsing the arguments. these are optional. however, if there
    -- are no arguments, we should not parse ':'. i.e., we cannot have
    -- a relation declaration like
    --    rel MyEmptyRel :
    (pos, args) <- parsePositioned $ do
      x <- P.observing $ P.try $ do
        _ <- indented
        _ <- l $ keywordOp ":"
        (pos, types) <- parsePositioned $ commaSepBy1 parseType
        return $ AST.RelationSignature pos types
      case x of
        Left _ -> return Nothing
        Right y -> return $ Just y
    -- parse a completion clause. The reason why we use the completion
    -- keyword instead of [myCompletion] is because we may want list types
    -- in relation arguments.
    comp <- do
      res <- P.observing $ P.try $ do
        _ <- sc
        (pos', ident) <- parsePositioned $ do
          _ <- indented
          _ <- l $ keyword "completion"
          _ <- indented
          parseLowerFirstIdentifier
        return $ AST.Completion pos' ident
      case res of
        Left _ -> return Nothing
        Right x -> return $ Just x
    let real_args =
          fromMaybe
            (AST.RelationSignature pos [])
            args
    return (name, real_args, comp)
  return $ AST.Relation pos a b c

-- TODO this needs to be cleaned up more.

-- | Parses a 'AST.Rule'. Rules are not indented. The turnstile @|-@ is not an
-- operator keyword, thus can (and should) be usable in expressions. Uses of
-- the turnstile in expressions should be parenthesized, e.g.
--
-- @rule myRule: Fact a b, if (a |- b) |- Fact a (b |- a)@
parseRule :: Parser AST.Rule
parseRule = do
  (pos, (name, bound_vars, premises, concl)) <- parsePositioned $ do
    -- parse the rule keyword. rules must not be indented.
    _ <- l $ L.nonIndented sc $ keyword "rule"
    -- try parsing the name and bound variables.
    _ <- indented
    maybe_name_and_bound_vars <- optional $ do
      -- rule names cannot be capitalized
      name <- l parseLowerFirstIdentifier
      _ <- indented
      -- parse the bound variables
      (pos, idents) <-
        parsePositioned $
          indentedWhiteSpaceConsumingMany parseLowerFirstIdentifier
      let vars = case idents of [] -> Nothing; _ -> Just $ AST.RuleBoundVars pos idents
      return (name, vars)
    let (name, bound_vars) = case maybe_name_and_bound_vars of
          Nothing -> (Nothing, Nothing)
          Just (x, y) -> (Just x, y)
    -- all rules have a ':' symbol.
    _ <- indented
    _ <- l $ keywordOp ":"
    premises <- do
      -- parse the premises
      (pos, premises) <- l $ parsePositioned $ commaSepBy parsePremise
      return $ AST.RulePremises pos premises
    -- parse the turnstile
    _ <- indented
    _ <- l turnstile
    -- parse the conclusion
    concl <- parseConclusion
    return (name, bound_vars, premises, concl)
  return $ AST.Rule pos name bound_vars premises concl

-- | Parses a 'AST.Premise' of a rule.
parsePremise :: Parser AST.Premise
-- There is no 'try' here because we try in the first identifier in
-- parseAssumption. This is because the first token of each branch are
-- obviously distinct, and once one matches, we should commit to it.
parsePremise = P.try parseAssumption <|> parseCondition

-- | Parses the 'AST.Conclusion' of a rule.
parseConclusion :: Parser AST.Conclusion
parseConclusion = do
  (pos, (header, real_args)) <- parsePositioned $ do
    -- must be indented
    _ <- indented
    -- must conclude fact, hence the first thing in the conclusion is a
    -- capitalized fact name
    header <- parseCapitalizedIdentifier
    (pos, args) <- parsePositioned $ indentedWhiteSpaceConsumingMany parseParenExpr
    let real_args = AST.ConclusionArguments pos args
    return (header, real_args)
  return $ AST.Conclusion pos header real_args

-- | Parses an 'AST.Assumption' (a fact). The arguments to facts must all be
-- variables as pattern matching on relation arguments is not supported.
parseAssumption :: Parser AST.Premise
parseAssumption = do
  (pos, (header, real_args)) <- parsePositioned $ do
    -- must be indented
    _ <- indented
    -- facts are all constructors, thus must be capitalized. we 'try' here so
    -- that we can backtrack if the rule premise is actually an 'if' condition.
    header <- P.try parseCapitalizedIdentifier
    -- try parse the arguments.
    (pos, args) <-
      parsePositioned $
        indentedWhiteSpaceConsumingMany parseLowerFirstIdentifier
    return (header, AST.AssumptionArguments pos args)
  return $ AST.PremiseAssumption pos header real_args

-- | Parses a 'AST.Condition' 'if' \<expr\>.
parseCondition :: Parser AST.Premise
parseCondition = do
  (pos, e) <- parsePositioned $ do
    _ <- indented
    _ <- keyword "if"
    parseExpr
  return $ AST.PremiseCondition pos e
