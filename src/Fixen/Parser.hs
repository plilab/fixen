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

data TopLevel = TLExtern AST.Extern | TLRelation AST.Relation | TLRule AST.Rule

-- TODO: Might wanna deal with this portion too.
partitionTopLevels :: AST.ModuleDeclaration -> [TopLevel] -> FixenPass FixenErrors AST.Program
partitionTopLevels mod_decl [] =
  return
    AST.Program
      { AST.hsBlocks = []
      , AST.priorities = Nothing
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
    _ <- L.nonIndented sc $ keyword "rule"
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
  = RPAssumption AST.Assumption
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
