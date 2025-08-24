-- |
--     Module      : Mozzarella.Parser.Type
--     Description : Parsers for Mozzarella (Haskell) types.
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Parsers for Mozzarella types. Currently, types in Mozzarella
--     are of the following grammar:
--
--     @
--     type ::= \<atom_type\>+ -- Type applications
--
--     atom_type ::= '(' \<type\> ')'
--                |  \<ident\>
--     @
--
--     "Atom types" (@\<atom_type\>@) are easier to parse, which is why
--     they are split off from the main @\<type\>@ production.
module Mozzarella.Parser.Type where

import Control.Applicative.Combinators (
  some,
  (<|>),
 )
import Data.List
import Error.Diagnose.Position qualified as DPos
import Mozzarella.IR.AST qualified as AST
import Mozzarella.Parser.Common
import Mozzarella.Parser.Token
import Text.Megaparsec qualified as P

-- | Parses a 'AST.Type'
parseType :: Parser AST.Type
parseType = parseTypeApp

-- | Parses a type application.
parseTypeApp :: Parser AST.Type
parseTypeApp = do
  -- Essentially, a type application is a list of atom types. Each type must
  -- be indented.
  (x : xs) <- some $ l (indented *> parseParenType)
  -- Fold the list as an application
  return $ foldl' folder x xs
  where
    folder :: AST.Type -> AST.Type -> AST.Type
    folder t t' =
      -- calculate source positions
      let startPos = DPos.begin $ AST.getPosition t
          endPos = DPos.end $ AST.getPosition t'
          file_name = DPos.file $ AST.getPosition t
          new_pos =
            DPos.Position
              { DPos.begin = startPos
              , DPos.end = endPos
              , DPos.file = file_name
              }
      in  -- make the app.
          AST.TypeApp new_pos t t'

-- | Parses an atomic (parenthesized) type
parseParenType :: Parser AST.Type
parseParenType = P.try f <|> parseTypeVar
  where
    f = do
      -- must be indented
      _ <- indented
      -- each type is indented
      (pos, t) <-
        l $
          parsePositioned $
            betweenParentheses
              (indented *> parseType)
      return $ AST.setPosition pos t

-- | Parses a type name.
parseTypeVar :: Parser AST.Type
parseTypeVar = do
  -- must be indented
  _ <- indented
  (pos, str) <- l $ parsePositioned parseCapitalizedIdentifier
  return $ AST.TypeVar pos str
