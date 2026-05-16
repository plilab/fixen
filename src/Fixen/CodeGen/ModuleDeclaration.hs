{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.CodeGen.ModuleDeclaration
-- Description : Code Generation for Module Declarations
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities for 'ModuleDeclaration's.
--
-- @since 0.0.1
module Fixen.CodeGen.ModuleDeclaration where

import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.Fields
import Fixen.IR.AST

-- | Generates the code for the Haskell module declaration
--
-- @since 0.0.1
codeGenModuleDeclaration :: Program -> Text
codeGenModuleDeclaration program =
  let n = program ^. moduleName . name
   in Text.concat ["module ", fullIdentifier n, " where"]
