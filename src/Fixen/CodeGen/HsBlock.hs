{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Fixen.CodeGen.HsBlock
-- Description : Code Generation for Haskell code blocks
-- Copyright   : (c) Programming Languages Innovation Lab@NUS
-- License     : MIT
-- Maintainer  : yongqi@nus.edu.sg
-- Stability   : experimental
--
-- This module provides code-generation facilities for 'HsBlock's.
--
-- @since 0.0.1
module Fixen.CodeGen.HsBlock where

import Data.Text (Text)
import Data.Text qualified as Text
import Fixen.Fields
import Fixen.IR.AST

--------------------------------------------------------------------------------

-- * Main Entry Point

--------------------------------------------------------------------------------

-- | Generates the Haskell code blocks in the 'Program'
--
-- @since 0.0.1
codeGenHsBlocks :: Program -> Text
codeGenHsBlocks prog =
  let blocks = prog ^. hsBlocks
   in if null blocks
        then ""
        else
          let t = blocks ^.. each . contents <&> Text.strip & Text.intercalate "\n\n"
           in Text.concat ["\n----- USER CODE START -----\n", t, "\n----- USER CODE END -----"]
