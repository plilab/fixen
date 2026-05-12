{-# LANGUAGE UndecidableInstances #-}

-- |
--     Module      : Fixen.Monad.Env.Position
--     Description : Position tracking for the Fixen compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides position tracking utilities for the Fixen compiler.
--     It maintains a 'PositionEnv' — an 'IntMap' from 'NodeId' values to
--     'Position' values (line/column information) — and provides functions
--     to look up and set positions for IR nodes.
--
--     The position environment is stored as a component of the pass state
--     (alongside 'NodeId' and 'FixenErrors'). It is populated during parsing
--     ('Fixen.Parser') as each AST node is constructed with a 'NodeId' and
--     its corresponding source position. Later passes can use this map to
--     retrieve source positions for error reporting and diagnostic messages.
module Fixen.Monad.Env.Position where

import Control.Monad.State.Strict
import Data.IntMap.Strict
import Error.Diagnose.Position
import Fixen.Fields
import Fixen.IR.AST
import Fixen.Monad.Type

--------------------------------------------------------------------------------

-- * The Position Environment

--------------------------------------------------------------------------------

-- | A map from 'NodeId' values to source positions ('Position').
--
--   This is the position environment carried in the pass state. Each key is
--   a 'NodeId' (an integer identifier assigned to an IR node during parsing),
--   and each value is the source position ('Position') where that node
--   appears in the original source file.
--
--   The map is populated during parsing ('Fixen.Parser') as nodes are
--   constructed. Later passes look up positions using 'fixenGetPosition'
--   to produce error messages with accurate line/column information.
type PositionEnv = IntMap Position

-- | Constraint alias indicating that a state type @σ@ contains a 'PositionEnv'.
--
-- This is a shorthand for @σ :>: PositionEnv@, meaning that a 'PositionEnv'
-- can be projected from @σ@ using the ':>:' class.
--
-- This constraint is required by all position lookup and update functions
-- in this module. It is satisfied by any state type that includes
-- 'PositionEnv' as a component, such as:
--
-- @
-- PositionEnv :*: NodeId :*: FixenErrors
-- @
type WithPositionEnv a = a :>: PositionEnv

--------------------------------------------------------------------------------

-- * Working Within the Positioned Monad

--------------------------------------------------------------------------------

-- | Obtains the 'PositionEnv'. Probably rarely used.
getPositionEnv :: (WithPositionEnv σ, MonadState σ μ) => μ PositionEnv
getPositionEnv = do
  st <- get
  return $ (↓) st

-- | Replaces the 'PositionEnv' with a new environment. Probably rarely used.
setPositionEnv
  :: (WithPositionEnv σ, MonadState σ μ)
  => PositionEnv
  -- ^ The new 'PositionEnv'
  -> μ ()
setPositionEnv new_env = do
  st <- get
  let new_state = st *<-: new_env
  put new_state

-- | Gets the 'Position' of a node in the 'Program'. Terminates exceptionally
-- if the provided node does not have a position.
getPosition
  :: (HasNodeId π NodeId, WithPositionEnv σ, MonadState σ μ)
  => π
  -- ^ The node in the 'Program'.
  -> μ Position
getPosition x = do
  let node = x ^. nodeId
  getPositionFromNodeId node

-- | Unexceptional equivalent of 'getPosition'.
getPosition_maybe
  :: (HasNodeId π NodeId, WithPositionEnv σ, MonadState σ μ)
  => π
  -- ^ The node in the 'Program'.
  -> μ (Maybe Position)
getPosition_maybe x = do
  let node = x ^. nodeId
  getPositionFromNodeId_maybe node

-- | Sets the position of a node in the 'Program'.
setPosition
  :: (HasNodeId π NodeId, WithPositionEnv σ, MonadState σ μ)
  => π
  -- ^ The node in the 'Program'
  -> Position
  -- ^ The 'Position' of the node.
  -> μ ()
setPosition x p = do
  let i = x ^. nodeId
  setPositionOfNodeId i p

-- | Same as 'getPosition', except that it obtains the position directly from
-- 'NodeId's.
getPositionFromNodeId :: (WithPositionEnv σ, MonadState σ μ) => NodeId -> μ Position
getPositionFromNodeId x = do
  pos_env <- getPositionEnv
  return $ pos_env ! x

-- | Same as 'getPosition_maybe', except that it obtains the position directly from
-- 'NodeId's.
getPositionFromNodeId_maybe :: (WithPositionEnv σ, MonadState σ μ) => NodeId -> μ (Maybe Position)
getPositionFromNodeId_maybe x = do
  pos_env <- getPositionEnv
  return $ pos_env !? x

-- | Same as 'setPosition', except that it directly sets the position of 'NodeId's.
setPositionOfNodeId :: (WithPositionEnv σ, MonadState σ μ) => NodeId -> Position -> μ ()
setPositionOfNodeId x p = do
  pos_env <- getPositionEnv
  let new_pos_env = insert x p pos_env
  setPositionEnv new_pos_env
