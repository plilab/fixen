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

import Control.Monad.State.Strict (
  MonadState,
  get,
  put,
 )
import Data.IntMap.Strict (
  IntMap,
  insert,
  (!),
  (!?),
 )
import Error.Diagnose.Position (Position)

-- import Fixen.Data.NodeId (HasNodeId (..))
import Fixen.IR.AST (HasNodeId (..))
import Fixen.Monad.Type (
  (*<-:),
  (:>:),
  (↓),
 )
import Prelude hiding (lookup)

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
--   This is a shorthand for @σ :>: PositionEnv@, meaning that a 'PositionEnv'
--   can be projected from @σ@ using the ':>:' class.
--
--   This constraint is required by all position lookup and update functions
--   in this module. It is satisfied by any state type that includes
--   'PositionEnv' as a component, such as:
--
--   @
--   PositionEnv :*: NodeId :*: FixenErrors
--   @
type Positioned a = a :>: PositionEnv

-- | Look up the source position of an IR node by its 'NodeId'.
--
--   This retrieves the position from the 'PositionEnv' in the pass state
--   and returns it directly. If the node's 'NodeId' is not found in the
--   map, this will throw an error (via the 'IntMap.!' operator).
--
--   Use 'fixenGetPosition_maybe' if you need safe lookup that returns
--   'Nothing' for missing nodes.
--
--   @
--   -- Given a node 'x' in a pass:
--   pos <- fixenGetPosition x
--   -- pos :: Position  — the source location of x
--   @
fixenGetPosition :: (HasNodeId α, Positioned σ, MonadState σ μ) => α -> μ Position
fixenGetPosition x = do
  -- Extract the NodeId from the IR node
  let i = getNodeId x
  -- Retrieve the current pass state
  st <- get
  -- Project the PositionEnv from the state
  let pos_env :: PositionEnv = (↓) st
  -- Look up the position (throws if not found)
  return $ pos_env ! i

-- | Look up the source position of an IR node by its 'NodeId', returning
-- 'Nothing' if the node is not found.
--
--   This is the safe variant of 'fixenGetPosition'. It uses the 'IntMap.!?'
--   operator which returns 'Nothing' for missing keys instead of throwing.
--
--   This is useful when the position may not have been recorded yet
--   (e.g., for nodes that were constructed programmatically rather than
--   parsed from source).
--
--   @
--   -- Given a node 'x' in a pass:
--   pos <- fixenGetPosition_maybe x
--   -- pos :: Maybe Position  — Nothing if not found
--   @
fixenGetPosition_maybe :: (HasNodeId α, Positioned σ, MonadState σ μ) => α -> μ (Maybe Position)
fixenGetPosition_maybe x = do
  -- Extract the NodeId from the IR node
  let i = getNodeId x
  -- Retrieve the current pass state
  st <- get
  -- Project the PositionEnv from the state
  let pos_env :: PositionEnv = (↓) st
  -- Safe lookup: returns Nothing if the NodeId is not in the map
  return $ pos_env !? i

-- | Set the source position of an IR node.
--
--   This updates the 'PositionEnv' in the pass state by inserting the
--   given 'Position' at the node's 'NodeId'. If a position was already
--   recorded for this 'NodeId', it is overwritten.
--
--   This is typically used during parsing to record the position of each
--   AST node as it is constructed, or in later passes to annotate nodes
--   whose positions were not available during parsing.
--
--   @
--   -- Given a node 'x' and a position 'p' in a pass:
--   fixenSetPosition x p
--   @
fixenSetPosition :: (HasNodeId α, Positioned σ, MonadState σ μ) => α -> Position -> μ ()
fixenSetPosition x p = do
  -- Extract the NodeId from the IR node
  let i = getNodeId x
  -- Retrieve the current pass state
  st <- get
  -- Project the PositionEnv from the state
  let pos_env :: PositionEnv = (↓) st
      -- Insert the new position into the map
      new_pos_env = insert i p pos_env
      -- Update the PositionEnv component in the state
      new_st = st *<-: new_pos_env
  -- Write the updated state back
  put new_st
