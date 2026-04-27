-- |
--     Module      : Fixen.Monad.Env.NodeId
--     Description : Node ID allocation for the Fixen compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module provides node ID allocation for the Fixen compiler.
--     It maintains a single 'NodeId' counter in the pass state and provides
--     a function to generate unique, monotonically increasing identifiers.
--
--     Each time 'fixenGetNewNodeId' is called, it returns the current counter
--     value and increments it. This ensures that every IR node receives a
--     unique identifier that can be used for error reporting, symbol
--     resolution, and other tracking purposes.
--
--     The node ID counter is stored as a component of the pass state
--     (alongside 'PositionEnv' and 'FixenErrors'). It is initialized to 0
--     at the start of parsing and incremented throughout the parsing and
--     processing pipeline.
--
module Fixen.Monad.Env.NodeId where

import Control.Monad.State.Strict (
  MonadState,
  get,
  put,
 )
import Fixen.Data.NodeId (NodeId)
import Fixen.Monad.Type (
   (*<-:),
   (:>:),
   (↓),
  )

-- | Constraint alias indicating that a state type @σ@ contains a 'NodeId'.
--
--   This is a shorthand for @σ :>: NodeId@, meaning that a 'NodeId' counter
--   can be projected from @σ@ using the ':>:' class.
--
--   This constraint is required by 'fixenGetNewNodeId'. It is satisfied by
--   any state type that includes 'NodeId' as a component, such as:
--
--   @
--   PositionEnv :*: NodeId :*: FixenErrors
--   @
type NodeIded σ = σ :>: NodeId

-- | Allocate a new unique 'NodeId' and increment the counter in the state.
--
--   This is the primary function for generating node identifiers. It:
--
--   1. Retrieves the current 'NodeId' counter from the pass state
--   2. Returns the current value as the new unique identifier
--   3. Increments the counter and stores it back in the state
--
--   The returned identifiers are monotonically increasing, starting from
--   the initial value stored in the state (typically 0 at the start of
--   parsing). This guarantees that every call produces a unique identifier
--   within a single compilation run.
--
--   This function is called by the parser ('Fixen.Parser') every time a
--   new AST node is constructed. Each AST constructor wraps its result
--   with a freshly allocated 'NodeId'.
--
--   @
--   -- In a pass, allocate a new node ID:
--   newId <- fixenGetNewNodeId
--   -- newId :: NodeId  — a unique, never-before-seen identifier
--   @
fixenGetNewNodeId :: (NodeIded σ, MonadState σ μ) => μ NodeId
fixenGetNewNodeId = do
  -- Retrieve the current pass state
  st <- get
  -- Project the NodeId counter from the state
  let i :: NodeId = (↓) st
      -- The next counter value
      j = i + 1
      -- Update the NodeId component in the state with the incremented value
      new_state = st *<-: j
  -- Write the updated state back
  put new_state
  -- Return the allocated (pre-increment) value as the new unique ID
  return j
