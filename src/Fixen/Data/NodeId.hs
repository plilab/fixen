-- |
--     Module      : Fixen.Data.NodeId
--     Description : Unique node identifier type and class for the Fixen IR
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     Every construct in the Fixen intermediate representation (IR) is
--     associated with a 'NodeId'. This module defines the 'NodeId' type
--     as a type synonym for 'Int' and provides the 'HasNodeId' class for
--     extracting the identifier from IR terms.
--
--     NodeId values are monotonically increasing integers, allocated
--     sequentially by the parser ('Fixen.Parser') during AST construction.
--     They serve as stable handles that can be used to:
--
--     * Correlate IR nodes with source positions (line/column information)
--     * Attach auxiliary metadata such as symbol resolution results
--     * Track provenance for error reporting and debugging
--     * Form the basis of graph traversals across the IR
--
--     The 'HasNodeId' class is implemented by virtually every IR data type
--     (see 'Fixen.IR.Core') so that generic code can uniformly access the
--     identifier of any node.
module Fixen.Data.NodeId where

import Data.List.NonEmpty

-- | A unique identifier assigned to every construct in the Fixen IR.
--
--   Internally this is represented as an 'Int'. Values are allocated
--   sequentially starting from 0 by the parser, so they are always
--   non-negative and strictly increasing within a single parse run.
--
--   A 'NodeId' is /stable/ — it does not change when the surrounding AST
--   is transformed or pretty-printed. This makes it suitable as a key
--   for maps that store auxiliary information (e.g. symbol tables,
--   position maps, diagnostic data) alongside the IR.
type NodeId = Int

-- | Class of IR terms that carry a 'NodeId'.
--
--   Almost every data type in the Fixen IR ('Fixen.IR.Core') is an
--   instance of this class. The class provides a single method,
--   'getNodeId', which extracts the identifier from a term.
--
--   This enables generic code to work uniformly with any IR node
--   without needing to know its concrete type. For example, a
--   function that collects all node identifiers in a subtree can be
--   written once using 'HasNodeId' rather than pattern-matching on
--   every IR constructor.
class HasNodeId α where
  -- | Extract the 'NodeId' from a term.
  --
  --   For IR constructors that store the node identifier as their first
  --   field (the common convention in this codebase), the instance
  --   implementation is typically a one-liner:
  --
  --   @
  --   instance HasNodeId MyType where
  --     getNodeId (MyType i _) = i
  --   @
  --
  --   Or even simpler, when the identifier is the sole field:
  --
  --   @
  --   instance HasNodeId MyType where
  --     getNodeId = myTypeNodeId   -- point-free
  --   @
  getNodeId :: α -> NodeId

instance HasNodeId NodeId where
  getNodeId = id

-- | We want to ensure that we can compare two items for equality modulo having
-- different 'NodeId's. This is used to equate things together for easy comparison.
class EqModuloNodeId α where
  -- | Equality modulo 'NodeId's.
  (===) :: α -> α -> Bool
  a === b = not (a /== b)

  -- | Disequality modulo 'NodeId's.
  (/==) :: α -> α -> Bool
  a /== b = not (a === b)

  {-# MINIMAL (===) | (/==) #-}

infix 4 ===
infix 4 /==

instance EqModuloNodeId α => EqModuloNodeId [α] where
  [] === [] = True
  (x : xs) === (y : ys) = x === y && xs === ys
  _ === _ = False

instance EqModuloNodeId α => EqModuloNodeId (NonEmpty α) where
  (x :| xs) === (y :| ys) = x === y && xs === ys
