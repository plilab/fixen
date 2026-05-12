-- |
--     Module      : Fixen.Monad
--     Description : Re-exports all monads and environment types for the Fixen compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module is the single import point for all monadic infrastructure
--     used by the Fixen compiler. It re-exports everything from:
--
--     * 'Fixen.Monad.Type' — the two core monads ('FixenM' and 'FixenPass')
--       and the type combinators (':+:', ':*:', ':<:', ':>:') used to
--       compose pass state
--
--     * 'Fixen.Monad.Env' — all environment submodules:
--       * 'Fixen.Monad.Env.Errors' — error accumulation, pass runners,
--         and failure helpers
--       * 'Fixen.Monad.Env.NodeId' — node ID allocation
--       * 'Fixen.Monad.Env.Position' — source position tracking
--
--     Import this module wherever you need access to the compiler's
--     monadic types and utilities.
--
--
-- @since 0.0.1
module Fixen.Monad (
  module Fixen.Monad.Type,
  module Fixen.Monad.Env,
) where

import Fixen.Monad.Env
import Fixen.Monad.Type
