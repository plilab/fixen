-- |
--     Module      : Fixen.Monad.Env
--     Description : Re-exports all environment submodules for the Fixen compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module re-exports all environment-related modules used by the
--     Fixen compiler passes. Import this module (or 'Fixen.Monad' which
--     re-exports it) to access:
--
--     * 'Fixen.Monad.Env.Errors' — error tracking, pass runners, and
--       failure helpers
--     * 'Fixen.Monad.Env.NodeId' — node ID allocation
--     * 'Fixen.Monad.Env.Position' — source position tracking
--     * 'Fixen.Monad.Env.Symbol' — Symbol information
--
-- @since 26.7
module Fixen.Monad.Env (
  module Fixen.Monad.Env.Errors,
  module Fixen.Monad.Env.NodeId,
  module Fixen.Monad.Env.Position,
  module Fixen.Monad.Env.Symbol,
) where

import Fixen.Monad.Env.Errors
import Fixen.Monad.Env.NodeId
import Fixen.Monad.Env.Position
import Fixen.Monad.Env.Symbol
