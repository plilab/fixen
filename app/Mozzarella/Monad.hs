-- |
--     Module      : Mozzarella.Monad
--     Description : The main monad for the Mozzarella compiler
--     Copyright   : (c) Programming Languages Innovation Lab@NUS
--     License     : MIT
--     Maintainer  : yongqi@nus.edu.sg
--     Stability   : experimental
--
--     This module defines the main monad for the Mozzarella compiler.
module Mozzarella.Monad (
  MozzarellaM,
  mozzarellaError,
  runMozzarellaM,
) where

import Control.Monad.Except qualified as Except
import Error.Diagnose.Diagnostic qualified as Diagnostic

-- | The monad
type MozzarellaM a = Except.ExceptT (Diagnostic.Diagnostic String) IO a

-- | Raises an error in 'MozzarellaM'
mozzarellaError :: Diagnostic.Diagnostic String -> MozzarellaM a
mozzarellaError = Except.liftEither . Left

-- | Obtains the underlying 'IO' from a 'MozzarellaM'
runMozzarellaM :: MozzarellaM a -> IO (Either (Diagnostic.Diagnostic String) a)
runMozzarellaM = Except.runExceptT
