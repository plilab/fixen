{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Definitions where

import Algebra.PartialOrd
import Numeric.Natural (Natural)

class (PartialOrd a) => MLB a where
  mlbs :: a -> a -> [a]
  default mlbs :: a -> a -> [a]
  mlbs x y | x == y    = [x]
           | otherwise = []

instance PartialOrd Natural where
  leq = (==)

instance {-# OVERLAPPING #-} PartialOrd String where
  leq = (==)

instance MLB Natural

instance MLB String

instance MLB Bool