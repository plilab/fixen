module Common.Definitions where

import Algebra.PartialOrd

class (PartialOrd a) => MLB a where
  mlbs :: a -> a -> [a]