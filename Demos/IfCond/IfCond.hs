module IfCond.IfCond where

import Algebra.PartialOrd

instance PartialOrd Ordering where
  leq = (<=)

eq :: Ordering -> Ordering -> Bool
eq = (==)