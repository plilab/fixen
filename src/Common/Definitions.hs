{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Definitions (mlbs, MLB) where

import Algebra.PartialOrd
import Numeric.Natural (Natural)

class (PartialOrd a) => MLB a where
  mlbs :: a -> a -> [a]
  default mlbs :: a -> a -> [a]
  mlbs x y | x == y    = [x]
           | otherwise = []

instance MLB Natural

instance MLB String

instance MLB Bool where
  mlbs p q = [p && q]

instance PartialOrd Natural where
  leq = (==)

instance {-# OVERLAPPING #-} PartialOrd String where
  leq = (==)

instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a, b, c) where
  leq (a, b, c) (a', b', c') = leq a a' && leq b b' && leq c c'

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d) => PartialOrd (a, b, c, d) where
  leq (a, b, c, d) (a', b', c', d') = leq a a' && leq b b' && leq c c' && leq d d'

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e) => PartialOrd (a, b, c, d, e) where
  leq (a, b, c, d, e) (a', b', c', d', e') = leq a a' && leq b b' && leq c c' && leq d d' && leq e e'

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f) => PartialOrd (a, b, c, d, e, f) where
  leq (a, b, c, d, e, f) (a', b', c', d', e', f') = leq a a' && leq b b' && leq c c' && leq d d' && leq e e' && leq f f'

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g) => PartialOrd (a, b, c, d, e, f, g) where
  leq (a, b, c, d, e, f, g) (a', b', c', d', e', f', g') = leq a a' && leq b b' && leq c c' && leq d d' && leq e e' && leq f f' && leq g g'

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h) => PartialOrd (a, b, c, d, e, f, g, h) where
  leq (a, b, c, d, e, f, g, h) (a', b', c', d', e', f', g', h') = leq a a' && leq b b' && leq c c' && leq d d' && leq e e' && leq f f' && leq g g' && leq h h'
