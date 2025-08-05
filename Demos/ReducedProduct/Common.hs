{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ReducedProduct.Common where

import GHC.Generics (Generic)
import Algebra.PartialOrd
import Data.Hashable (Hashable)

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
  deriving (Eq, Show, Generic)

instance Hashable Expr

instance PartialOrd Expr where
  leq = (==)