{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ReducedProduct.Common where

import Algebra.PartialOrd
import Control.DeepSeq
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Expr = Id String | InputE | Num Int | Plus Expr Expr | Leq Expr Expr | Gte Expr Expr | Eq Expr Expr
    deriving (Eq, Show, Generic)

instance NFData Expr where
    rnf (Id x) = rnf x
    rnf (Num x) = rnf x
    rnf (Plus e e') = rnf (e, e')
    rnf (Leq e e') = rnf (e, e')
    rnf (Gte e e') = rnf (e, e')
    rnf (Eq e e') = rnf (e, e')
    rnf InputE = ()

instance Hashable Expr

instance PartialOrd Expr where
    leq = (==)

