{-# OPTIONS_GHC -Wno-orphans #-}
module Common.Instances () where

import Algebra.Lattice.Dropped
import Algebra.Lattice.Lifted
import Algebra.Lattice.Op
import Algebra.Lattice.Ordered
import Data.Functor.Classes
import Data.Hashable.Lifted

instance Eq1 Dropped where
  liftEq p dx dy = case (dx, dy) of
    (Drop x, Drop y) -> p x y
    (Top,    Top)    -> True
    _                -> False

instance Eq1 Lifted where
  liftEq p (Lift x) (Lift y) = p x y
  liftEq _ Bottom   Bottom   = True
  liftEq _ _        _        = False

instance Eq1 Op where
  liftEq p x y = p (getOp x) (getOp y)

instance Eq1 Ordered where
  liftEq p x y = p (getOrdered x) (getOrdered y)

instance Ord1 Dropped where
  liftCompare c dx dy = case (dx, dy) of
    (Drop x, Drop y) -> c x y
    (Drop _, Top)    -> LT
    (Top,    Drop _) -> GT
    (Top,    Top)    -> EQ

instance Ord1 Lifted where
  liftCompare c lx ly = case (lx, ly) of
    (Lift x, Lift y) -> c x y
    (Lift _, Bottom) -> GT
    (Bottom, Lift _) -> LT
    (Bottom, Bottom) -> EQ

instance Ord1 Op where
  liftCompare c x y = c (getOp x) (getOp y)

instance Ord1 Ordered where
  liftCompare c x y = c (getOrdered x) (getOrdered y)

instance Show1 Dropped where
  liftShowsPrec sp _ _ dx = case dx of
    Drop x -> showString "Drop " . sp 11 x
    _      -> showString "Top"

instance Show1 Lifted where
  liftShowsPrec sp _ _ lx = case lx of
    Lift x -> showString "Lift " . sp 11 x
    _      -> showString "Bottom"

instance Show1 Ordered where
  liftShowsPrec sp _ d = sp d . getOrdered

instance Show1 Op where
  liftShowsPrec sp _ d = sp d . getOp 

instance Hashable1 Dropped

instance Hashable1 Lifted

instance Hashable1 Ordered

instance Hashable1 Op