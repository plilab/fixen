{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Fixen.IR.Core.Annotations (
  -- * Annotations
  -- $ann
  GetAnnotation (..),
  SetAnnotation (..),
  getAnnotationOf,
  setAnnotationOf,
) where

import Fixen.Data.AlaCarte

--------------------------------------------------------------------------------
--
-- Annotations

-- $ann
-- The core building blocks and program constructs are annotated. These classes
-- provide useful utility for retrieving and modifying them.

-- | A type that can obtain an annotation
class GetAnnotation ann e | e -> ann where
  -- | Get the annotation
  getAnnotation :: e -> ann

-- | The 'SetAnnotation' class witnesses the ability to set the annotation for
-- a type, potentially yielding a different type.
class SetAnnotation ann x y | ann x -> y where
  -- | Set the annotation
  setAnnotation
    :: ann
    -- ^ The annotation
    -> x
    -- ^ The term whose annotation is to be modified
    -> y

instance (GetAnnotation ann l, GetAnnotation ann r) => GetAnnotation ann (l :+: r) where
  getAnnotation (Left x) = getAnnotation x
  getAnnotation (Right x) = getAnnotation x

instance (SetAnnotation ann l l', SetAnnotation ann r r') => SetAnnotation ann (l :+: r) (l' :+: r') where
  setAnnotation ann (Left x) = Left (setAnnotation ann x)
  setAnnotation ann (Right x) = Right (setAnnotation ann x)

instance (GetAnnotation ann (f a), GetAnnotation ann (g a)) => GetAnnotation ann ((f ::+:: g) a) where
  getAnnotation (Inl x) = getAnnotation x
  getAnnotation (Inr x) = getAnnotation x

instance (SetAnnotation ann (f a) (f' a'), SetAnnotation ann (g a) (g' a')) => SetAnnotation ann ((f ::+:: g) a) ((f' ::+:: g') a') where
  setAnnotation ann (Inl x) = Inl (setAnnotation ann x)
  setAnnotation ann (Inr x) = Inr (setAnnotation ann x)

instance (GetAnnotation ann (f (Fixpoint f))) => GetAnnotation ann (Fixpoint f) where
  getAnnotation (Fixpoint x) = getAnnotation x

instance (SetAnnotation ann (f (Fixpoint f)) (f' (Fixpoint f'))) => SetAnnotation ann (Fixpoint f) (Fixpoint f') where
  setAnnotation ann (Fixpoint x) = Fixpoint (setAnnotation ann x)

-- | Since annotations are frequently composed with products, this convenience
-- function retrieves and focuses of exactly one component of the larger
-- annotation.
getAnnotationOf :: forall target group e. (group :>: target, GetAnnotation group e) => e -> target
getAnnotationOf = (↓) . getAnnotation

-- | Since annotations are frequently composed with products, this convenience
-- function focuses and sets exactly one component of the larger
-- annotation.
setAnnotationOf
  :: forall target group e e'
   . (group :>: target, SetAnnotation group e e', GetAnnotation group e)
  => target
  -- ^ The annotation
  -> e
  -- ^ The term whose annotation is to be modified
  -> e'
setAnnotationOf ann e =
  let old_ann_group = getAnnotation e
      new_ann_group = old_ann_group *<-: ann
  in  setAnnotation new_ann_group e
