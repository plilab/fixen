{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Compiler.CodeGen.Util where

import Data.Unique
import Data.Bifunctor (Bifunctor(first, second))
import Control.Monad.Cont (MonadIO(liftIO))

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f t = concat <$> mapM f t

concatSequence :: (Traversable t, Monad m) => t (m [a]) -> m [a]
concatSequence = (concat <$>) . sequence

foldr1Default :: (Foldable t) => (a -> a -> a) -> a -> t a -> a
foldr1Default f a ta 
  | null ta   = a
  | otherwise = foldr1 f ta

filterBy :: [Bool] -> [a] -> [a]
filterBy bs = foldr (\(b, a) -> if b then (a:) else id) [] . zip bs

partitionBy :: [Bool] -> [a] -> ([a], [a])
partitionBy bs = 
  foldr
    (\(b, a) -> (if b then first else second) (a:)) 
    ([], [])
  . zip bs

gensym :: (MonadIO m) => m String
gensym = fresh "v"

fresh :: (MonadIO m) => String -> m String
fresh v = liftIO $ showString (v ++ "_") . show . hashUnique <$> newUnique

idsTo :: Int -> [String]
idsTo = idsFromTo 0

idsFromToCount :: Int -> Int -> [String]
idsFromToCount n m = idsFromTo n (n + m)

idsFromTo :: Int -> Int -> [String]
idsFromTo n m = map (("v" ++) . show) [n..m-1]

prime :: String -> String
prime = (++ "'")

factCon :: String -> String
factCon = (++ "Fact")

dbProj :: String -> String
dbProj = ("facts" ++)