module Main where

import Control.Monad (filterM, forM)
import Criterion.Main

import ReducedProduct.CEval12K qualified as CEval12K
import ReducedProduct.CEval1K qualified as CEval1K
import ReducedProduct.CEval2K qualified as CEval2K
import ReducedProduct.CEval4K qualified as CEval4K
import ReducedProduct.CEval8K qualified as CEval8K
import ReducedProduct.FixenNoPriorities qualified as NoPriorities
import ReducedProduct.FixenWithPriorities qualified as WithPriorities
import ReducedProduct.Hand qualified as Hand

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "CEval (1K)"
        [ bench "Hand" $ nf Hand.solveHand CEval1K.handTest
        , bench "Fixen (No Priorities)" $ nf NoPriorities.solve CEval1K.noPrioritiesTest
        , bench "Fixen (With Priorities)" $ nf WithPriorities.solve CEval1K.withPrioritiesTest
        ]
    , bgroup
        "CEval (2K)"
        [ bench "Hand" $ nf Hand.solveHand CEval2K.handTest
        , bench "Fixen (No Priorities)" $ nf NoPriorities.solve CEval2K.noPrioritiesTest
        , bench "Fixen (With Priorities)" $ nf WithPriorities.solve CEval2K.withPrioritiesTest
        ]
    , bgroup
        "CEval (4K)"
        [ bench "Hand" $ nf Hand.solveHand CEval4K.handTest
        , bench "Fixen (No Priorities)" $ nf NoPriorities.solve CEval4K.noPrioritiesTest
        , bench "Fixen (With Priorities)" $ nf WithPriorities.solve CEval4K.withPrioritiesTest
        ]
    , bgroup
        "CEval (8K)"
        [ bench "Hand" $ nf Hand.solveHand CEval8K.handTest
        , bench "Fixen (No Priorities)" $ nf NoPriorities.solve CEval8K.noPrioritiesTest
        , bench "Fixen (With Priorities)" $ nf WithPriorities.solve CEval8K.withPrioritiesTest
        ]
    , bgroup
        "CEval (12K)"
        [ bench "Hand" $ nf Hand.solveHand CEval12K.handTest
        , bench "Fixen (No Priorities)" $ nf NoPriorities.solve CEval12K.noPrioritiesTest
        , bench "Fixen (With Priorities)" $ nf WithPriorities.solve CEval12K.withPrioritiesTest
        ]
    ]
