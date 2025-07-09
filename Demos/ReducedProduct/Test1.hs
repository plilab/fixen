module ReducedProduct.Test1 where

{-
  use this file, invoke `Compiler.Compile.compile` on "Demos/Dataflow/sign" and 
  "Demos/Dataflow/SignAnalysis.hs" in order to produce the module `Dataflow.SignAnalysis`
-}

import ReducedProduct.IntervalAnalysis
import ReducedProduct.Interval

{- the program:
Example 6.3 from Tutorial on Static Inference of Numeric Invariants by
Abstract Interpretation
by Antoine Miné
1 : V ← 1;
2 : while V ≤ 10 do V ← V + 2 done;
3 : if V ≥ 12 then
4 :     V ← 0
5 : endif
-}

test :: [Fact]
test = [
  -- V = 1
  mkVar 0 "V",
  mkAssign 1 "V" (Num 1), mkSeq 0 1,
  -- while V <= 10
  mkCond 2 (Leq (Id "V") (Num 10)) 3 100, mkSeq 1 2,
  mkAssign 3 "V" (Plus (Id "V") (Num 2)), mkSeq 3 2,
  -- end
  mkVar 100 "END"
  ]