module ReducedProduct.IntervalTest where

{-
  use this file, invoke `Compiler.Compile.compile` on "Demos/Dataflow/sign" and 
  "Demos/Dataflow/SignAnalysis.hs" in order to produce the module `Dataflow.SignAnalysis`
-}

import ReducedProduct.Common
import ReducedProduct.IntervalAnalysis
import ReducedProduct.Interval

{- the program:
Example 6.3 from Tutorial on Static Inference of Numeric Invariants by
Abstract Interpretation
by Antoine Miné
1 : V ← 1;                                              -- [1, 1]
2 : while V ≤ 9 do V ← V + 2 done;                      -- [1, 11]
3 : if V == 11 then                                     -- [10, 11]
4 :     V ← 0
5 : endif
-}

test :: [Fact]
test = [
  -- V = 1
  mkVar 0 "V", mkSeq 0 1,
  mkAssign 2 "V" (Num 1), mkSeq 2 4,
  -- while V <= 10
  mkCond 4 (Leq (Id "V") (Num 9)) 5 6,
  mkAssign 5 "V" (Plus (Id "V") (Num 2)), mkSeq 5 4,
  -- if V == 11 then:
  mkCond 6 (Eq (Id "V") (Num 11)) 7 101,
  mkAssign 7 "V" (Num 0), mkSeq 7 101,
  -- end
  mkVar 101 "END"
  ]