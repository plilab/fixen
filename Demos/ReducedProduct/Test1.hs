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
  mkVar 0 "V", mkSeq 0 1,
  mkAssign 1 "V" (Num 1), mkSeq 1 2,
  mkPhi 2, mkSeq 2 3,
  -- while V <= 10
  mkCond 3 (Leq (Id "V") (Num 10)) 4 5,
  mkAssign 4 "V" (Plus (Id "V") (Num 2)), mkSeq 4 2,
  -- if V >= 12 then:
  mkCond 5 (Gte (Id "V") (Num 12)) 6 100,
  mkAssign 6 "V" (Num 0), mkSeq 6 100,
  -- end
  mkVar 100 "END"
  ]