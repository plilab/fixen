module ReducedProduct.IsEvenTest where

{-
  use this file, invoke `Compiler.Compile.compile` on "Demos/Dataflow/sign" and 
  "Demos/Dataflow/SignAnalysis.hs" in order to produce the module `Dataflow.SignAnalysis`
-}

import ReducedProduct.IsEvenAnalysis
import ReducedProduct.IsEven

{- the program:
Example 6.3 from Tutorial on Static Inference of Numeric Invariants by
Abstract Interpretation
by Antoine Miné
my number = nZ + y (Z is integer)                       -- [isEven]
1 : V ← 1;                                              -- [Odd]
2 : while V ≤ 9 do V ← V + 2 done;                      -- [Odd]   [10, 11] -> [11, 11]
3 : if V == 11 then                                     -- [11, 11]
4 :     V ← 0
5 : endif
-}

test :: [Fact]
test = [
  -- V = 1
  mkVar 0 "V", mkSeq 0 1,
  mkPhi 1, mkSeq 1 2,
  mkAssign 2 "V" (Num 1), mkSeq 2 3,
  mkPhi 3, mkSeq 3 4,
  -- while V <= 10
  mkCond 4 (Leq (Id "V") (Num 9)) 5 6,
  mkAssign 5 "V" (Plus (Id "V") (Num 2)), mkSeq 5 3,
  -- if V == 11 then:
  mkCond 6 (Eq (Id "V") (Num 11)) 7 100,
  mkAssign 7 "V" (Num 0), mkSeq 7 8,
  mkPhi 8, mkSeq 8 100,
  -- end
  mkPhi 100, mkSeq 100 101,
  mkVar 101 "END"
  ]