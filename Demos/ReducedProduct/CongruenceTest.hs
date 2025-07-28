module ReducedProduct.CongruenceTest where

{-
  use this file, invoke `Compiler.Compile.compile` on "Demos/Dataflow/sign" and 
  "Demos/Dataflow/SignAnalysis.hs" in order to produce the module `Dataflow.SignAnalysis`
-}

import ReducedProduct.CongruenceAnalysis
import ReducedProduct.Congruence

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
  mkAssign 1 "V" (Num 1), mkSeq 1 2,
  mkPhi 2, mkSeq 2 3,
  -- while V <= 10
  mkCond 3 (Leq (Id "V") (Num 9)) 4 5,
  mkAssign 4 "V" (Plus (Id "V") (Num 2)), mkSeq 4 2,
  -- if V == 11 then:
  mkCond 5 (Eq (Id "V") (Num 11)) 6 100,
  mkAssign 6 "V" (Num 0), mkSeq 6 100,
  -- end
  mkPhi 100, mkSeq 100 101,
  mkVar 101 "END"
  ]