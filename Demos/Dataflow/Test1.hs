module Dataflow.Test1 where

{-
  use this file, invoke `Compiler.Compile.compile` on "Demos/Dataflow/sign" and 
  "Demos/Dataflow/SignAnalysis.hs" in order to produce the module `Dataflow.SignAnalysis`
-}
{- 
import Dataflow.SignAnalysis
import Dataflow.Sign

{- the program:
  var x; var y; var z
  x := 7
  y := 2
  while y <= x
    x := x + ((-1) * y)
    y := y + 1
  end
  if x <= y then
    z := 0
  else
    z := 1
  end
-}

test :: [Fact]
test = [
  mkVar 0 "x",
  mkVar 1 "y", mkSeq 0 1,
  mkVar 2 "z", mkSeq 1 2,
  mkAssign 3 "x" (Num 7), mkSeq 2 3,
  mkAssign 4 "y" (Num 2), mkSeq 3 4,
  -- while x <= x
  mkCond 5 (Leq (Id "y") (Id "x")), mkSeq 4 5,
  mkAssign 6 "x" (Plus (Id "x") (Times (Num $ -1) (Id "y"))), mkSeq 5 6,
  mkAssign 7 "y" (Plus (Id "y") (Num 1)), mkSeq 6 7, mkSeq 7 5,
  -- end
  -- if x <= y
  mkCond 8 (Leq (Id "x") (Id "y")), mkSeq 5 8,
  mkAssign 9 "z" (Num 0), mkSeq 8 9,
  -- else 
  mkAssign 10 "z" (Num 1), mkSeq 8 10
  ] -}