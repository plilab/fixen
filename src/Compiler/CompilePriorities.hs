module Compiler.CompilePriorities where

import Syntax.Sorted

compilePriorities :: Program -> Program
compilePriorities prog = let
  
  (rules', priorities) = undefined

  in prog

{-
  rules = { (P_i)* |- C_i } for i

  priorities = { (R_j)* |- r1[x1/y1...xn/yn] < r2[x1'/y1'...xn'/yn'] }


-}