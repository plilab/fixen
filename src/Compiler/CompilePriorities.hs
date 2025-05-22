module Compiler.CompilePriorities where

import Syntax.Common
import Syntax.Sorted

compilePriorities :: PriorityProgram -> OrderingProgram
compilePriorities prog = let
  
  (rules', factOrds) = undefined

  in prog { ordClauses = factOrds }

{- 
  rules = { (P_i)* |- C_i } for i

  priorities = { (R_j)* |- r1[x1/y1...xn/yn] < r2[x1'/y1'...xn'/yn'] }
  

-}