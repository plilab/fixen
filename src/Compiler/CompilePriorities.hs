module Compiler.CompilePriorities where

import Syntax.Sorted
import Data.Maybe (mapMaybe)

compilePriorities :: PriorityProgram -> OrderingProgram
compilePriorities prog = let
  
  (rules', factOrds) = undefined

  in prog { ordClauses = mapMaybe getFactOrd (ordClauses prog) }
    where getFactOrd (Left clause) = Just clause
          getFactOrd _ = Nothing

{- 
  rules = { (P_i)* |- C_i } for i

  priorities = { (R_j)* |- r1[x1/y1...xn/yn] < r2[x1'/y1'...xn'/yn'] }
  

-}