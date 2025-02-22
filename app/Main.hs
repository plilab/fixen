{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prettyprinter
import Parsing (parseTopLevel)

main :: IO ()
main = do
  let res = parseTopLevel "\
    \lat Dist\n\
    \rel edge : Nat, Nat, Dist\n\
    \rel distTo : Nat, Dist\n\
    \rule init: |- distTo start 0\n\
    \rule addDist: distTo v1 d1, edge v1 v2 d2, leq (add d1 d2) d |- distTo v2 d\n\
    \ord: leq d11 d12 |- addDist {d1 = d11} <= addDist {d1 = d12}\n\
    \query distTo as distTo - +\n"
  case res of
    Right prog -> print $ pretty prog
    Left err -> print err
