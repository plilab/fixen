import Compiler.Compile (compileStr)

testProg :: String
testProg = "\
    \module ShortestPath where\n\
    \import Given\n\
    \data Dist\n\
    \rel edge : String, String, Dist\n\
    \rel distTo : String, Dist\n\
    \rel start: String\n\
    \rule init: start s |- distTo s (DistNat 0)\n\
    \rule addDist:  distTo v1 d1, edge v1 v2 d2 |- distTo v2 (add d1 d2)\n\
    \ord: leq d11 d12 |- addDist {d1 = d11} <= addDist {d1 = d12}\n\
    \query distTo as distTo - +\n\
    \query distTo as closerThan + -\n\
    \query distTo as reachableIn - -\n\
    \query distTo as enumDistTo + +"

main :: IO ()
main = compileStr testProg "examples/ShortestPath.hs"
      {- putPretty raw
      let prog = sortRawProgram raw
      putPretty prog
      let ruls = makeUniqueNames $ rules prog
      putNamedRules ruls
      print $ vsep $ map pretty $ signatures prog
      let forest = buildRuleForest (signatures prog) (map snd ruls)
      putPretty forest
      putStrLn "forest out"
      let forest' = explicateForest forest
      putPretty forest'
      putStrLn "forest' out" -}
{-       modul <- raw &
        generateProgram . 
        explicateConstraints .
        compactify .
        generateUniqueNames .
        sortRawProgram
      putStrLn $ prettyPrint modul

    Left err -> print err -}
{-   where
    putPretty :: (Pretty a) => a -> IO ()
    putPretty a = putDoc (pretty a) >> putStrLn "" 

    putNamedRules :: [(Identifier, RuleClause)] -> IO ()
    putNamedRules = print . vsep . map (\(name, rul) -> "rule" <+> maybe "_" pretty name <+> "=" <+> pretty rul) -}