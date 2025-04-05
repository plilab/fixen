import Prettyprinter
import Prettyprinter.Render.Text
import Syntax.Raw
import Parsing (parseTopLevel)
import Compiler.GenerateUniqueNames (makeUniqueNames)
import Compiler.Compactify (buildRuleForest)
import Compiler.ExplicateConstraints (explicateForest)

testProg = parseTopLevel "\
    \lat Dist\n\
    \rel edge : Nat, Nat, Dist\n\
    \rel distTo : Nat, Dist\n\
    \rel start: Nat\n\
    \rule init: start s |- distTo s 0\n\
    \rule addDist:  distTo v1 d1, edge v1 v2 d2 |- distTo v2 (add d1 d2)\n\
    \rule addDist0: start s, edge s v1 d1 |- distTo v1 d1\n\
    \ord: leq d11 d12 |- addDist {d1 = d11} <= addDist {d1 = d12}\n\
    \query distTo as distTo - +\n"

main :: IO ()
main = do 
  case testProg of
    Right raw -> do
      putPretty raw
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
      putStrLn "forest' out"

    Left err -> print err
  where
    putPretty :: (Pretty a) => a -> IO ()
    putPretty a = putDoc (pretty a) >> putStrLn "" 

    putNamedRules = print . vsep . map (\(name, rul) -> "rule" <+> maybe "_" pretty name <+> "=" <+> pretty rul)