module Main where

import Control.Monad (filterM, forM, forM_)
import Criterion.Main

import Control.DeepSeq
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- import qualified ReducedProduct.IntervalAnalysis as IG
-- import qualified ReducedProduct.IntervalHand as IH
-- import qualified ReducedProduct.IntervalTest as IT
import ShortestPath.Converter (
    Graph,
    convertToFixedKleen,
    convertToHandwritten,
    convertToKleen,
    convertToNoPriorityKleen,
    loadGraph,
 )
import qualified ShortestPath.HandwrittenDijkstra as HW
import qualified ShortestPath.ShortestPath as SP
import qualified ShortestPath.ShortestPathFixed as FSP
import qualified ShortestPath.ShortestPathNoPriority as SPNP
import qualified ShortestPath.ShortestPathNoPriorityFixed as SPNPF
import qualified ShortestPath.SubOptHandwrittenDijkstra as SOHD

instance NFData FSP.Database where
    rnf (FSP.Database x y) = rnf (x, y)

instance NFData SP.DataBase where
    rnf (SP.DataBase x y z a) = rnf (x, y, z, a)

instance NFData SPNP.DataBase where
    rnf (SPNP.DataBase x y z) = rnf (x, y, z)

instance NFData SPNPF.DataBase where
    rnf (SPNPF.DataBase x y z a) = rnf (x, y, z, a)

instance NFData SP.Fact where
    rnf (SP.StartFact s) = rnf (s)
    rnf (SP.DistToFact d) = rnf (d)
    rnf (SP.EdgeFact e) = rnf (e)

instance NFData SP.Start where
    rnf (SP.Start x) = rnf (x)

instance NFData SP.Edge where
    rnf (SP.Edge x y z) = rnf (x, y, z)

instance NFData SP.DistTo where
    rnf (SP.DistTo x y) = rnf (x, y)

instance NFData SPNP.Fact where
    rnf (SPNP.StartFact s) = rnf (s)
    rnf (SPNP.DistToFact d) = rnf (d)
    rnf (SPNP.EdgeFact e) = rnf (e)

instance NFData SPNP.Start where
    rnf (SPNP.Start x) = rnf (x)

instance NFData SPNP.Edge where
    rnf (SPNP.Edge x y z) = rnf (x, y, z)

instance NFData SPNP.DistTo where
    rnf (SPNP.DistTo x y) = rnf (x, y)

-- instance NFData IG.DataBase where
--     rnf (IG.DataBase a b c d e) = rnf (a, b, c, d, e)

-- data DataBase = DataBase{factsStateBefore ::
--                          M.HashMap Natural (S.HashSet IState),
--                          factsSeq :: S.HashSet (Natural, Natural),
--                          factsVar :: S.HashSet (Natural, String),
--                          factsCond ::
--                          M.HashMap Natural (S.HashSet (Expr, Natural, Natural)),
--                          factsAssign :: M.HashMap (Natural, String) (S.HashSet Expr)}
--                   deriving (Show, Eq)

{- | Find all *.json graphs in a directory.
Hopefully the files are in '/benchmarks/json-graphs'
-}
listJSON :: FilePath -> IO [FilePath]
listJSON dir = do
    ok <- doesDirectoryExist dir
    if not ok
        then pure []
        else do
            xs <- listDirectory dir
            let candidates = [dir </> x | x <- xs, takeExtension x == ".json"]
            filterM doesFileExist candidates

main :: IO ()
main = do
    lst <- listJSON "./benchmarks/json-graphs"
    graphs <- forM lst $ \fp -> do
        e <- loadGraph fp
        case e of
            Left err -> ioError (userError (fp <> ": " <> err))
            Right g -> pure (takeBaseName fp, g)
    let toBenchmarkGroups :: (String, Graph) -> Benchmark
        toBenchmarkGroups (name, g) =
            let facts = convertToKleen g
                fixed = convertToFixedKleen g
                noPriorityFacts = convertToNoPriorityKleen g
                adj = convertToHandwritten g
             in facts `deepseq`
                    noPriorityFacts `deepseq`
                        adj `deepseq`
                            bgroup
                                name
                                [ -- bench "Kleen solve" $
                                  --  nf SP.compute facts
                                  bench "Fixed Kleen solve" $
                                    nf FSP.solve fixed
                                , -- , bench "Kleen (no priority) (fixed) solve" $
                                  --     nf SPNPF.compute noPriorityFacts
                                  bench "Handwritten Dijsktra solve" $
                                    nf (SOHD.dijkstra "0") adj
                                    -- , bench "Handwritten Anti-Dijsktra solve" $
                                    --     nf (SOHD.dijkstraNT "0") adj
                                    -- bench "Handwritten Bellman-Ford solve" $
                                    --   nf (SOHD.dijkstraNoPQ "0") adj
                                    -- , bench "Handwritten Proper Dijkstra" $
                                    --     nf (HW.dijkstra "0") adj
                                    -- , bench "Handwritten V2 Dijkstra solve" $
                                    --     nf (HW.dijkstra "0") adj
                                ]
    defaultMain $ map toBenchmarkGroups graphs

-- main :: IO ()
-- main = do
--     lst <- listJSON "./benchmarks/json-graphs"
--     graphs <- forM lst $ \fp -> do
--         e <- loadGraph fp
--         case e of
--             Left err -> ioError (userError (fp <> ": " <> err))
--             Right g -> pure (takeBaseName fp, g)
--     forM_ graphs $ \x -> do
--         putStrLn (fst x)
--         putStrLn ""
--         let fixed = convertToFixedKleen (snd x)
--         let npfacts = convertToNoPriorityKleen (snd x)
--         let hand = convertToHandwritten (snd x)
--         putStrLn "Kleen (Fixed)"
--         print $ FSP.count $ FSP.compute fixed
--         putStrLn "Kleen (no priorities)"
--         print $ SPNPF.count $ SPNPF.compute npfacts
--         putStrLn "Handwritten"
--         print $ snd $ SOHD.dijkstra "0" hand
