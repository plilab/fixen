module Main where

import Criterion.Main    
import Control.Monad (filterM, forM)
 
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension, takeBaseName)
import Control.DeepSeq

import ShortestPath.Converter
  ( loadGraph
  , convertToKleen
  , convertToHandwritten
  , convertToFixedKleen
  , convertToNoPriorityKleen
  , Graph
  )
import qualified ShortestPath.HandwrittenDijkstra as HW
import qualified ShortestPath.ShortestPath as SP
import qualified ShortestPath.ShortestPathFixed as FSP
import qualified ShortestPath.SubOptHandwrittenDijkstra as SOHD
import qualified ShortestPath.ShortestPathNoPriority as SPNP

instance NFData FSP.DataBase where
    rnf (FSP.DataBase x y z a) = rnf (x, y, z, a)

instance NFData SP.DataBase where
    rnf (SP.DataBase x y z a) = rnf (x, y, z, a)

instance NFData SPNP.DataBase where
    rnf (SPNP.DataBase x y z) = rnf (x, y, z)

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


-- | Find all *.json graphs in a directory.
-- Hopefully the files are in '/benchmarks/json-graphs'
listJSON :: FilePath -> IO [FilePath]
listJSON dir = do
    ok <- doesDirectoryExist dir
    if not ok then pure []
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
            Right g  -> pure (takeBaseName fp, g)
    let toBenchmarkGroups :: (String, Graph) -> Benchmark
        toBenchmarkGroups (name, g) =
            let facts = convertToKleen g
                fixed = convertToFixedKleen g
                noPriorityFacts = convertToNoPriorityKleen g
                adj = convertToHandwritten g
            in facts `deepseq` noPriorityFacts `deepseq` adj `deepseq`
                bgroup name 
                    [ bench "Kleen solve" $
                        nf SP.compute facts
                    , bench "Fixed Kleen solve" $
                        nf FSP.compute fixed
                    -- , bench "Kleen (no priority) solve" $
                    --     nf SPNP.compute noPriorityFacts
                    , bench "Handwritten Dijsktra solve" $
                        nf (SOHD.dijkstra "0") adj
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
--             Right g  -> pure (takeBaseName fp, g)
--     let facts = convertToKleen (snd $ head graphs)
--     let fixed = convertToFixedKleen (snd $ head graphs)
--     let hand = convertToHandwritten (snd $ head graphs)
--     print $ SP.count $ SP.compute facts
--     print $ snd $ SOHD.dijkstra "0" hand 
--     print $ FSP.count $ FSP.compute fixed