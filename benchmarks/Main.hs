module Main where

import Criterion.Main    
import Control.Monad (filterM, forM)
import qualified Data.Map.Strict as M
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension, takeBaseName)

import ShortestPath.Converter
  ( loadGraph
  , convertToKleen
  , convertToHandwritten
  , Graph
  )
import qualified ShortestPath.HandwrittenDijkstra as HW
import qualified ShortestPath.ShortestPath as SP
import Criterion.Types (Benchmark(Benchmark))

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
                adj = convertToHandwritten g
            in 
            bgroup name 
                [ bench "Kleen solve" $
                    whnf SP.compute facts
                , bench "Handwritten Dijsktra solve" $
                    whnf (HW.dijkstra "0") adj
            ]
        
    defaultMain $ map toBenchmarkGroups graphs

     