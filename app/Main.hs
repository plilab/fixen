module Main (main) where

import ShortestPath.Converter (loadGraph, convertToKleen, convertToHandwritten)

main :: IO ()
main = do
  result <- loadGraph "/Users/briancheong/repos/fixed-point/Demos/ShortestPath/test.json"
  case result of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right graph -> do
      putStrLn "=== Kleen Facts ==="
      mapM_ print (convertToKleen graph)

      putStrLn "\n=== Handwritten adjacency ==="
      print (convertToHandwritten graph)

