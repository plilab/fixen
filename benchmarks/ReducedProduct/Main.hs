module Main where

import Control.Monad (filterM, forM)
import Criterion.Main

import Control.DeepSeq
import ReducedProduct.CEval
import ReducedProduct.FixenNoPriorities qualified as NoPriorities
import ReducedProduct.FixenWithPriorities qualified as WithPriorities
import ReducedProduct.Hand qualified as Hand

-- import Pareto.FixenWithPriorities qualified as WithPriorities
-- import Pareto.Hand qualified as Hand
-- import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
-- import System.FilePath (takeBaseName, takeExtension, (</>))

-- listJSON :: FilePath -> IO [FilePath]
-- listJSON dir = do
--   ok <- doesDirectoryExist dir
--   if not ok
--     then pure []
--     else do
--       xs <- listDirectory dir
--       let candidates = [dir </> x | x <- xs, takeExtension x == ".json"]
--       filterM doesFileExist candidates

main :: IO ()
main = do
  defaultMain $
    deepseq noPrioritiesTest $
      [ bgroup
          "ReducedProduct"
          [ bench "Hand" $ nf Hand.solveHand handTest
          , bench "Fixen (No Priorities)" $ nf NoPriorities.solve noPrioritiesTest
          , bench "Fixen (With Priorities)" $ nf WithPriorities.solve withPrioritiesTest
          ]
      ]

-- lst <- listJSON "./benchmarks/Pareto/json-graphs"
-- graphs <- forM lst $ \fp -> do
--   e <- loadGraph fp
--   case e of
--     Left err -> ioError (userError (fp <> ": " <> err))
--     Right g -> pure (takeBaseName fp, g)
-- let toBenchmarkGroups :: (String, Graph) -> Benchmark
--     toBenchmarkGroups (name, g) =
--       let handAdj = convertToHand g
--           noPrioritiesAdj = convertToFixenNoPriorities g
--           withPrioritiesAdj = convertToFixenWithPriorities g
--        in handAdj `deepseq`
--             noPrioritiesAdj `deepseq`
--               withPrioritiesAdj `deepseq`
--                 bgroup
--                   name
--                   [ bench "Hand" $ nf (Hand.pareto "1") handAdj
--                   , bench "Fixen (No Priorities)" $
--                       nf (NoPriorities.reSolve noPrioritiesAdj) [NoPriorities.DistTo "1" 0 0]
--                   , bench "Fixen (With Priorities)" $
--                       nf (WithPriorities.reSolve withPrioritiesAdj) [WithPriorities.DistTo "1" 0 0]
--                   ]
-- defaultMain $ map toBenchmarkGroups graphs

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
