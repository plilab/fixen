module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad (filterM, forM_, when)
import Criterion.Main
import GHC.Clock
import ShortestPath.Converter (
  Graph,
  convertToFixenNoPriorities,
  convertToFixenWithPriorities,
  convertToHand,
  loadGraph,
 )
import ShortestPath.FixenNoPriorities qualified as NoPriorities
import ShortestPath.FixenWithPriorities qualified as WithPriorities
import ShortestPath.Hand qualified as Hand
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Mem

listJSON :: FilePath -> IO [FilePath]
listJSON dir = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      xs <- listDirectory dir
      let candidates = [dir </> x | x <- xs, takeExtension x == ".json"]
      filterM doesFileExist candidates

-- main :: IO ()
-- main = do
--   lst <- listJSON "./benchmarks/ShortestPath/json-graphs"
--   print defaultConfig
--   forM_ lst $ \fp -> do
--     performBlockingMajorGC
--     e <- loadGraph fp
--     case e of
--       Left err -> ioError (userError (fp <> ": " <> err))
--       Right g -> do
--         putStrLn (takeBaseName fp)
--         handAdj <- evaluate $ force $ convertToHand g
--         start <- getMonotonicTimeNSec
--         handRes <- evaluate $ force $ Hand.dijkstraQueueOpt "0" handAdj
--         end <- handRes `deepseq` getMonotonicTimeNSec
--         let hand_time :: Double = fromIntegral (end - start) / (10 ^ 9)
--         -- re-run a second time in case original graph loading messed with time
--         handAdj2 <- evaluate $ force $ convertToHand g
--         start2 <- getMonotonicTimeNSec
--         handRes2 <- evaluate $ force $ Hand.dijkstraQueueOpt "0" handAdj2
--         end2 <- handRes2 `deepseq` getMonotonicTimeNSec
--         let hand_time2 :: Double = fromIntegral (end2 - start2) / (10 ^ 9)
--         putStrLn $ "Hand: " ++ show hand_time2
--         performBlockingMajorGC
--         when (hand_time < 2) $ do
--           noPrioritiesAdj <- evaluate $ force $ convertToFixenNoPriorities g
--           np_start <- getMonotonicTimeNSec
--           npRes <- evaluate $ force $ NoPriorities.reSolve noPrioritiesAdj [NoPriorities.DistTo "0" 0]
--           np_end <- npRes `deepseq` getMonotonicTimeNSec
--           putStrLn $ "No Priorities: " ++ show (fromIntegral (np_end - np_start) / (10 ^ 9) :: Double)
--         performBlockingMajorGC
--         withPrioritiesAdj <- evaluate $ force $ convertToFixenWithPriorities g
--         wp_start <- getMonotonicTimeNSec
--         wpRes <- evaluate $ force $ WithPriorities.reSolve withPrioritiesAdj [WithPriorities.DistTo "0" 0]
--         wp_end <- wpRes `deepseq` getMonotonicTimeNSec
--         putStrLn $ "\rWith Priorities: " ++ show (fromIntegral (wp_end - wp_start) / (10 ^ 9) :: Double)

main :: IO ()
main = do
  lst <- listJSON "./benchmarks/ShortestPath/json-graphs"
  defaultMain $
    (mkHandBGroup <$> lst)
      ++ (mkWPBGroup <$> lst)
      ++ ( mkNPBGroup
             <$> filter
               ( (`notElem` ["FLA", "NW", "NE", "CAL", "LKS", "E", "W", "CTR"]) -- timeouts
                   . takeBaseName
               )
               lst
         )
  where
    setupHandEnv fp = do
      e <- loadGraph fp
      case e of
        Left err -> ioError (userError (fp <> ": " <> err))
        Right g -> return $ convertToHand g
    setupNPEnv fp = do
      e <- loadGraph fp
      case e of
        Left err -> ioError (userError (fp <> ": " <> err))
        Right g -> return $ convertToFixenNoPriorities g
    setupWPEnv fp = do
      e <- loadGraph fp
      case e of
        Left err -> ioError (userError (fp <> ": " <> err))
        Right g -> return $ convertToFixenWithPriorities g
    mkHandBGroup fp =
      env (setupHandEnv fp) $ \hand ->
        bgroup (takeBaseName fp) [bench "Hand" $ nf (Hand.dijkstraQueueOpt "0") hand]
    mkNPBGroup fp =
      env (setupNPEnv fp) $ \np ->
        bgroup
          (takeBaseName fp)
          [ bench "Fixen (No Priorities)" $
              nf (NoPriorities.reSolve np) [NoPriorities.DistTo "0" 0]
          ]
    mkWPBGroup fp =
      env (setupWPEnv fp) $ \wp ->
        bgroup
          (takeBaseName fp)
          [ bench "Fixen (With Priorities)" $
              nf (WithPriorities.reSolve wp) [WithPriorities.DistTo "0" 0]
          ]

-- (takeBaseName fp)
-- ( if takeBaseName fp `elem` ["NE", "CAL", "LKS", "E", "W", "CTR"]
--     then
--       [ bench "Hand" $ nf (Hand.dijkstraQueueOpt "0") hand
--       , bench "Fixen (With Priorities)" $
--           nf (WithPriorities.reSolve wp) [WithPriorities.DistTo "0" 0]
--           -- , bench "Hand (Lousy Queue)" $ nf (Hand.dijkstra "0") hand
--       ]
--     else
--       [ bench "Hand" $ nf (Hand.dijkstraQueueOpt "0") hand
--       , bench "Fixen (No Priorities)" $
--           nf (NoPriorities.reSolve np) [NoPriorities.DistTo "0" 0]
--       , bench "Fixen (With Priorities)" $
--           nf (WithPriorities.reSolve wp) [WithPriorities.DistTo "0" 0]
--           -- , bench "Hand (Lousy Queue)" $ nf (Hand.dijkstra "0") hand
--       ]
-- )

--
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
--                   [ bench "Hand" $ nf (Hand.dijkstraQueueOpt "0") handAdj
--                   , bench "Fixen (No Priorities)" $
--                       nf (NoPriorities.reSolve noPrioritiesAdj) [NoPriorities.DistTo "0" 0]
--                   , bench "Fixen (With Priorities)" $
--                       nf (WithPriorities.reSolve withPrioritiesAdj) [WithPriorities.DistTo "0" 0]
--                   , bench "Hand (Lousy Queue)" $ nf (Hand.dijkstra "0") handAdj
--                   ]
-- defaultMain $ map toBenchmarkGroups graphs
