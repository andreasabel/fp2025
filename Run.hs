module Main where

import Control.Monad
import Control.Monad.State (StateT, execStateT, get, gets, put, modify)
import Control.Monad.IO.Class

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Spine (allBeavers, beavers)
import Turing hiding (run, score, main)

-- | Running for a certain number of steps
run :: Int -> [Rule] -> Int -> Config -> (Int, Config)
run fuel rls n conf
  | fuel <= 0 = (n, conf)
  | otherwise = n `seq`
      case rules rls conf of
        Nothing    -> (n, conf)
        Just conf'
          | cFallL conf' || cFallR conf' -> (-n, conf')
          | otherwise    -> run (fuel - 1) rls (n + 1) conf'

score :: Int -> [Rule] -> (Int, Config)
score fuel rs = run fuel rs 0 initConfig

data Run = Run
  { rBest  :: !Int
  , rFellL :: !Integer
  , rFellR :: !Integer
  , rLoop  :: !Integer
  , rRuns  :: !Integer
  , rStats :: IntMap Int
  }

printRun :: Run -> IO ()
printRun (Run best fellL fellR loop count stats) = do
  putStr $ unlines
    [ unwords [ "Best:       ", show best  ]
    , unwords [ "Stuck left: ", show fellL ]
    , unwords [ "Fell right: ", show fellR ]
    , unwords [ "Out of fuel:", show loop  ]
    , unwords [ "Total:      ", show count ]
    ]
  forM_ (IntMap.toList stats) $ \ (i, n) -> do
    putStrLn $ unwords [ show n, "terminated in", show i, "steps" ]

main :: IO ()
main = do
  run <- flip execStateT (Run 0 0 0 0 0 IntMap.empty) $ do
    forM_ (zip [0..] bs) \ (i, b) -> do
      let (k, conf) = score fuel b
      best <- gets rBest
      if (k < 0) then modify \ (Run best fellL fellR loop count stats) -> do
        let l = if cFallL conf then fellL + 1 else fellL
        let r = if cFallR conf then fellR + 1 else fellR
        Run best l r loop count stats
      else if k >= fuel then
        modify \ (Run best fellL fellR loop count stats) -> Run best fellL fellR (loop + 1) count stats
      else do
        modify \ (Run best fellL fellR loop count stats) -> Run best fellL fellR loop count $ IntMap.insertWith (+) k 1 stats
        when (k > best) do
          modify \ (Run _ fellL fellR loop count stats) -> Run k fellL fellR loop count stats
          liftIO $ printBeaver k i b
          get >>= liftIO . printRun
      modify \ (Run best fellL fellR loop count stats) -> Run best fellL fellR loop (count + 1) stats
  printRun run
  return ()
  where
    (n, fuel) = (5,100000) -- (4, 4000)
    bs   = beavers n
    printBeaver k i b = do
      print (k, i)
      mapM_ print b
      vizrun 200 b 0 initConfig
      return ()

--   let (k, i) = maximum $ filter (\ (n, _) -> n < fuel) $ zipWith (\ i b -> (score fuel b, i)) [0..] bs
