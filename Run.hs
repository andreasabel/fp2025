module Main where

import Control.Monad
import Control.Monad.State (StateT, execStateT, get, gets, put, modify)
import Control.Monad.IO.Class

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
          | cFallR conf' -> (-n, conf')
          | otherwise    -> run (fuel - 1) rls (n + 1) conf'

score :: Int -> [Rule] -> Int
score fuel rs = fst $ run fuel rs 0 initConfig

data Run = Run
  { rBest :: !Int
  , rFell :: !Integer
  , rRuns :: !Integer
  }

main :: IO ()
main = do
  Run best fellR count <- flip execStateT (Run 0 0 0) $ do
    forM_ (zip [0..] bs) \ (i, b) -> do
      let k = score fuel b
      best <- gets rBest
      when (k < 0) $ modify \ (Run best fellR count) -> Run best (fellR + 1) count
      when (k < fuel && k > best) do
        modify \ (Run _ fellR count) -> Run k fellR count
        liftIO $ printBeaver k i b
      modify \ (Run best fellR count) -> Run best fellR (count + 1)
  putStr $ unlines
    [ unwords [ "Best:      ", show best ]
    , unwords [ "Fell right:", show fellR ]
    , unwords [ "Total:     ", show count ]
    ]
  return ()
  where
    (n, fuel) = (3,50) -- (4, 4000)
    bs   = beavers n
    printBeaver k i b = do
      print (k, i)
      mapM_ print b
      vizrun 200 b 0 initConfig
      return ()

--   let (k, i) = maximum $ filter (\ (n, _) -> n < fuel) $ zipWith (\ i b -> (score fuel b, i)) [0..] bs
