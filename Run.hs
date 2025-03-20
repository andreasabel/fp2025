module Main where

import Control.Monad
import Control.Monad.State (StateT, execStateT, get, put)
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
        Just conf' -> run (fuel - 1) rls (n + 1) conf'

score :: Int -> [Rule] -> Int
score fuel rs = fst (run fuel rs 0 (A,tape0))

main :: IO ()
main = do
  flip execStateT 0 $ do
    forM_ (zip [0..] bs) \ (i, b) -> do
      let k = score fuel b
      best <- get
      when (k < fuel && k > best) do
        put k
        liftIO $ printBeaver k i
  return ()
  where
    n    = 4
    fuel = 4000
    bs   = beavers n
    printBeaver k i = do
      print (k, i)
      let b = bs !! i
      mapM_ print b
      vizrun 80 b 0 (A, tape0)
      return ()

--   let (k, i) = maximum $ filter (\ (n, _) -> n < fuel) $ zipWith (\ i b -> (score fuel b, i)) [0..] bs
