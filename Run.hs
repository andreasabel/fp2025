module Main where

import Control.Monad
import Control.Monad.State (StateT, execStateT, get, gets, put, modify)
import Control.Monad.IO.Class

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Data.Time
import Data.Time.Format

import Spine (allBeavers, beavers)
import Turing hiding (run, score, main)

-- | Running for a certain number of steps
run :: [Rule] -> Int -> Int -> Config -> (Int, Config)
run rls fuel0 = loop fuel0
  where
    loop fuel n conf
      | fuel <= 0 = (n, conf)
      | otherwise = n `seq`
          case rules rls conf of
            Nothing    -> (n, conf)
            Just conf'@(Config _ _ nl nr)
              | nl >= numRules             -> kill
              | nr >= numRules             -> kill
              | n `mod` check == 0, exceedsMaxSteps ns conf' n -> kill
              | otherwise -> loop (fuel - 1) (n + 1) conf'
              where
                kill = (-n, conf')
    numRules = length rls
    ns = numRules `div` 2
    check = max 1 (fuel0 `div` 100)

exceedsMaxSteps :: Int -> Config -> Int -> Bool
exceedsMaxSteps n conf steps = loop l (steps `div` n `div` (l + 1))
  where
    l = tapeSize (cTape conf)
    loop m 0 = False
    loop 0 s = True
    loop m s = loop (m-1) (s `div` 2)

score :: Int -> [Rule] -> (Int, Config)
score fuel rs = run rs fuel 0 initConfig

data Run = Run
  { rBest  :: !Int
  , rFellL :: !Integer
  , rFellR :: !Integer
  , rExceed:: !Integer
  , rLoop  :: !Integer
  , rRuns  :: !Integer
  , rStats :: IntMap Int
  }

printRun :: Run -> IO ()
printRun (Run best fellL fellR exceed loop count stats) = do
  putStr $ unlines
    [ unwords [ "Best:          ", show best   ]
    , unwords [ "Stuck left:    ", show fellL  ]
    , unwords [ "Fell right:    ", show fellR  ]
    , unwords [ "Too many steps:", show exceed ]
    , unwords [ "Out of fuel:   ", show loop   ]
    , unwords [ "Total:         ", show count  ]
    ]
  forM_ (IntMap.toList stats) $ \ (i, n) -> do
    putStrLn $ unwords [ show n, "terminated in", show i, "steps" ]

curTime :: MonadIO m => m String
curTime = liftIO do
    currentTime <- getCurrentTime
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime

printTime :: MonadIO m => m ()
printTime = liftIO do
  putStrLn =<< curTime

main :: IO ()
main = do
  run <- flip execStateT (Run 0 0 0 0 0 0 IntMap.empty) $ do
    forM_ (zip [ndrop ..] bs) \ (i, b) -> do
      when (i `mod` 100000 == 0) $ liftIO do
        ct <- curTime
        putStrLn $ unwords [ ct, show i ]
      when (i `mod` 1000000 == 0) do
        get >>= liftIO . printRun
      let (!k, !conf@(Config _ _ nl nr)) = score fuel b
      let tape = tapeSize (cTape conf)
      best <- gets rBest
      if (k < 0) then modify \ (Run best fellL fellR exceed loop count stats) -> do
        let l = if nl > 1 then fellL + 1 else fellL
        let r = if nr > 1 then fellR + 1 else fellR
        let e = if exceedsMaxSteps n conf (0 - k) then exceed + 1 else exceed
        Run best l r e loop count stats
      else if k >= fuel then
        modify \ (Run best fellL fellR exceed loop count stats) -> Run best fellL fellR exceed (loop + 1) count stats
      else do
        modify \ (Run best fellL fellR exceed loop count stats) -> Run best fellL fellR exceed loop count $ IntMap.insertWith (+) k 1 stats
        when (k > best) do
          modify \ (Run _ fellL fellR exceed loop count stats) -> Run k fellL fellR exceed loop count stats
          liftIO $ printBeaver k i b tape
          get >>= liftIO . printRun
          printTime
      modify \ (Run best fellL fellR exceed loop count stats) -> Run best fellL fellR exceed loop (count + 1) stats
  printRun run
  return ()
  where
    (n, fuel, ndrop) = (5, 100000, 100000000) -- (4, 10000, 0) -- (3,100,0) --
    bs = drop ndrop $ beavers n
    printBeaver k i b tape = do
      putStrLn $ unwords [ "Candidate nr.", show i ]
      mapM_ print b
      vizrun 200 b 0 initConfig
      putStr $ unlines
        [ unwords [ "Steps:    ", show k    ]
        , unwords [ "Tape used:", show tape ]
        ]
      return ()

--   let (k, i) = maximum $ filter (\ (n, _) -> n < fuel) $ zipWith (\ i b -> (score fuel b, i)) [0..] bs
