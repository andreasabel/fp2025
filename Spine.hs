module Spine where

import Control.Monad
import Control.Monad.State (StateT(StateT), evalStateT)

import Data.Maybe
import qualified Data.Graph as Graph (buildG, reachable)
import Data.List (uncons)
import qualified Data.Set as Set

import Turing hiding (main)

type List = []

-- Generate TMs that statically use all of their states and avoid state name permutations.
--
-- We first construct a TM spine by assigning the target state for the transitions from
-- A0, A1, B0, B1, ...
--
-- We maintain the following auxiliary data:
--
-- 1. Whether we made a halting transition yet.
-- 2. List of yet untargeted states, in the beginning that is [B, C, ...]
-- 3. List of reachable states, in the beginning [A] (and H depending on 1).
--
--

type Machine = [Rule]

allBeavers :: Int -> [Machine]
allBeavers n = do
  let srcs = take n states
  let tgts = srcs ++ [H]
  let sp = [ (s,i) | s <- srcs, i <- symbols ]
  forM sp \ (s, i) -> do
    t <- tgts
    o <- symbols
    d <- directions
    return ((s,i) :-> (t,o,d))


beavers :: Int -> [Machine]
beavers n = do
  sp  <- spines n
  dat <- forM sp $ (\ (_,t) -> if t == H then return (O,L) else liftM2 (,) symbols directions)
  return $ zipWith (\ ((s,i),t) (o,d) -> (s,i) :-> (t,o,d)) sp dat

sources :: Int -> [(State,Symbol)]
sources n = do
  i <- symbols
  s <- take n states
  return (s,i)

initSt :: Int -> St
initSt n = St (Just H) [A] (tail $ take n states)

type Spine = [((State,Symbol),State)]

-- stronglyConnComp :: Ord key => [(node, key, [key])] -> [SCC node]

-- | Build the reverse transition graph and check whether each node is reachable from H.
connected :: Int -> Spine -> Bool
connected n sp =
    all (\ s -> fromEnum s `Set.member` r) [0 .. n-1] -- $ take n states
  where
    h = fromEnum H
    g = Graph.buildG (0, h) $ map (\ ((s,_),t) -> (fromEnum t, fromEnum s)) sp
    r = Set.fromList $ Graph.reachable g h

data St = St
  { halting    :: Maybe State  -- ^ @Nothing@ or @Just H@.
  , reachable  :: List State   -- ^ Non-empty list, initialized to the start state @A@.
  , untargeted :: List State   -- ^ Opposite of @reachable@, initialized to @[B,...,E]@ (for the 5 state beaver).
  } deriving (Show)

-- -- | States that we can pick as target.
-- states :: St -> [State]
-- states (St h r u) = maybeToList (listToMaybe u) ++ r ++ maybeToList h

-- | Can we end the spine construction in this state?
--   We have to have targeted all states and made a halting transition.
finalized :: St -> Bool
finalized = \case
  St Nothing _ [] -> True
  _ -> False

-- pick :: St -> [(State, St)]
pick :: StateT St List State
pick = StateT \ st@(St h r u) -> concat
  [ maybe [] (\ (s, ss) -> [(s, St h (s:r) ss)]) (uncons u)
  , map (, st) r
  , maybe [] (\ H -> [(H, St Nothing r u)]) h
  ]

spines :: Int -> [Spine]
spines n = filter (connected n) $ evalStateT (spinesM (sources n)) (initSt n)

-- spinesM :: [(State, Symbol)] -> St -> [(Spine, St)]
spinesM :: [(State, Symbol)] -> StateT St List Spine
spinesM [] = StateT \ st -> if finalized st then [([],st)] else [] --fail
spinesM ((s,i) : dom) = do
  t <- pick
  (((s,i),t) :) <$> spinesM dom

main :: IO ()
main = forM_ [1..5] \ n -> do
  let l = length $ spines n
  putStrLn $ unwords
    [ show n
    , "\t", show l
    , "\t", show $ l * (4 `pow` (2*n - 1))
    , "\t", show $ (32 * n) `pow` n
    , "\t", show $ (4 * n + 1) `pow` (2*n)
    ]

pow b n = iterate (b *) 1 !! n


{-

1 	 2 	 8 	 32 	 25
2 	 12 	 768 	 4096 	 6561
3 	 60 	 61440 	 884736 	 4826809
4 	 280 	 4587520 	 268435456 	 6975757441
5 	 1260 	 330301440 	 104857600000 	 16679880978201
-}


-- main :: IO ()
-- main = do
--   print $ maximum $ zipWith (\ i b -> (score b, i)) [0..] (beavers 2)
