module Turing where

import Control.Applicative( (<|>) )
import Data.Maybe

------------------------------------------------------------------
-- a Symbol is what is written on the tape
-- a State is the value of the internal state of a machine

data Symbol = O | I deriving ( Eq, Ord )
data State  = A | B | C | D | E | F | H deriving ( Eq, Ord, Show, Enum, Bounded )

instance Show Symbol where
  show O = "-"
  show I = "x"

inv :: Symbol -> Symbol
inv O = I
inv I = O

symbols :: [Symbol]
symbols = [O, I]

states :: [State]
states = [minBound .. maxBound]

------------------------------------------------------------------
-- a Tape is a pair of lists
-- the head is at the first element of the second list
-- the first list is reversed

type Tape = ([Symbol],[Symbol])

tapeSize :: Tape -> Int
tapeSize (xs, ys) = length xs + length ys

tape0 :: Tape
tape0 = ([], [])

look :: Tape -> Maybe Symbol
look (_, x:_) = Just x
look (_, [] ) = Nothing

write :: Symbol -> Tape -> Tape
write x (ls, [])   = (ls, [x])
write x (ls, _:rs) = (ls, x:rs)

-- we can move (L)eft or (R)ight on a Tape

data Dir = L | R
  deriving ( Eq, Ord, Show )

directions :: [Dir]
directions = [L, R]

move :: Dir -> Tape -> Tape
move L (  [],   rs) = (  [],   rs) -- bouncing
move L (x:ls,   rs) = (  ls, x:rs)
move R (  ls, x:rs) = (x:ls,   rs)
move R (  ls,   []) = (O:ls,   [])

------------------------------------------------------------------
-- a Config is a pair of a state and a tape

data Config = Config
  { cState :: !State
  , cTape  :: !Tape
  , cBumpL :: !Int  -- ^ Consecutive hits of the left wall.
  , cExtR  :: !Int  -- ^ Consecutive extensions of the tape on the right.
  }

-- a Rule describes what should happen if
-- we are in a given state and read a given symbol;
-- we get a new state, a new symbol, and a direction

data Rule = (State, Symbol) :-> (State, Symbol, Dir)
  deriving ( Eq, Ord, Show )

rule :: Rule -> Config -> Maybe Config
rule ((s0,x0) :-> (s1,x1,d)) (Config s tape nl nr)
  | s0 == s
  , let mx = look tape
  , x0 == fromMaybe O mx
  = Just $ Config s1 (move d $ write x1 tape)
      (if null (fst tape) then nl + 1 else 0)
      (if isNothing mx then nr + 1 else 0)
  | otherwise = Nothing

rules :: [Rule] -> Config -> Maybe Config
rules rls conf = foldr (<|>) Nothing [ rule r conf | r <- rls ]

------------------------------------------------------------------
-- running a set of rules to completion

run :: [Rule] -> Int -> Config -> (Int, Config)
run rls n conf = n `seq` case rules rls conf of
                           Nothing    -> (n, conf)
                           Just conf' -> run rls (n+1) conf'

vizrun :: Int -> [Rule] -> Int -> Config -> IO (Int, Config)
vizrun w rls n conf@(Config s (ls, rs) _ _) =
  n `seq`
  do putStrLn $ take w
       $ concat [ " " ++ show x ++ " " | x <- reverse ls ]
      ++ show s
      ++ concat [ show x ++ "  " | x <- rs ]
     case rules rls conf of
       Nothing    -> return (n, conf)
       Just conf' -> vizrun w rls (n+1) conf'

score :: [Rule] -> Int
score rs = fst $ run rs 0 initConfig

initConfig :: Config
initConfig = Config A tape0 0 0

------------------------------------------------------------------

main :: IO ()
main = vizrun 80 example 0 initConfig >>= print . fst

exampleKoen :: [Rule]
exampleKoen =
  [ (A,O) :-> (B,I,R)
  , (A,I) :-> (B,O,R)
  , (B,O) :-> (C,O,R)
  , (B,I) :-> (H,O,R)
  , (C,O) :-> (C,I,L)
  , (C,I) :-> (A,I,R)
  ]

-- 3 states, 45 steps
example3 :: [Rule]
example3 =
  [ (A,O) :-> (B,O,L)
  , (B,O) :-> (C,I,R)
  , (C,O) :-> (A,I,L)
  , (A,I) :-> (C,I,L)
  , (B,I) :-> (H,O,L)
  , (C,I) :-> (C,O,R)
  ]

-- 4 states,
example4' :: [Rule]
example4' =
  [ (A,O) :-> (B,I,L)
  , (B,O) :-> (C,I,L)
  , (C,O) :-> (D,O,L)
  , (D,O) :-> (A,I,L)
  , (A,I) :-> (A,I,R)
  , (B,I) :-> (B,O,R)
  , (C,I) :-> (H,O,L)
  , (D,I) :-> (B,O,R)
  ]

example4_959 :: [Rule]
example4_959 =
  [ (A,O) :-> (B,O,R)
  , (B,O) :-> (B,I,L)
  , (C,O) :-> (A,O,R)
  , (D,O) :-> (H,O,L)
  , (A,I) :-> (A,I,R)
  , (B,I) :-> (C,O,L)
  , (C,I) :-> (D,I,L)
  , (D,I) :-> (A,O,R)
  ]

-- 4 states, 3544
example4 :: [Rule]
example4 =
  [ (A,O) :-> (B,O,R)
  , (B,O) :-> (C,O,R)
  , (C,O) :-> (C,I,L)
  , (D,O) :-> (H,O,L)
  , (A,I) :-> (B,I,R)
  , (B,I) :-> (D,O,R)
  , (C,I) :-> (A,O,L)
  , (D,I) :-> (A,O,R)
  ]

-- 5 states, 3240 steps
example :: [Rule]
example =
  [ (A,O) :-> (B,I,R)
  , (B,O) :-> (C,O,R)
  , (C,O) :-> (D,O,R)
  , (D,O) :-> (E,O,L)
  , (E,O) :-> (E,I,L)
  , (A,I) :-> (E,I,L)
  , (B,I) :-> (E,I,R)
  , (C,I) :-> (B,O,R)
  , (D,I) :-> (H,O,L)
  , (E,I) :-> (A,O,L)
  ]

------------------------------------------------------------------
