module Main where

import Control.Applicative( (<|>) )

------------------------------------------------------------------
-- a Symbol is what is written on the tape
-- a State is the value of the internal state of a machine

data Symbol = O | I deriving ( Eq, Ord )
data State  = A | B | C | D | E | F | H deriving ( Eq, Ord, Show )

instance Show Symbol where
  show O = "-"
  show I = "x"

------------------------------------------------------------------
-- a Tape is a pair of lists
-- the head is at the first element of the second list
-- the first list is reversed

type Tape = ([Symbol],[Symbol])

tape0 :: Tape
tape0 = ([], repeat O)

look :: Tape -> Symbol
look (_, x:_) = x

write :: Symbol -> Tape -> Tape
write x (ls, _:rs) = (ls, x:rs)

-- we can move (L)eft or (R)ight on a Tape

data Dir = L | R
  deriving ( Eq, Ord, Show )

move :: Dir -> Tape -> Tape
move L (  [],   rs) = (  [],   rs) -- bouncing
move L (x:ls,   rs) = (  ls, x:rs)
move R (  ls, x:rs) = (x:ls,   rs)

------------------------------------------------------------------
-- a Config is a pair of a state and a tape

type Config = (State,Tape)

-- a Rule describes what should happen if
-- we are in a given state and read a given symbol;
-- we get a new state, a new symbol, and a direction

data Rule = (State, Symbol) :-> (State, Symbol, Dir)
  deriving ( Eq, Ord, Show )

rule :: Rule -> Config -> Maybe Config
rule ((s0,x0) :-> (s1,x1,d)) (s, tape)
  | s0 == s && x0 == look tape = Just (s1, move d (write x1 tape))
  | otherwise                  = Nothing

rules :: [Rule] -> Config -> Maybe Config
rules rls conf = foldr (<|>) Nothing [ rule r conf | r <- rls ] 

------------------------------------------------------------------
-- running a set of rules to completion

run :: [Rule] -> Int -> Config -> (Int, Config)
run rls n conf = n `seq` case rules rls conf of
                           Nothing    -> (n, conf)
                           Just conf' -> run rls (n+1) conf'

vizrun :: Int -> [Rule] -> Int -> Config -> IO (Int, Config)
vizrun w rls n conf@(s, (ls, rs)) =
  n `seq`
  do putStrLn $ take w
       $ concat [ " " ++ show x ++ " " | x <- reverse ls ]
      ++ show s
      ++ concat [ show x ++ "  " | x <- rs ]
     case rules rls conf of
       Nothing    -> return (n, conf)
       Just conf' -> vizrun w rls (n+1) conf'

score :: [Rule] -> Int
score rs = fst (run rs 0 (A,tape0))

------------------------------------------------------------------

main :: IO ()
main = vizrun 80 example 0 (A,tape0) >>= print . fst

example :: [Rule]
example = [ (A,O) :-> (B,I,R)
          , (A,I) :-> (B,O,R)
          , (B,O) :-> (C,O,R)
          , (B,I) :-> (H,O,R)
          , (C,O) :-> (C,I,L)
          , (C,I) :-> (A,I,R)
          ]

------------------------------------------------------------------
