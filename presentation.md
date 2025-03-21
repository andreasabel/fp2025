---
title: Busy Beaver on a One-Sided Tape
author: Andreas Abel
date: 2025-03-21
---

Puzzle posed by Koen Claessen during the 2025 Chalmers/GU retreat of the **Functional Programming** group of the Department of Computer Science and Engineering, held 19-21 March 2025 in Nääs Fabriken (Tollered).

https://github.com/koengit/fp2025

Consider Turing Machine with a one-side infinite tape.
You can walk as far to the right as you want.
Yet if you are at the beginning of the tape and try to go further left, you just stay where you are.

**Find busy beavers with up to 6 states** (6 is already hopeless...).

There was a 48 hour window to post entries.

In the end, I beat Ulf Norell (~40000 steps) by a small margin with a 5 state beaver that runs for 71076 steps.
```haskell
winner :: [Rule]
winner =
  [ (A,O) :-> (B,O,L)
  , (B,O) :-> (C,O,R)
  , (C,O) :-> (D,I,R)
  , (D,O) :-> (E,I,R)
  , (E,O) :-> (E,I,L)
  , (A,I) :-> (D,O,R)
  , (B,I) :-> (E,O,L)
  , (C,I) :-> (C,O,L)
  , (D,I) :-> (H,O,L)
  , (E,I) :-> (A,O,L)
  ]
```
(The busiest beaver will likely run for 100 million steps!)

Enumerating Beavers
===================

Number of Beavers (Turing machines) with _n_ states `A,B,...` and a halting state `H`:
_n×2 → 1+n×2×2_ which is _(4n + 1)²ⁿ_.
For _n = 5_ this is around 16 trillion, way too many to try all.

So let us focus on the interesting ones!

Spines
------

Criteria: Avoid permutations of state names. Use all states.

1. All states reachable from `(A,O)`.
2. Halting state `H` reachable from all states.
3. A single halting transition.

Implementation: First enumerate _spines_ `(s,i) :-> (t,_,_)`.

A priori _(n + 1)²ⁿ_ spines, for _n = 5_ this is `6¹⁰` (60 million) spines.

- Divide states in _reached from `A`_ and _not yet used_.

- When assigning each of the _2n_ targets either:
  1. pick the next _unused_ state and move it to _reached_,
  2. pick an already reached state,
  3. pick `H` and mark it as unavailable for future picks.

```haskell
-- | State of the spine constructor.
data St = St
  { halting    :: Maybe State
      -- ^ @Nothing@ or @Just H@.
  , reachable  :: List State
      -- ^ Non-empty list, initialized to the start state @A@.
  , untargeted :: List State
      -- ^ Opposite of @reachable@, initialized to @[B .. E]@ (for the 5 state beaver).
  }

-- pick :: St -> [(State, St)]
pick :: StateT St List State
pick = StateT \ st@(St h r u) -> concat
  [ maybe [] (\ (s, ss) -> [(s, St h (s:r) ss)]) (uncons u)
  , map (, st) r
  , maybe [] (\ H -> [(H, St Nothing r u)]) h
  ]
```

- When spine is complete, discard it if `H` is not reachable from some state.
  (Implemented with `Data.Graph.reachable` from `containers` package.)

For _n=5_ we are down to 300.004 spines (0.5%)!

Actions
-------

Fill rule skeleton `(s,i) :-> (t,_,_)` with actions output `o` and direction `d`.
Does not matter for halting state `t=H`.
So _(2n-1)⁴_ possibilities.
For _n = 5_ this is _4⁹ = 262.144_, so around ¼ million beavers sharing a spine.

Considered: ~~Rule out unbalanced actions (e.g. many more `I` than `O` or `L` than `R`).~~

Number of Beavers
-----------------

| n | spines |  interesting     | total beavers      |
|--:|-------:|-----------------:|-------------------:|
| 1 |      2 |               8  |                 25 |
| 2 |     22 |           1.408  |              6.561 |
| 3 |    393 |         402.432  |          4.826.809 |
| 4 |   9649 |     158.089.216  |      6.975.757.441 |
| 5 | 300004 |**78.644.248.576**| 16.679.880.978.201 |

78 billion is still a lot of candidates to try out!


Eliminating non-terminating beavers
===================================

Limit beaver to a maximum number of steps (its fuel).

Kill beaver when it:
1. runs out of fuel,
2. gets stuck on the left wall,
3. runs off to the right infinity, or
4. doesn't get anywhere.

Implementation:
1. Count down.
2. If you have hit the wall _2n_ times in a row, you'll do it forever,
   because none of the states got you away from it.
3. If you have extended the tape to the right _2n_ times in a row, you'll do that forever.
4. If you have visited only few different cells for a long time, you'll be stuck with them forever.

Ad 4.
The information of a configuration with _k_ visited tape cells is _2ᵏ·k·n_.
Breakdown: _k_ coded bits, _k_ head positions, _n_ TM states.
If your number of steps exceeds this, you are in a cycle!

To implement these early termination conditions we have to refine `Tape` and `Config`.

(Koen's implementation of the tape as `([], repeat O)` shows a very lazy programmer.)

```haskell
data Tape = Tape
  { tapeSize  :: !Int
  , tapeLeft  :: ![Symbol]
  , tapeRight :: ![Symbol]
  }

data Config = Config
  { cState :: !State
  , cTape  :: !Tape
  , cBumpL :: !Int
     -- ^ Consecutive hits of the left wall.
  , cExtR  :: !Int
     -- ^ Consecutive extensions of the tape on the right.
  }
```

Run
===

- Tried to beat Ulf's ~7000 step BB'(5).
- First found only one with ~3240 steps.
- So I set fuel to 100.000.

Search space is vast.  I guess I was lucky!
```
2025-03-21 01:31:39 GMT

Steps:     71076
Tape used: 32

Stuck left:     1122147
Fell right:     2030004
Too many steps: 525156
Out of fuel:    503033
Total:          7870656
```
We tried around 8 million candidates.
We only had to run 0.5 million to the time out (100.000 steps).
3.5 million were eliminated by our loop-checking criteria.
The others terminated regularly.

Terminating beaver breakdown at this point.
```
262144 terminated in 2 steps
245760 terminated in 3 steps
880640 terminated in 4 steps
821248 terminated in 5 steps
663296 terminated in 6 steps
393280 terminated in 7 steps
221648 terminated in 8 steps
122564 terminated in 9 steps
70428 terminated in 10 steps
34964 terminated in 11 steps
24800 terminated in 12 steps
15948 terminated in 13 steps
12309 terminated in 14 steps
8674 terminated in 15 steps
6485 terminated in 16 steps
3697 terminated in 17 steps
3301 terminated in 18 steps
2600 terminated in 19 steps
2186 terminated in 20 steps
1535 terminated in 21 steps
1149 terminated in 22 steps
895 terminated in 23 steps
802 terminated in 24 steps
582 terminated in 25 steps
647 terminated in 26 steps
539 terminated in 27 steps
671 terminated in 28 steps
517 terminated in 29 steps
295 terminated in 30 steps
197 terminated in 31 steps
199 terminated in 32 steps
201 terminated in 33 steps
156 terminated in 34 steps
158 terminated in 35 steps
129 terminated in 36 steps
103 terminated in 37 steps
124 terminated in 38 steps
174 terminated in 39 steps
101 terminated in 40 steps
53 terminated in 41 steps
59 terminated in 42 steps
82 terminated in 43 steps
40 terminated in 44 steps
40 terminated in 45 steps
50 terminated in 46 steps
51 terminated in 47 steps
32 terminated in 48 steps
53 terminated in 49 steps
98 terminated in 50 steps
33 terminated in 51 steps
30 terminated in 52 steps
44 terminated in 53 steps
22 terminated in 54 steps
87 terminated in 55 steps
19 terminated in 56 steps
27 terminated in 57 steps
22 terminated in 58 steps
31 terminated in 59 steps
19 terminated in 60 steps
27 terminated in 61 steps
21 terminated in 62 steps
71 terminated in 63 steps
12 terminated in 64 steps
28 terminated in 65 steps
6 terminated in 66 steps
14 terminated in 67 steps
13 terminated in 68 steps
11 terminated in 69 steps
4 terminated in 70 steps
4 terminated in 71 steps
11 terminated in 72 steps
14 terminated in 73 steps
8 terminated in 74 steps
1 terminated in 75 steps
1 terminated in 76 steps
2 terminated in 77 steps
1 terminated in 78 steps
10 terminated in 79 steps
4 terminated in 80 steps
5 terminated in 81 steps
5 terminated in 82 steps
4 terminated in 83 steps
5 terminated in 84 steps
7 terminated in 85 steps
2 terminated in 86 steps
2 terminated in 88 steps
1 terminated in 89 steps
4 terminated in 91 steps
5 terminated in 92 steps
3 terminated in 93 steps
5 terminated in 94 steps
4 terminated in 95 steps
1 terminated in 96 steps
1 terminated in 98 steps
2 terminated in 99 steps
6 terminated in 101 steps
2 terminated in 103 steps
1 terminated in 105 steps
2 terminated in 107 steps
1 terminated in 109 steps
1 terminated in 110 steps
2 terminated in 113 steps
1 terminated in 117 steps
1 terminated in 120 steps
2 terminated in 123 steps
1 terminated in 125 steps
1 terminated in 128 steps
1 terminated in 130 steps
1 terminated in 131 steps
2 terminated in 138 steps
1 terminated in 146 steps
2 terminated in 152 steps
1 terminated in 154 steps
2 terminated in 166 steps
1 terminated in 169 steps
1 terminated in 189 steps
1 terminated in 199 steps
1 terminated in 204 steps
1 terminated in 206 steps
1 terminated in 207 steps
1 terminated in 234 steps
1 terminated in 241 steps
1 terminated in 295 steps
1 terminated in 303 steps
1 terminated in 379 steps
2 terminated in 386 steps
1 terminated in 554 steps
1 terminated in 924 steps
1 terminated in 71076 steps
```
The 71076 looked like a lucky shot into the search space.


Another run of 2 million candidates showing effectiveness (somewhat) of loop checkers:
```
Best:           261
Stuck left:     352363
Fell right:     564642
Too many steps: 180732
Out of fuel:    112777
Total:          2000000

376832 terminated in 3 steps
8192 terminated in 4 steps
130048 terminated in 5 steps
114176 terminated in 6 steps
75392 terminated in 7 steps
64320 terminated in 8 steps
26056 terminated in 9 steps
9284 terminated in 10 steps
10208 terminated in 11 steps
3260 terminated in 12 steps
2332 terminated in 13 steps
2676 terminated in 14 steps
918 terminated in 15 steps
674 terminated in 16 steps
709 terminated in 17 steps
486 terminated in 18 steps
455 terminated in 19 steps
331 terminated in 20 steps
356 terminated in 21 steps
168 terminated in 22 steps
158 terminated in 23 steps
153 terminated in 24 steps
142 terminated in 25 steps
202 terminated in 26 steps
98 terminated in 27 steps
89 terminated in 28 steps
51 terminated in 29 steps
42 terminated in 30 steps
67 terminated in 31 steps
35 terminated in 32 steps
36 terminated in 33 steps
23 terminated in 34 steps
43 terminated in 35 steps
22 terminated in 36 steps
21 terminated in 37 steps
25 terminated in 38 steps
24 terminated in 39 steps
18 terminated in 40 steps
12 terminated in 41 steps
21 terminated in 42 steps
55 terminated in 43 steps
8 terminated in 44 steps
14 terminated in 45 steps
4 terminated in 46 steps
27 terminated in 47 steps
12 terminated in 49 steps
6 terminated in 51 steps
7 terminated in 53 steps
19 terminated in 54 steps
6 terminated in 55 steps
1 terminated in 57 steps
2 terminated in 58 steps
1 terminated in 59 steps
14 terminated in 60 steps
1 terminated in 61 steps
2 terminated in 62 steps
1 terminated in 63 steps
2 terminated in 64 steps
4 terminated in 65 steps
1 terminated in 66 steps
1 terminated in 67 steps
1 terminated in 68 steps
3 terminated in 69 steps
1 terminated in 71 steps
1 terminated in 73 steps
1 terminated in 74 steps
2 terminated in 75 steps
2 terminated in 76 steps
1 terminated in 77 steps
1 terminated in 78 steps
1 terminated in 80 steps
4 terminated in 81 steps
1 terminated in 91 steps
1 terminated in 93 steps
1 terminated in 100 steps
1 terminated in 104 steps
1 terminated in 111 steps
1 terminated in 167 steps
1 terminated in 179 steps
1 terminated in 261 steps
```
