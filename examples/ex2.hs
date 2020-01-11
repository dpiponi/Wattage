-- Heaps of pieces

import Formal as F
import Multivariate as M

main = do
  let x0 = M.var 0
  let x1 = M.var 1
  let y0 = M.var 2
  let y1 = M.var 3
  let y2 = M.var 4

  -- Consider a tetris type game with just the piece □□
  -- and only 3 columns.
  --  012
  -- |  □| 
  -- |  □| y2
  -- |   |
  -- | □ | 
  -- | □ | y1
  -- |   |
  -- |□  | 
  -- |□  | y0
  -- |   |
  -- | □□| x1
  -- |   |
  -- |□□ | x0
  -- +---+
  -- We use x0, x1, y0, y1, y2 to represent the
  -- 5 positions and orientations.
  --
  -- Note how a x1 dropped on a x0 sits on top.
  -- But a y0 dropped next to an x1 sits next to it.
  -- In this sense x0 interacts with x1
  --               y0 doesn't interact with x1
  --
  -- Let `trivial` equal the sum of all heaps where
  -- no interactions happen, weighted by (-1)^# pieces.
  -- So we have:
  let trivial = 1 - x0 - x1 - y0 - y1 - y2
                  + x0*y2 + y0*x1 + y0*y1 + y0*y2 + y1*y2
                  - y0*y1*y2

  -- Then the sum of all heaps is given by:
  let heaps = 1 / trivial

  -- For example, the number of heaps with 
  -- an x0, an x1 and 32 y0's should
  -- be 66 as there are 33 places the x0 can be inserted
  -- into the y0's and the x1 either sits at the bottom
  -- or rests on top of the x0.
  print (M.coefficient [1, 1, 32, 0, 0] heaps :: Rational)

  -- 6 x0's, 9 x1's, 7 y0's, 8 y1's and 8 y2's
  print (M.coefficient [6, 9, 7, 8, 8] heaps :: Rational)

  -- Note that getting the [1, 1, 100, 0, 0] coefficient, say, can
  -- take a long time. We're asking for coefficients from raising
  -- `trivial` to around the power of 100. The code tries to 
  -- compute lazily but still allocates the data structures. A
  -- future version may try to do things more sparsely.
