-- Heaps of pieces

import Wattage as W
import Multivariate as M

main = do
  let z0 = M.var 0
  let z1 = M.var 1
  let z2 = M.var 2
  -- Consider a tetris type game with just the piece □□
  -- and only 4 columns. No rotation of pieces allowed.
  -- Here's a possible configuration
  --  0123
  -- |    |
  -- |  □□| z2
  -- | □□ | z1
  -- |□□□□| z0 * z2
  -- |  □□| z2
  -- | □□ | z1
  -- |□□  | z0
  -- +----+
  -- We use z0, z1, z2 to represent the piece
  -- in columns 0-1, 1-2 or 2-3
  --
  -- Note how a z1 dropped on a z0 sits on top.
  -- But a z2 dropped on a z0 sits next to it.
  -- In this sense z0 interacts with z1
  --               z1 interacts with z2
  -- But z0 and z2 don't interact.
  --
  -- Let trivial equal the sum of all heaps where
  -- no interactions happen, weighted by (-1)^# pieces.
  -- So we have:
  let trivial = 1 - z0 - z1 - z2 + z0 * z2
  -- Then the sum of all heaps is given by:
  let heaps = 1 / (1 - z0 - z1 - z2 + z0 * z2)

  -- For example, the number of heaps with 99 on the
  -- left and 9 in the middle is the binmoial
  -- coefficient 99C9
  print $ (M.coefficient [90,9,0] heaps :: Rational)
