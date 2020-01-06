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
  -- |    |
  -- | □□ | z1
  -- |    |
  -- |□□□□| z0 * z2
  -- |    |
  -- |  □□| z2
  -- |    |
  -- | □□ | z1
  -- |    |
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
  let heaps = 1 / trivial

  -- For example, the number of heaps with 90 on the
  -- left and 9 in the middle is the binmoial
  -- coefficient 99C9 = 1731030945644
  print $ (M.coefficient [90,9,0] heaps :: Rational)

  -- We can also allow vertical pieces
  --  0123
  -- |    |
  -- |□   | x0
  -- |□   |
  -- |    |
  -- | □  | x1
  -- | □  |
  -- |    |
  -- |  □ | x2
  -- |  □  |
  -- |    |
  -- |   □| x3
  -- |   □ |
  -- +----+

  let x0 = M.var 3
  let x1 = M.var 4
  let x2 = M.var 5
  let x3 = M.var 6
  let trivial' = 1 - z0 - z1 - z2 + z0 * z2
                   - x0 - x1 - x2 - x3
                   + x0 * x1 + x0 * x2 + x0 * x3
                   + x1 * x2 + x1 * x3 + x2 * x3
                   - x0 * x1 * x2 - x0 * x1 * x3
                   - x0 * x2 * x3 - x1 * x2 * x3
                   + x0 * x1 * x2 * x3
                   + z0 * x2 + z0 * x3 - z0 * x2 * x3
                   + z1 * x0 + z1 * x3 - z1 * x0 * x3
                   + z2 * x0 + z2 * x1 - z2 * x0 * x1
  let heaps' = 1 / trivial'
  -- The number of ways to stack one of each horizontal
  -- piece and a vertical piece in column 1 is 12
  print $ (M.coefficient [1, 1, 1, 0, 1] $ heaps' :: Rational)

  print $ (M.coefficient [2, 2, 2, 2, 2, 2, 2] $ heaps' :: Rational)
