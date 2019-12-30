module Main where

import Poly
import Wattage
import Homogeneous hiding (test)
import Data.Array
import Data.Ratio

lift x = x : repeat 0

h x y = [1, 2] ... integrate (tail (integrate (
  exp (h x (y * lift (exp x)) - 2 * h x y + h x (y * lift (exp (-x)))))))

-- make_zero n = H 1 n $ array (0, n-1) [(j, 0) | j <- [0 .. n-1]]

z0 :: [Homogeneous Rational]
z0 = Zero : make_var 0 2 : repeat Zero
z1 = Zero : make_var 1 2 : repeat Zero
-- z2 = Zero : make_var 2 3 : repeat Zero

zz = make_var

test :: [Homogeneous Rational] -> [Homogeneous Rational]
test (0 : xs) = xs
test xs = xs

-- Exp doesn't need rational
-- It needs a module over the rationals
main :: IO ()
main = do
--   print $ take 8 $ z/(1-z)
--   print "Hello"
--   let u0 = upgrade 3 x0
--   let u1 = upgrade 3 x1
--   let u2 = x2
--   print $ take 5 $ 1/(1- z0-z1)
--   print $ take 5 $ exp z0
--   let v = (u0+u1+u2)*(u0+2*u1+3*u2)
--   print $ makeMonomial 2 [1, 1, 1]
--   print $ v
--   print $ v / (u0+u1+u2)
--   print $ leadingTerm (x0 * x1 + x2 * x2)
--   let u = 2 * zz 0 3 * zz 1 3 * zz 2 3 + zz 2 3 * zz 2 3 * zz 2 3
--   let v = zz 0 3 * zz 1 3
--   print $ subtractMonomialTimes u 2 [0, 0, 1] v
--   print $ x0/x0
--   print $ take 8 $ test $ z0*exp (z0*z1)
--   print $ take 8 $ test $ exp z0 - 1
--   print $ take 5 $ test z0
--   print $ take 5 z0
--   print $ take 5 $ z0 / z0
  mapM_ print $ take 100 $ z0*exp (z0*z1)/(exp z0 - 1)
--   print $ take 8 $ z0/z0
--   print $ take 50 z0
--   print $ take 10 $ z0*z0
--   print $ take 4 $ exp (z0 + z1)
--   print $ take 4 $ exp (z0 + z1)
  return ()
