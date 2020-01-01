module Main where

import Poly
import Wattage
import Homogeneous hiding (test)
import Data.Array
import Data.Ratio

-- lift x = x : repeat 0

-- h x y = [1, 2] ... integrate (ftail (integrate (
--   exp (h x (y * lift (exp x)) - 2 * h x y + h x (y * lift (exp (-x)))))))

-- make_zero n = H 1 n $ array (0, n-1) [(j, 0) | j <- [0 .. n-1]]

z0 :: MFormal Rational
z0 = F [Zero, make_var 0 2]
z1 = F [Zero, make_var 1 2]

zz = make_var

test :: [Homogeneous Rational] -> [Homogeneous Rational]
test (0 : xs) = xs
test xs = xs

u0 = make_var 0 2 :: Homogeneous Rational
u1 = make_var 1 2 :: Homogeneous Rational

var i j = F [Zero, make_var i j]

type MFormal a = Formal (Homogeneous a)

pderiv :: (Num a, Eq a, Show a) => Int -> MFormal a -> MFormal a
pderiv i xs = mapf (hderiv i) (ftail xs)

pint :: (Num a, Eq a, Show a, Fractional a) => Int -> MFormal a -> MFormal a
pint i xs = 0 `prepend` mapf (hint i) xs

ι = fromIntegral

-- Actual hurwitz numbers
-- https://arxiv.org/pdf/1605.07473.pdf
--
-- Exp doesn't need rational
-- It needs a module over the rationals
main :: IO ()
main = do
  print "Hello"

--   let x = z0
--   let y = z1
--   let intY = pint 1
--   let h x y = intY (intY (exp (h x (y * exp x) - 2 * h x y + h x (y * exp (-x)))) / y)
--   mapM_ print $ take 12 $ unF $ h x y

--   let q0 = var 0 3 :: MFormal Q
--   let q1 = var 1 3
--   let q2 = var 2 3
--   print $ take 80 $ unF $ 1/((1+q2*q1)*(1-q0-2*q1+q0*q2))

  let u = var 0 6 :: MFormal Q
  let p i = var i 6 :: MFormal Q
  let a nvars h = (1/2) * sum [
                (ι i + ι j) * p i * p j * pderiv (i + j) h + ι i * ι j * p (i + j) * pderiv i (pderiv j h) |
                n <- [1 .. nvars],
                i <- [1 .. n - 1],
                let j = n - i :: Int]
  let h0 = exp (p 1) :: MFormal Q
  let h1 = a 5 h0 :: MFormal Q
  let h2 = a 5 h1 :: MFormal Q
  let h3 = a 5 h2 :: MFormal Q
  let h4 = a 5 h3 :: MFormal Q
  mapM_ print $ take 10 $ unF $ (h4 / exp (p 1)) * fromIntegral (fact 4)

--   print $ hderiv 1 $ u0 * u0 * u1 * u1 * u0 * u1
--   print $ hint 1 $ u0 * u0 * u1 * u1
--   print $ take 8 $ pderiv 0 $ z0+z0*z0*z1*z1
--   print $ take 8 $ pint 1 $ z0 + z1 + z0*z1
--   let pexp x = let e = 1 + pint 0 (e * pderiv 0 x) in e
--   print $ take 8 $ pexp (z0*z1)
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
--   mapM_ print $ take 100 $ z0*exp (z0*z1)/(exp z0 - 1)
--   print $ take 8 $ z0/z0
--   print $ take 50 z0
--   print $ take 10 $ z0*z0
--   print $ take 4 $ exp (z0 + z1)
--   print $ take 4 $ exp (z0 + z1)
  return ()
