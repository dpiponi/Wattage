import Formal as F
import Homogeneous as H
import Multivariate as M
import Data.Function.Memoize
import Data.Array as A
import Debug.Trace

-- Some definitions that make use of self-reference
-- https://en.wikipedia.org/wiki/Lambert_W_function

lambertW = do
  let x = F.var :: Formal Q
  let u = [0, 1] ... F.integrate (1 / (x + exp u))
  print $ F.truncate 15 u
  print $ F.truncate 15 $ inverse $ (x * exp x)

-- Evaluating series given by eq (10), p.3 of
-- CLASSICAL HURWITZ NUMBERS AND RELATED COMBINATORICS
-- https://people.mpim-bonn.mpg.de/zagier/files/tex/ClassicalHurwitz/HurwitzRevised.pdf
-- The numbers printed are given in table B.4 on page 57 of
-- On Asymptotics and Resurgent Structures of
-- Enumerative Gromov–Witten Invariants
-- https://arxiv.org/pdf/1605.07473.pdf

gromov_witten =  do
  let x = M.var 0 :: Multivariate Q
  let y = M.var 1
  let intY = M.integrate 1
  let h x y = intY (intY (exp (h x (y * exp x) - 2 * h x y + h x (y * exp (-x)))) / y)
  print $ F.truncate 12 $ unM $ h x y
--   print ""
--   print $ F.truncate 14 $ unM $ exp (h x y)

x :: Formal Q
x = F.var

c :: Int -> Formal Q
c 1 = 1
c d0 = let d = fromIntegral d0
  in 1 / (d * d * (d - 1)) * sum [
      k * (d-k) * (d-k) * (exp (k * x) - 2 + exp (-k * x)) * c' k0 * c' (d0 - k0) |
      k0 <- [1 .. d0-1::Int],
      let k = fromIntegral k0]
c' = memoize c

tt x = trace (show x) x

main = do
--   print $ F.truncate 5 $ c' 2
--   print $ F.truncate 5 $ unM $ combine $ [exp (k * x) :: Formal Q | k0 <- [0..], let k = fromIntegral k0]
--   print $ combine $ [c' k :: Formal Q | k0 <- [0..], let k = fromIntegral k0]
  let h = combine $ [c' k | k <- [0..]]
  print $ F.truncate 20 $ unM $ exp h
--   print $ F.truncate 20 $ c 4
--   print $ F.truncate 5 $ unM $ combine [c d | d <- [0..]]
--   lambertW
--   gromov_witten
