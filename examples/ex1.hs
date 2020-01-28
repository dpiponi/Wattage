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
-- [1] https://people.mpim-bonn.mpg.de/zagier/files/tex/ClassicalHurwitz/HurwitzRevised.pdf
-- The numbers printed are given in table B.4 on page 57 of
-- On Asymptotics and Resurgent Structures of
-- Enumerative Gromov–Witten Invariants
-- [2] https://arxiv.org/pdf/1605.07473.pdf

pandharipande =  do
  putStrLn "First the direct application of the Pandharipande formula"
  let x = M.var 0 :: Multivariate Q
  let y = M.var 1
  let intY = M.integrate 1
  let h x y = intY (intY (exp (h x (y * exp x) - 2 * h x y + h x (y * exp (-x)))) / y)
  print $ F.truncate 12 $ unM $ h x y

x :: Formal Q
x = F.var
-- These are the C_d on page 4 of [1]
c :: Int -> Formal Q
c 1 = 1
c d0 = let d = fromIntegral d0
  in 1 / (d * d * (d - 1)) * sum [
      k * (d-k) * (d-k) * (exp (k * x) - 2 + exp (-k * x)) * c' k0 * c' (d0 - k0) |
      k0 <- [1 .. d0-1::Int],
      let k = fromIntegral k0]
c' = memoize c

dubrovin = do
  putStrLn "Now the faster Dubrovin formula."
  putStrLn "First some of H"
  let h = ogf $ [c' k | k <- [0..]]
  print $ M.truncate 20 $ h
  putStrLn "Now some of Z_H = exp H"
  print $ M.truncate 20 $ exp h

  -- Example from page 40 of [2]
  let g = 100 :: Int
  let d = 6 :: Int
  let n = 2*g+2*d-2
  -- 36773029021136586120...20640
  putStrLn "H_100,6 ="
  print $ fromIntegral (fact (fromIntegral n)) * M.coefficient [n, d] h

main = do
  pandharipande
  dubrovin
