import Formal as F
import Homogeneous as H
import Multivariate as M

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

main = do
  lambertW
  gromov_witten
