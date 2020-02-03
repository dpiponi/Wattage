module Homogeneous.Index where

import qualified Data.Array as A

type Z = Integer

fact :: Integer -> Integer
fact n = product [1..n]

pochhammer :: Integer -> Integer -> Integer
pochhammer x n = product [x, x+1 .. x+n-1]

-- Dimension of space of degree d polynomials in n variables
hdim' :: Int -> Int -> Int
hdim' n d = fromInteger (
  pochhammer (fromIntegral n) (fromIntegral d) `div` fact (fromIntegral d))

max_n_cache = 127 :: Int
max_d_cache = 1023 :: Int
hdimCache = A.array ((0, 0), (max_n_cache, max_d_cache))
                    [((i, j), hdim' i j) |
                     i <- [0..max_n_cache],
                     j <- [0..max_d_cache]]

-- | `hdim n d` is the dimension of the space of homogeneous polynomials
-- of degree `d` in `n` variables.
hdim n d = if n < max_n_cache && d < max_d_cache
  then hdimCache A.! (n, d)
  else hdim' n d

type Exponent = [Int]

-- | `addr' n d es` is the offset into the array of coefficients of the
-- coefficient of x^es in an n-variable  homogeneous polynomial of degree d.
addr' :: Int -> Int -> Exponent -> Int
addr' _ _ [] = 0
addr' _ _ [_] = 0
addr' n deg (e : es) =
  let r = deg - e
  in hdim r (n - 1) + addr' (n - 1) r es

-- | Return list of all exponents in degree `d` homogeneous polynomial
-- with `n` variables. Listed in order used in `Homogeneous` internal
-- structure.
allOfDegree :: Int -> Int -> [Exponent]
allOfDegree d 1 = [[d]]
allOfDegree d n = do
  i <- [d, d-1 .. 0]
  js <- allOfDegree (d - i) (n-1)
  return (i : js)

withAllSplits' :: Int -> Int -> Int -> Int -> Int -> Int ->
                  Exponent -> (Int -> Int -> a) -> [a]
withAllSplits' _ _ _ _ _ _ [] _ = error "Can only split a non-empty exponent list"
withAllSplits' addr0 addr1 n0 n1 d0 d1 [d] f =
    [f addr0 addr1 | d == d0 + d1]
withAllSplits' addr0 addr1 n0 n1 d0 d1 (i : is) f = do
  let lower = max 0 (i - d1)
  let upper = min i d0
  j0 <- [upper, upper - 1 .. lower]
  let j1 = i - j0
  let new_n0 = n0 - 1
  let new_n1 = n1 - 1
  let new_d0 = d0 - j0
  let new_d1 = d1 - j1
  let new_addr0 = addr0+hdim (d0-j0) new_n0
  let new_addr1 = addr1+hdim (d1-j1) new_n1
  withAllSplits' new_addr0 new_addr1 new_n0 new_n1 new_d0 new_d1 is f

exponentAdd :: Exponent -> Exponent -> Exponent
exponentAdd = zipWith (+)

exponentSub :: Exponent -> Exponent -> Exponent
exponentSub = zipWith (-)

-- | `allGreaterEqual xs ys` tests whether all exponents in
-- `xs` are greater than or equal to their corresponding exponents
-- in `ys`.
allGreaterEqual :: Exponent -> Exponent -> Bool
allGreaterEqual [] [] = True
allGreaterEqual [] _ = error "Mismatched exponents"
allGreaterEqual _ [] = error "Mismatched exponents"
allGreaterEqual (x : xs) (y : ys) = x >= y && allGreaterEqual xs ys

-- | `decr i es` decrements the `i`th exponent in `es` returning
-- `Nothing` if it's already at zero.
decr :: Int -> Exponent -> Maybe Exponent
decr _ [] = error "Can't decrement element of empty list"
decr 0 (i : is) = if i > 0 then Just ((i - 1) : is) else Nothing
decr n (i : is) = do
    js <- decr (n - 1) is
    return (i : js)
