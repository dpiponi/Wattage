{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Homogeneous where

import Data.Array
import qualified Data.Array.MArray as M
import Data.Array.ST
import Control.Monad
import qualified Data.List as L
import Control.Monad.ST
import Data.Ratio
import qualified Data.Map as Map
import Debug.Trace
import Wattage

type Z = Integer
-- type Q = Ratio Integer

type Permutation = Array Z Z

fromList :: Z -> [(Z, Z)] -> Permutation
fromList n as = array (0, n - 1) as

transpose :: Z -> Z -> Z -> Permutation
transpose n a b = fromList n [(i, if i == a then b else (if i == b then a else i)) | i <- [0..n-1]]

identity n = fromList n [(i, i) | i <- [0..n-1]]

hcompose :: Permutation -> Permutation -> Permutation
hcompose a b = let m = snd (bounds a)
              in fromList (m + 1) [(i, a ! (b ! i)) | i <- [0 .. m]]

-- cycle_length :: Permutation -> Z -> Z
-- cycle_length a i = cycle_length' a i (a!i)
--   where cycle_length' _ i j | i == j = 1
--         cycle_length' a i j = 1 + cycle_length' a i (a!j)

test :: Permutation -> [Z]
test a = L.reverse $ L.sort $ runST $ do
  let m = snd (bounds a)
  flags :: STArray s Z Bool <- M.newArray (0, m) False
  a' <- M.thaw a :: ST s (STArray s Z Z)
  let cycles_from i | i > m = return []
      cycles_from i = do
        j <- readArray flags i
        if j then cycles_from (i + 1)
          else do
            let cycle_length i k | i == k = return 1
                cycle_length i k = do
                  writeArray flags k True
                  l <- readArray a' k
                  n <- cycle_length i l
                  return (n + 1)
            writeArray flags i True
            k <- readArray a' i
            r <- cycle_length i k
            c <- cycles_from (i + 1)
            return $ (r : c)
  cycles_from 0

hurwitz :: Z -> [Z] -> Q
hurwitz m mu =
  let n = sum mu
      ps = (do
              etas <- replicateM (fromIntegral m) $ do
                    i <- [0 .. n-2]
                    j <- [i+1 .. n-1]
                    return $ transpose n i j
              let p = foldr hcompose (identity n) etas
              guard $ test p == mu
              return p)
--   in ps
  in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

-- hurwitz :: Z -> [Z] -> Q
hurwitz' m mu =
  let n = sum mu
      ps = (do
              etas <- replicateM (fromIntegral m) $ do
                    i <- [0 .. n-2]
                    j <- [i+1 .. n-1]
                    return $ transpose n i j
              let p = foldr hcompose (identity n) etas
              guard $ test p == mu
              return p)
  in ps
--   in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

fact n = product [1..n]

cycleCounts mu = map snd (Map.toList (Map.fromListWith (+) (zip mu (repeat 1))))

naut mu = product (map fact (cycleCounts mu))

hh m mu =
  let smu = sum mu
      n = m + 2 - smu
  in fromIntegral (fact m) /
    ((fromIntegral (naut mu)) * product [((fromIntegral (mu!!i))^(mu!!i))/(fromIntegral $ fact (mu!!i))*(fromIntegral $ mu!!i)^(n-3) | i <- [0..n-1]])

pochhammer :: Int -> Int -> Int
pochhammer x n = product [x, x+1 .. x+n-1]

addr :: Int -> [Int] -> Int
addr _ [_] = 0
addr deg exponents =
  let m = length exponents - 1
      r = deg - head exponents
  in pochhammer r m `div` fact m + addr r (tail exponents)

data Homogeneous a = Zero | H { degree :: Int, num_vars :: Int, coefficients :: Array Int a }
--   deriving Show

instance (Show a, Num a, Eq a) => Show (Homogeneous a) where
  showsPrec p Zero = showString "0"
  showsPrec p (H d n c) = showParen (p > 5) $ showString $ L.intercalate "+" [
    show c0 ++ "*" ++ concat ["x"++show i ++ "^" ++ show j | (i, j) <- zip [0..] js, j /= 0] |
      js <- all_of_degree d n,
      let c0 = c ! addr d js,
      c0 /= 0]

isMonomial :: (Num a, Eq a) => Homogeneous a -> Maybe [Int]
isMonomial (H d n c) =
  let indices = all_of_degree d n
      index = filter (\i -> (c ! addr d i) /= 0) indices
  in if length index == 1 then Just (head index) else Nothing

-- instance (Fractional a, Show a, Eq a) => Fractional (Homogeneous a) where
--   fromRational i = H 0 1 $ listArray (0, 0) [fromRational i]
--   h0@(H d0 n0 c0) / h1@(H d1 n1 c1) = 
--     case isMonomial h1 of
--       Nothing -> error "Can only divide by monomial"
--       Just _ ->
--         let n = max n0 n1
--             H d0' n0' c0' = if n0 < n then upgrade n h0 else h0
--             H d1' n1' c1' = if n0 < n then upgrade n h1 else h1
--             Just index = isMonomial h1
--             r = c1' ! addr d1 index
--             size = pochhammer n (d0' - d1) `div` fact (d0' - d1')
--             size0 = pochhammer n d0' `div` fact d0'
--             size1 = pochhammer n d1' `div` fact d1'
--             in if length [indices | indices <- all_of_degree d1' n,
--                                     let c = c0' ! addr d0' indices,
--                                     c /= 0,
--                                     any (<) $ zipWith (-) indices index] > 0
--                   then error "Can't divide by larger exponents"
--                   else H (d0 - d1) n $
--                     array (0, size - 1) [(addr (d0' - d1'), c0' ! addr d0' indices') |
--                                                   indices <- all_of_degree d0' n,
--                                                   indices' <- zipWith (-) indices index]

--   recip (H d n c) | d == 0 = H d n $ fmap recip c
--   recip h = error $ "No recip of " ++ show h

make_var :: Num a => Int -> Int -> Homogeneous a
make_var i n = H {degree=1, num_vars=n, coefficients=array (0, n-1) [(j, if i == j then 1 else 0) | j <- [0 .. n-1]] }

all_of_degree :: Int -> Int -> [[Int]]
all_of_degree d 1 = [[d]]
all_of_degree d n = do
  i <- [0 .. d]
  js <- all_of_degree (d - i) (n-1)
  return (i : js)

all_splits :: Int -> Int -> [Int] -> [([Int], [Int])]
all_splits _ _ [] = error "Can only split a non-empty exponent list"
all_splits d0 d1 [n] =
  if n == d0 + d1
    then [([d0], [d1])]
    else []
all_splits d0 d1 (i : is) = do
  j0 <- [0 .. min i d0]
  let j1 = i - j0
  guard $ j1 >= 0 && j1 <= d1
  (ks, ls) <- all_splits (d0 - j0) (d1 - j1) is
  return (j0 : ks, j1 : ls)

x0 = make_var 0 1
x1 = make_var 1 2
x2 = make_var 2 3

upgrade :: Num a => Int -> Homogeneous a -> Homogeneous a
upgrade n Zero = Zero
upgrade n1 (H d n0 c0) =
  let s0 = pochhammer n0 d `div` fact d
      s1 = pochhammer n1 d `div` fact d
  in H d n1 $ array (0, s1 - 1) [(i, if i < s0 then c0 ! i else 0) | i <- [0 .. s1 - 1]]

-- Dimension of space of degree d polynomials in n variables
hdim n d = pochhammer n d `div` fact d

htimes :: Num a => Homogeneous a -> Homogeneous a -> Homogeneous a
htimes Zero _ = Zero
htimes _ Zero = Zero
htimes (H d0 n0 c0) (H d1 n1 c1) =
  let d = d0 + d1
  in H d n0 $ array (0, hdim n0 d - 1) $
       [(addr d is, sum [(c0 ! addr d0 js)*(c1 ! addr d1 ks) |
                                 (js, ks) <- all_splits d0 d1 is]) |
        is <- all_of_degree d n0]

isZero :: (Eq a, Num a, Show a) => Homogeneous a -> Bool
isZero Zero = True
isZero h@(H _ _ c) = all (== 0) $ elems c

instance (Eq a, Num a, Show a) => Eq (Homogeneous a) where
  h0 == h1 | isZero h0 && isZero h1 = True
  h0@(H d0 n0 c0) == h1@(H d1 n1 c1) | d0 == d1 =
    let n = max n0 n1
        H d0' n0' c0' = if n0 < n then upgrade n h0 else h0
        H d1' n1' c1' = if n0 < n then upgrade n h1 else h1
    in trace (show (c0', c1')) $ c0' == c1'
  _ == _ = False

instance (Eq a, Show a, Num a) => Num (Homogeneous a) where
--   h0 + h1 | isZero h0 = h1
--   h0 + h1 | isZero h1 = h0
  h0 + Zero = h0
  Zero + h1 = h1
  h0@(H d0 n0 c0) + h1@(H d1 n1 c1) | d0 /= d1 = error $ "Can't add mixed degrees: " ++ show (h0, h1)
  h0@(H d0 n0 c0) + h1@(H d1 n1 c1) =
    let n = max n0 n1
        H d0' n0' c0' = if n0 < n then upgrade n h0 else h0
        H d1' n1' c1' = if n0 < n then upgrade n h1 else h1
    in H d0' n $ listArray (bounds c0') $ zipWith (+) (elems c0') (elems c1')
  h0 * Zero = Zero
  Zero * h1 = Zero
  h0@(H d0 n0 c0) * h1@(H d1 n1 c1) =
    let n = max n0 n1
        h0' = if n0 < n then upgrade n h0 else h0
        h1' = if n1 < n then upgrade n h1 else h1
    in h0' `htimes` h1'
  fromInteger 0 = Zero
  fromInteger i = H 0 1 $ listArray (0, 0) [fromInteger i]
  negate Zero = Zero
  negate (H d n c) = H d n $ fmap negate c
  signum _ = error "No signum for Homogeneous"
  abs _ = error "No abs for Homogeneous"

-- htimes :: Num a => Homogeneous a -> Homogeneous a -> Homogeneous a
-- htimes (H d0 n0 c0) (H d1 n1 c1) =
--   H (d0 + d1) n0 $ array (0, (pochhammer n0 (d0 + d1) `div` fact (d0 + d1)) - 1) $
--     [(addr (d0 + d1) is, 1) | is <- all_of_degree (d0 + d1) n0]

{-
main = do
  print $ hurwitz 1 [2]
  print $ hurwitz 4 [3]
  print $ hurwitz 2 [2, 2] -- I expect 1/2 ???
  print $ hurwitz 0 [1, 1, 1]
  print $ hurwitz 1 [2, 1, 1, 1]

  print $ hh 1 [2, 1, 1, 1]
-}
