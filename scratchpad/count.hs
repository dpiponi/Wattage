{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array
import qualified Data.Array.MArray as M
import Data.Array.ST
import Control.Monad
import qualified Data.List as L
import Control.Monad.ST
import Data.Ratio
import qualified Data.Map as Map

type Z = Integer
type Q = Ratio Integer

type Permutation = Array Z Z

fromList :: Z -> [(Z, Z)] -> Permutation
fromList n as = array (0, n - 1) as

transpose :: Z -> Z -> Z -> Permutation
transpose n a b = fromList n [(i, if i == a then b else (if i == b then a else i)) | i <- [0..n-1]]

identity n = fromList n [(i, i) | i <- [0..n-1]]

compose :: Permutation -> Permutation -> Permutation
compose a b = let m = snd (bounds a)
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
              let p = foldr compose (identity n) etas
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
              let p = foldr compose (identity n) etas
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
addr n is =
  let m = length is - 1
      r = n - head is
  in pochhammer r m `div` fact m + addr r (tail is)

data Homogeneous a = H { degree :: Int, num_vars :: Int, coefficients :: Array Int a }
--   deriving Show

instance (Show a, Num a, Eq a) => Show (Homogeneous a) where
  showsPrec p (H d n c) = showParen (p > 5) $ showString $ L.intercalate "+" [
    show c0 ++ "*" ++ concat ["x"++show i ++ "^" ++ show j | (i, j) <- zip [0..] js, j /= 0] |
      js <- all_of_degree d n,
      let c0 = c ! addr d js,
      c0 /= 0]

make_var :: Num a => Int -> Int -> Homogeneous a
make_var i n = H 1 n $ array (0, n-1) [(j, if i == j then 1 else 0) | j <- [0 .. n-1]]

all_of_degree :: Int -> Int -> [[Int]]
all_of_degree d 1 = [[d]]
all_of_degree d n = do
  i <- [0 .. d]
  js <- all_of_degree (d - i) (n-1)
  return (i : js)

all_splits :: Int -> Int -> [Int] -> [([Int], [Int])]
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
upgrade n1 (H d n0 c0) =
  let s0 = pochhammer n0 d `div` fact d
      s1 = pochhammer n1 d `div` fact d
  in H d n1 $ array (0, s1 - 1) [(i, if i < s0 then c0 ! i else 0) | i <- [0 .. s1 - 1]]

htimes :: Num a => Homogeneous a -> Homogeneous a -> Homogeneous a
htimes (H d0 n0 c0) (H d1 n1 c1) =
  H (d0 + d1) n0 $ array (0, (pochhammer n0 (d0 + d1) `div` fact (d0 + d1)) - 1) $
    [(addr (d0 + d1) is, sum [(c0 ! addr d0 js)*(c1 ! addr d1 ks) | (js, ks) <- all_splits d0 d1 is]) | is <- all_of_degree (d0 + d1) n0]

instance Num a => Num (Homogeneous a) where
  H d0 n0 c0 + H d1 n1 c1 | d0 /= d1 = error "Can't add mixed degrees"
  h0@(H d0 n0 c0) + h1@(H d1 n1 c1) =
    let n = max n0 n1
        H d0 n0 c0 = if n0 < n then upgrade n h0 else h0
        H d1 n1 c1 = if n0 < n then upgrade n h1 else h1
    in H d0 n $ listArray (bounds c0) $ zipWith (+) (elems c0) (elems c1)
  h0@(H d0 n0 c0) * h1@(H d1 n1 c1) =
    let n = max n0 n1
        h0' = if n0 < n then upgrade n h0 else h0
        h1' = if n1 < n then upgrade n h1 else h1
    in h0' `htimes` h1'
  fromInteger i = H 0 1 $ listArray (0, 1) [fromInteger i]
  negate (H d n c) = H d n $ fmap negate c

-- htimes :: Num a => Homogeneous a -> Homogeneous a -> Homogeneous a
-- htimes (H d0 n0 c0) (H d1 n1 c1) =
--   H (d0 + d1) n0 $ array (0, (pochhammer n0 (d0 + d1) `div` fact (d0 + d1)) - 1) $
--     [(addr (d0 + d1) is, 1) | is <- all_of_degree (d0 + d1) n0]

main = do
--   print $ hurwitz 1 [2]
--   print $ hurwitz 4 [3]
--   print $ hurwitz 2 [2, 2] -- I expect 1/2 ???
--   print $ hurwitz 0 [1, 1, 1]
--   print $ hurwitz 1 [2, 1, 1, 1]
-- 
--   print $ hh 1 [2, 1, 1, 1]

--   print $ hh 2 []

  forM_ [1..8] $ \gd -> do
    forM_ [1..gd] $ \d -> do
      let g = gd - d
      let n = 2*g+2*d-2
      let d0 = fromInteger d
      putStrLn $ "g=" ++ show g ++ " d=" ++ show d ++ " n=" ++ show n ++ ": " ++ show (hurwitz n (take d0 (cycle [1])) / fromIntegral (fact n))
