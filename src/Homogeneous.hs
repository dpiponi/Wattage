{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Homogeneous where

import qualified Data.Array as A
import qualified Data.Array.MArray as M
import Data.Array.ST
import Control.Monad
import qualified Data.List as L
import Control.Monad.ST
import Data.Ratio
import qualified Data.Map as Map
import Debug.Trace
import Wattage
import Control.DeepSeq

type Z = Integer
-- type Q = Ratio Integer

-- type Permutation = A.Array Z Z

-- fromList :: Z -> [(Z, Z)] -> Permutation
-- fromList n as = array' (0, n - 1) as
-- 
-- transpose :: Z -> Z -> Z -> Permutation
-- transpose n a b = fromList n [(i, if i == a then b else (if i == b then a else i)) | i <- [0..n-1]]
-- 
-- identity n = fromList n [(i, i) | i <- [0..n-1]]
-- 
-- hcompose :: Permutation -> Permutation -> Permutation
-- hcompose a b = let m = snd (A.bounds a)
--               in fromList (m + 1) [(i, a A.! (b A.! i)) | i <- [0 .. m]]
-- 
-- -- cycle_length :: Permutation -> Z -> Z
-- -- cycle_length a i = cycle_length' a i (aA.!i)
-- --   where cycle_length' _ i j | i == j = 1
-- --         cycle_length' a i j = 1 + cycle_length' a i (aA.!j)
-- 
-- test :: Permutation -> [Z]
-- test a = L.reverse $ L.sort $ runST $ do
--   let m = snd (A.bounds a)
--   flags :: STArray s Z Bool <- M.newArray (0, m) False
--   a' <- M.thaw a :: ST s (STArray s Z Z)
--   let cycles_from i | i > m = return []
--       cycles_from i = do
--         j <- readArray flags i
--         if j then cycles_from (i + 1)
--           else do
--             let cycle_length i k | i == k = return 1
--                 cycle_length i k = do
--                   writeArray flags k True
--                   l <- readArray a' k
--                   n <- cycle_length i l
--                   return (n + 1)
--             writeArray flags i True
--             k <- readArray a' i
--             r <- cycle_length i k
--             c <- cycles_from (i + 1)
--             return $ (r : c)
--   cycles_from 0
-- 
-- hurwitz :: Z -> [Z] -> Q
-- hurwitz m mu =
--   let n = sum mu
--       ps = (do
--               etas <- replicateM (fromIntegral m) $ do
--                     i <- [0 .. n-2]
--                     j <- [i+1 .. n-1]
--                     return $ transpose n i j
--               let p = foldr hcompose (identity n) etas
--               guard $ test p == mu
--               return p)
-- --   in ps
--   in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))
-- 
-- -- hurwitz :: Z -> [Z] -> Q
-- hurwitz' m mu =
--   let n = sum mu
--       ps = (do
--               etas <- replicateM (fromIntegral m) $ do
--                     i <- [0 .. n-2]
--                     j <- [i+1 .. n-1]
--                     return $ transpose n i j
--               let p = foldr hcompose (identity n) etas
--               guard $ test p == mu
--               return p)
--   in ps
-- --   in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

fact :: Integer -> Integer
fact n = product [1..n]

-- cycleCounts mu = map snd (Map.toList (Map.fromListWith (+) (zip mu (repeat 1))))
-- 
-- naut mu = product (map fact (cycleCounts mu))
-- 
-- hh m mu =
--   let smu = sum mu
--       n = m + 2 - smu
--   in fromIntegral (fact m) /
--     ((fromIntegral (naut mu)) * product [((fromIntegral (mu A.!A.!i))^(mu A.!A.!i))/(fromIntegral $ fact (mu A.!A.!i))*(fromIntegral $ mu A.!A.!i)^(n-3) | i <- [0..n-1]])
-- 
pochhammer :: Integer -> Integer -> Integer
pochhammer x n = product [x, x+1 .. x+n-1]

addr :: Int -> [Int] -> Int
addr _ [_] = 0
addr deg exponents =
  let m = fromIntegral (length exponents - 1)
      r = fromIntegral (deg - head exponents)
  in fromInteger $ pochhammer r m `div` fact m + fromIntegral (addr (fromInteger r) (tail exponents))

data Homogeneous a = Zero | H { degree :: Int, num_vars :: Int, coefficients :: A.Array Int a }
--   deriving Show

instance (Show a, Num a, Eq a) => Show (Homogeneous a) where
  showsPrec p Zero = showString "0"
  showsPrec p (H d n c) = showParen (p > 5) $ showString $ "<" ++ L.intercalate "+" [
    show c0 ++ "*" ++ concat ["x"++show i ++ "^" ++ show j | (i, j) <- zip [0..] js, j /= 0] |
      js <- allOfDegree d n,
      let c0 = c A.! addr d js,
      c0 /= 0] ++ ">"

isMonomial :: (Num a, Eq a) => Homogeneous a -> Maybe [Int]
isMonomial Zero = Nothing
isMonomial (H d n c) =
  let indices = allOfDegree d n
      index = filter (\i -> (c A.! addr d i) /= 0) indices
  in if length index == 1 then Just (head index) else Nothing

instance (Fractional a, Show a, Eq a) => Fractional (Homogeneous a) where
  fromRational i = H 0 1 $ listArray' (0, 0) [fromRational i]
  _ / Zero = error "Division by zero"
  Zero / _ = Zero
  h0@(H d0 n0 c0) / h1@(H d1 n1 c1) = 
        let n = max n0 n1
            h0' = if n0 < n then upgrade n h0 else h0
            h1' = if n1 < n then upgrade n h1 else h1
        in hdivide 0 h0' h1'

--   recip (H d n c) | d == 0 = H d n $ fmap recip c
--   recip h = error $ "No recip of " ++ show h

-- checkList :: [a] -> [a]
-- checkList [] = []
-- checkList (a : as) = a `seq` checkList as
-- 
-- check :: Array i a -> Array i a
-- check a = checkList (A.elems a) `seq` a

-- array' bounds elems =
--     if snd bounds - fst bounds /= length elems - 1
--       then error $ "\x1b[41m;ARRAY:" ++ show bounds ++ ": " ++ show elems
--       else let a = A.array bounds elems
--            in trace(show(bounds, elems)) $ trace (show a) $ a
-- 
-- listArray' bounds elems =
--     if snd bounds - fst bounds /= length elems - 1
--       then error $ "\x1b[41m;LISTARRAY:" ++ show bounds ++ ": " ++ show elems
--       else let a = A.listArray bounds elems
--            in trace(show(bounds, elems)) $ trace (show a) $ a

listArray' = A.listArray
array' = A.array

make_var :: (Show a, Num a) => Int -> Int -> Homogeneous a
make_var i n = H {degree=1, num_vars=n, coefficients=array' (0, n-1) [(j, if i == j then 1 else 0) | j <- [0 .. n-1]] }

makeMonomial :: (Show a, Eq a, Num a) => a -> [Int] -> Homogeneous a
makeMonomial a ks = 
    let d = sum ks
        n = length ks
        size = hdim n d
        i = addr d ks
        m = H d n $ array' (0, size - 1) [(j, if j == i then a else 0) | j <- [0 .. size - 1]]
    in m

allOfDegree :: Int -> Int -> [[Int]]
allOfDegree d 1 = [[d]]
allOfDegree d n = do
  i <- [0 .. d]
  js <- allOfDegree (d - i) (n-1)
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

x0 = make_var 0 1 :: Homogeneous Rational
x1 = make_var 1 2 :: Homogeneous Rational
x2 = make_var 2 3 :: Homogeneous Rational

upgrade :: (Show a, Num a) => Int -> Homogeneous a -> Homogeneous a
upgrade n Zero = Zero
upgrade n1 (H d n0 c0) =
  let s0 = hdim n0 d -- pochhammer n0 d `div` fact d
      s1 = hdim n1 d -- pochhammer n1 d `div` fact d
  in H d n1 $ array' (0, s1 - 1) [(i, if i < s0 then c0 A.! i else 0) | i <- [0 .. s1 - 1]]

-- Dimension of space of degree d polynomials in n variables
hdim :: Int -> Int -> Int
hdim n d = fromInteger (pochhammer (fromIntegral n) (fromIntegral d) `div` fact (fromIntegral d))

htimes :: (Show a, Num a) => Homogeneous a -> Homogeneous a -> Homogeneous a
htimes Zero _ = Zero
htimes _ Zero = Zero
htimes (H d0 n0 c0) (H d1 n1 c1) =
  let d = d0 + d1
  in H d n0 $ array' (0, hdim n0 d - 1) $
       [(addr d is, sum [(c0 A.! addr d0 js)*(c1 A.! addr d1 ks) |
                                 (js, ks) <- all_splits d0 d1 is]) |
        is <- allOfDegree d n0]

exponentAdd :: [Int] -> [Int] -> [Int]
exponentAdd a b = zipWith (+) a b

exponentSub :: [Int] -> [Int] -> [Int]
exponentSub a b = zipWith (-) a b

allGreaterEqual :: [Int] -> [Int] -> Bool
allGreaterEqual js ks = all id $ zipWith (>=) js ks

-- Optimise.
-- Need to look at relationship between addr when d varies.
-- Use to only init needed elements by walking through addresses,
-- not exponents.
monomialTimesHomogeneous :: (Show a, Num a) => [Int] -> Homogeneous a -> Homogeneous a
monomialTimesHomogeneous _ Zero = Zero
monomialTimesHomogeneous js (H d0 n c0) =
    let d1 = sum js + d0
        size1 = hdim n d1
    in H d1 n $ array' (0, size1 - 1) $
        [(addr d1 ks, a) |
         ks <- allOfDegree d1 n,
         let a = if allGreaterEqual ks js
                   then c0 A.! addr d0 (exponentSub ks js)
                   else 0]

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : as) = Just a

leadingTerm :: (Eq a, Num a, Show a) => Homogeneous a -> Maybe [Int]
leadingTerm Zero = Nothing
leadingTerm (H d n c) =
    maybeHead $ [ ks |
                      ks <- allOfDegree d n,
                      let i = addr d ks,
                      c A.! i /= 0
                    ]

hdivide :: (Eq a, Num a, Fractional a, Show a) => Homogeneous a -> Homogeneous a -> Homogeneous a -> Homogeneous a
hdivide acc Zero _ = acc
hdivide _ _ Zero = error "Dvision by zero"
hdivide acc h0@(H d0 n0 c0) h1@(H d1 n1 c1) =
    case leadingTerm h0 of
        Nothing -> acc
        Just lt0 -> case leadingTerm h1 of
                        Nothing -> error "Division by zero"
                        Just lt1 ->
                            if allGreaterEqual lt0 lt1
                              then let ratio = c0 A.! addr d0 lt0 / c1 A.! addr d1 lt1
                                       js = exponentSub lt0 lt1
                                       acc' = acc + makeMonomial ratio js
                                   in hdivide acc' (subtractMonomialTimes h0 ratio js h1) h1
                              else error ("Doesn't divide:" ++ show h0 ++ " / " ++ show h1)
                            



hscale :: Num a => a -> Homogeneous a -> Homogeneous a
hscale _ Zero = Zero
hscale a (H d0 n0 c0) = H d0 n0 $ fmap (a *) c0

-- Sort of a SAXPY
subtractMonomialTimes :: (Show a, Eq a, Num a) => Homogeneous a -> a -> [Int] -> Homogeneous a -> Homogeneous a
subtractMonomialTimes Zero _ _ Zero = Zero
subtractMonomialTimes h _ _ Zero = h
subtractMonomialTimes h0 a js h1 = h0 - hscale a (monomialTimesHomogeneous js h1)

isZero :: (Eq a, Num a, Show a) => Homogeneous a -> Bool
isZero Zero = True
isZero h@(H _ _ c) = all (== 0) $ A.elems c

instance (Eq a, Num a, Show a) => Eq (Homogeneous a) where
  h0 == h1 | isZero h0 && isZero h1 = True
  h0@(H d0 n0 c0) == h1@(H d1 n1 c1) | d0 == d1 =
    let n = max n0 n1
        H d0' n0' c0' = if n0 < n then upgrade n h0 else h0
        H d1' n1' c1' = if n1 < n then upgrade n h1 else h1
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
        H d1' n1' c1' = if n1 < n then upgrade n h1 else h1
    in H d0' n $ listArray' (A.bounds c0') $ zipWith (+) (A.elems c0') (A.elems c1')
  h0 * Zero = Zero
  Zero * h1 = Zero
  h0@(H d0 n0 c0) * h1@(H d1 n1 c1) =
    let n = max n0 n1
        h0' = if n0 < n then upgrade n h0 else h0
        h1' = if n1 < n then upgrade n h1 else h1
    in h0' `htimes` h1'
  fromInteger 0 = Zero
  fromInteger i = H 0 1 $ listArray' (0, 0) [fromInteger i]
  negate Zero = Zero
  negate (H d n c) = H d n $ fmap negate c
  signum _ = error "No signum for Homogeneous"
  abs _ = error "No abs for Homogeneous"

-- htimes :: Num a => Homogeneous a -> Homogeneous a -> Homogeneous a
-- htimes (H d0 n0 c0) (H d1 n1 c1) =
--   H (d0 + d1) n0 $ array' (0, (pochhammer n0 (d0 + d1) `div` fact (d0 + d1)) - 1) $
--     [(addr (d0 + d1) is, 1) | is <- allOfDegree (d0 + d1) n0]

{-
main = do
  print $ hurwitz 1 [2]
  print $ hurwitz 4 [3]
  print $ hurwitz 2 [2, 2] -- I expect 1/2 ???
  print $ hurwitz 0 [1, 1, 1]
  print $ hurwitz 1 [2, 1, 1, 1]

  print $ hh 1 [2, 1, 1, 1]
-}
