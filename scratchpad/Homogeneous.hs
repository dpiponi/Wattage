{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Homogeneous(Homogeneous(..),
                   Exponent,
                   coefficient,
                   make_var,
                   hint,
                   allCoefficients,
                   fact,
                   var,
                   integrate,
                   d,
                   homogeneousFromList,
                   fromAllCoefficients) where

import Homogeneous.Index
import qualified Data.Array as A
import qualified Data.Array.MArray as M
import qualified Data.Map as Map
import Data.Array.ST
import Control.Monad
import qualified Data.List as L
import Control.Monad.ST
import Data.Ratio
import Data.Maybe
import Debug.Trace
import qualified Formal as F

trimmed :: (Eq a, Num a) => [a] -> [a]
trimmed [] = []
trimmed (0 : xs) = let ys = trimmed xs
                   in if null ys then [] else 0 : ys
trimmed (x : xs) = x : trimmed xs

-- | `coefficient es h` returns coefficient of `x^es` in homogeneous polynomial
-- `h`.
coefficient :: Num a => Exponent -> Homogeneous a -> a
coefficient _ Zero = 0
coefficient is (H d n cs) =
    let is' = trimmed is
        nn = length is'
        dd = sum is'
    in if nn > n
          then 0
          else cs A.! addr' n d is'

data Homogeneous a = Zero
                 | H { degree :: Int, num_vars :: Int,
                       coefficients :: A.Array Int a }

superscripts, subscripts :: Map.Map Char Char
superscripts = Map.fromList $ zip "0123456789-" "⁰¹²³⁴⁵⁶⁷⁸⁹⁻"
subscripts = Map.fromList $ zip "0123456789-" "₀₁₂₃₄₅₆₇₈₉₋"

superscript :: Int -> String
superscript = map (\c -> Map.findWithDefault c c superscripts) . show

subscript :: Int -> String
subscript = map (\c -> Map.findWithDefault c c subscripts) . show

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

showVar :: Int -> Int -> String
showVar i 1 = "x" ++ show i
-- showVar i 1 = "x" ++ subscript i
-- showVar i k = "x" ++ subscript i ++ superscript k
-- showVar i k = "x" ++ subscript i ++ "^" ++ show k
showVar i k = "x" ++ show i ++ "^" ++ show k

showTerm :: (Show a, Num a, Eq a) => a -> Exponent -> String
showTerm a js | all (==0) js = show a
showTerm 1 js = L.intercalate " * " [showVar i j |
                                          (i, j) <- enumerate js, j /= 0]
showTerm (-1) js = "- " ++ L.intercalate " * " [showVar i j |
                                          (i, j) <- enumerate js, j /= 0]
showTerm c0 js | signum c0 == -1 = ("- "  ++) $ showsPrec 8 (- c0) $ " * " ++ L.intercalate " * " [showVar i j |
                                          (i, j) <- enumerate js, j /= 0]
showTerm c0 js = showsPrec 8 c0 $ " * " ++ L.intercalate " * " [showVar i j |
                                              (i, j) <- enumerate js, j /= 0]

showTerms :: (Show a, Num a, Eq a) => F.Position -> [(a, Exponent)] -> String
showTerms F.Initial [] = "0"
showTerms F.Initial ((c, js) : xs) | signum c == -1 = showTerm c js ++ showTerms F.NonInitial xs
showTerms F.Initial ((c, js) : xs) = showTerm c js ++ showTerms F.NonInitial xs
showTerms F.NonInitial [] = ""
showTerms F.NonInitial ((x, js) : xs) | signum x == -1 = " - " ++ showTerm (-x) js ++ showTerms F.NonInitial xs
showTerms F.NonInitial ((x, js) : xs) = " + " ++ showTerm x js ++ showTerms F.NonInitial xs

-- This is a mess and needs to be simplified. XXX
instance (Show a, Num a, Eq a) => Show (Homogeneous a) where
  showsPrec p Zero = showString "0"
--   showPrec p x h | countTerms h == 1 = 
  showsPrec p h =
    let terms = nonZeroTerms h
        precedence = if length terms == 1 then 8 else 7
    in showParen (p > precedence) $ showString $ showTerms F.Initial (nonZeroTerms h)

instance (Fractional a, Show a, Eq a) => Fractional (Homogeneous a) where
  fromRational i = H 0 1 $ listArray' (0, 0) [fromRational i]
  _ / Zero = error "Division by zero"
  Zero / _ = Zero
  h0@(H d0 n0 c0) / h1@(H d1 n1 c1) =
        let n = max n0 n1
            h0' = if n0 < n then upgrade n h0 else h0
            h1' = if n1 < n then upgrade n h1 else h1
        in hdivide h0' h1'

listArray' = A.listArray
array' = A.array

make_var :: (Show a, Num a) => Int -> Int -> Homogeneous a
make_var i n = H {degree=1, num_vars=n, coefficients=array' (0, n-1) [(j, if i == j then 1 else 0) | j <- [0 .. n-1]] }

var :: (Show a, Num a) => Int -> Homogeneous a
var i = make_var i (i + 1)

makeMonomial :: (Show a, Eq a, Num a) => a -> Exponent -> Homogeneous a
makeMonomial a ks =
    let d = sum ks
        n = length ks
        size = hdim n d
        i = addr' n d ks
        m = H d n $ array' (0, size - 1) [(j, if j == i then a else 0) |
                                          j <- [0 .. size - 1]]
    in m

-- x0 = make_var 0 1 :: Homogeneous Rational
-- x1 = make_var 1 2 :: Homogeneous Rational
-- x2 = make_var 2 3 :: Homogeneous Rational

-- XXX Could be optimised maybe
upgrade :: (Show a, Num a) => Int -> Homogeneous a -> Homogeneous a
upgrade n Zero = Zero
upgrade n (H _ n0 _) | n < n0 =
  error "Trying to lower number of variables in homogeneous polynomial"
upgrade n h@(H _ n0 _) | n == n0 = h
upgrade n1 (H d n0 c0) =
  let s0 = hdim n0 d
      s1 = hdim n1 d
  in homogeneousFromList n1 d [(c0 A.! addr' n0 d i, i) | i <- allOfDegree d n0]

-- | Construct homogeneous polynomial from a list of pairs
-- of coefficients and exponents.
-- Missing coefficients are set to zero.
homogeneousFromList :: Num a => Int -> Int -> [(a, Exponent)] -> Homogeneous a
homogeneousFromList n d as =
    H d n $ runST $ do
        arr <- newArray (0, hdim n d - 1) 0 :: Num a => ST s (STArray s Int a)
        forM_ as $ \(a, is) -> writeArray arr (addr' n d is) a
        freeze arr

-- Build homogeneous polynomial from function of exponent
makeHomogeneous :: Int -> Int -> (Exponent -> a) -> Homogeneous a
makeHomogeneous d n f =
    H d n $ A.listArray (0, hdim n d -1) $ map f $ allOfDegree d n

-- Build homogeneous polynomial from function of index and exponent.
makeIndexHomogeneous :: Int -> Int -> (Int -> Exponent -> a) -> Homogeneous a
makeIndexHomogeneous d n f =
    H d n $ array' (0, hdim n d -1) [(i, f i is) |
                                       (i, is) <- enumerate (allOfDegree d n)]

toListHomogeneous :: Homogeneous a -> [(a, Exponent)]
toListHomogeneous Zero = []
toListHomogeneous (H n d cs) = zip (A.elems cs) (allOfDegree n d)

nonZeroTerms :: (Num a, Eq a) => Homogeneous a -> [(a, Exponent)]
nonZeroTerms = filter (\(c, _) -> c /= 0) . toListHomogeneous

htimes :: (Show a, Num a) => Homogeneous a -> Homogeneous a -> Homogeneous a
htimes Zero _ = Zero
htimes _ Zero = Zero
htimes (H d0 n0 c0) (H d1 n1 c1) =
  makeHomogeneous (d0 + d1) n0 $ \is ->
    sum $ withAllSplits' 0 0 n0 n1 d0 d1 is $ \addrj addrk ->
        c0 A.! addrj * c1 A.! addrk

-- Optimise.
-- Need to look at relationship between addr when d varies.
-- Use to only init needed elements by walking through addresses,
-- not exponents.
-- monomialTimesHomogeneous :: (Show a, Num a) => Exponent -> Homogeneous a -> Homogeneous a
-- monomialTimesHomogeneous _ Zero = Zero
-- monomialTimesHomogeneous js (H d0 n c0) =
--     let d1 = sum js + d0
--         size1 = hdim n d1
--     in makeIndexHomogeneous d1 n $ \_ ks ->
--          if allGreaterEqual ks js
--              then c0 A.! addr' n d0 (exponentSub ks js)
--              else 0

subtractMonomialTimes' :: (Show a, Num a) => Homogeneous a -> a -> Exponent -> Homogeneous a -> Homogeneous a
subtractMonomialTimes' Zero _ _ Zero = Zero
subtractMonomialTimes' h _ _ Zero = h
-- This branch untested.
-- Probably never called. XXX
subtractMonomialTimes' Zero a js (H d0 n0 c0) =
    makeIndexHomogeneous d0 n0 $ \i ks ->
         if allGreaterEqual ks js
             then -a * c0 A.! addr' n0 d0 (exponentSub ks js)
             else 0
subtractMonomialTimes' (H d1 n1 c1) a js (H d0 n0 c0) =
    makeIndexHomogeneous d1 n1 $ \i ks ->
         if allGreaterEqual ks js
             then c1 A.! i - a * c0 A.! addr' n0 d0 (exponentSub ks js)
             else c1 A.! i

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : as) = Just a

leadingTerm :: (Eq a, Num a, Show a) => Homogeneous a -> Maybe (Int, Exponent)
leadingTerm Zero = Nothing
leadingTerm (H d n c) =
    maybeHead [ (i, ks) | (i, ks) <- enumerate (allOfDegree d n),
                       c A.! i /= 0 ]

onlyTerm :: (Eq a, Num a, Show a) => Homogeneous a -> Maybe (a, Exponent)
onlyTerm Zero = Nothing
onlyTerm (H d n c) =
    case [(a, ks) | (i, ks) <- enumerate (allOfDegree d n), let a = c A.! i, a /= 0 ] of
        [(a, ks)] -> Just (a, ks)
        otherwise -> Nothing

hdivide _ Zero = error "Division by zero"
hdivide Zero _ = Zero
hdivide h0@(H d0 n0 c0) h1@(H d1 n1 c1) = --trace (show (h0, h1)) $
    case onlyTerm h1 of
        Just (a, ks) -> simpleDivide h0 d1 a ks
        Nothing -> homogeneousFromList (max n0 n1) (d0 - d1) (hdivide' [] h0 h1)
simpleDivide Zero _ _ _ = error "Zero should have been handled by divide"
simpleDivide h0@(H d0 n0 c0) d1 a ks =
    makeHomogeneous (d0 - d1) n0 $ \js ->
        c0 A.! addr' n0 d0 (exponentAdd js ks) / a

-- XXX Need to not repeatedly restart when looking for leading term
hdivide' :: (Eq a, Num a, Fractional a, Show a) => [(a, Exponent)] -> Homogeneous a -> Homogeneous a -> [(a, Exponent)]
hdivide' acc Zero _ = acc
hdivide' _ _ Zero = error "Dvision by zero"
hdivide' acc h0 h1@(H d1 n1 c1) =
    case leadingTerm h1 of
        Nothing -> error "Division by zero"
        Just (i1, lt1) -> hdivide'' (c1 A.! i1) lt1 acc h0 h1
hdivide'' _ _ _ Zero _ = error "Should already have been handled by hdivide"
hdivide'' _ _ _ H{} Zero = error "Should already have been handled by hdivide"
hdivide'' a lt1 acc h0@(H d0 n0 c0) h1@(H d1 n1 c1) =
            case leadingTerm h0 of
                Nothing -> acc
                Just (i0, lt0) ->
                    if allGreaterEqual lt0 lt1
                      then let ratio = c0 A.! i0 / a
                               js = exponentSub lt0 lt1
                               acc' = (ratio, js) : acc
                           in hdivide'' a lt1 acc' (subtractMonomialTimes' h0 ratio js h1) h1
                      else error ("Doesn't divide:" ++ show h0 ++ " / " ++ show h1)

hscale :: (Eq a, Num a) => a -> Homogeneous a -> Homogeneous a
hscale _ Zero = Zero
-- hscale 0 _ = Zero
hscale a (H d0 n0 c0) = H d0 n0 $ fmap (a *) c0

-- Sort of a SAXPY
-- subtractMonomialTimes :: (Show a, Eq a, Num a) => Homogeneous a -> a -> Exponent -> Homogeneous a -> Homogeneous a
-- subtractMonomialTimes Zero _ _ Zero = Zero
-- subtractMonomialTimes h _ _ Zero = h
-- -- Maybe do this subtraction directly XXX
-- subtractMonomialTimes h0 a js h1 = h0 - hscale a (monomialTimesHomogeneous js h1)

isZero :: (Eq a, Num a, Show a) => Homogeneous a -> Bool
isZero Zero = True
isZero h@(H _ _ c) = all (== 0) $ A.elems c

instance (Eq a, Num a, Show a) => Eq (Homogeneous a) where
  h0 == h1 | isZero h0 && isZero h1 = True
  h0@(H d0 n0 c0) == h1@(H d1 n1 c1) | d0 == d1 =
    let n = max n0 n1
        H d0' n0' c0' = if n0 < n then upgrade n h0 else h0
        H d1' n1' c1' = if n1 < n then upgrade n h1 else h1
    in c0' == c1'
  _ == _ = False

instance (Eq a, Show a, Num a) => Num (Homogeneous a) where
  h0 + Zero = h0
  Zero + h1 = h1

  h0@(H d0 _ _) + h1@(H d1 _ _) | d0 /= d1 =
    error $ "Can't add mixed degrees: " ++ show (h0, h1)
  h0@(H _ n0 _) + h1@(H _ n1 _) =
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

  signum _ = 1 -- Not clear what this should be
  abs _ = error "No abs for Homogeneous"

-- Should gather, not scatter surely XXX
hderiv :: (Num a, Eq a, Show a) => Int -> Homogeneous a -> Homogeneous a
hderiv i Zero = Zero
hderiv i (H 0 n c) = Zero
hderiv i (H d n c) | i >= n = Zero
hderiv i (H d n c) =
    let size = hdim n (d - 1)
    in homogeneousFromList n (d - 1) [(a, js) |
                                           is <- allOfDegree d n,
                                           let p = is !! i,
                                           let mjs = decr i is,
                                           isJust mjs,
                                           let Just js = mjs,
                                           let a = fromIntegral p * (c A.! addr' n d is)]

hderiv' :: (Show a, Num a) => Int -> Homogeneous a -> Homogeneous a
hderiv' i Zero = Zero
hderiv' i (H 0 n c) = Zero
hderiv' i (H d n c) | i >= n = Zero
hderiv' i (H d n hs) =
  let delta = [if j == i then 1 else 0 | j <- [0 .. n - 1]]
      ptrs = allOfDegree'' (d - 1) n (adjust_up_by delta $ zero n)
      size = hdim n (d - 1)
  in H (d - 1) n $ A.listArray (0, size - 1) $
      [fromIntegral (1 + (es !! i)) * (hs A.! j) | (j, es) <- ptrs]

-- | `d i h` is the derivative of the homogeneous polynomial
-- `h` with respect to the `i`th variable.
d :: (Num a, Eq a, Show a) => Int -> Homogeneous a -> Homogeneous a
d = hderiv'

-- scaleDeriv :: (Num a, Eq a, Show a) => Int -> Homogeneous a -> Homogeneous a
-- scaleDeriv i Zero = Zero
-- scaleDeriv i (H 0 n c) = Zero
-- scaleDeriv i (H d n c) | i >= n = Zero
-- scaleDeriv i (H d n c) =
--     makeIndexHomogeneous n (d - 1) $ \i is -> let b = c A.! addr' n d is
--                                               in b * fromIntegral (is !! i)

scaleInt :: (Num a, Eq a, Show a, Fractional a) => Int -> Homogeneous a -> Homogeneous a
scaleInt i Zero = Zero
scaleInt i (H 0 n c) = Zero
scaleInt i (H d n c) | i >= n = Zero
scaleInt i (H d n c) =
    makeIndexHomogeneous n (d - 1) $ \i is -> let b = c A.! addr' n d is
                                              in b / fromIntegral (is !! i)

-- probably bad on implicit vars XXX
hint :: (Num a, Eq a, Show a, Fractional a) => Int -> Homogeneous a -> Homogeneous a
hint i Zero = Zero
hint i h@(H d n c) | i >= n = hint i (upgrade (i+1) h)
hint i h@(H d n c) = --trace (show (i, d, n, c)) $
     homogeneousFromList n (d + 1) [(a, is) |
                                           is <- allOfDegree (d + 1) n,
                                           let mjs = decr i is,
                                           isJust mjs,
                                           let Just js = mjs,
                                           let a = (c A.! addr' n d js) / fromIntegral (is !! i)]

integrate :: (Num a, Eq a, Show a, Fractional a) => Int -> Homogeneous a -> Homogeneous a
integrate = hint

-- | Construct a homogeneous polynomial from  a list of its coefficients
-- in standard order.
fromAllCoefficients n d bs = 
  let s = hdim' n d
  in if s /= length (bs)
    then error ("Wrong # of coefficients d = " ++
                show d ++ " n = " ++ show n ++
                " s = " ++ show s ++
                " len(bs) =" ++ show (length bs))
    else H d n (A.listArray (0, s - 1) bs)

-- | Obtain a list of all coefficients from a homogeneous polynomial
-- in standard order. This includes zero polynomials which are returned
-- as a isuitably sized list of 0's.
allCoefficients :: (Show a, Num a) => Int -> Int -> Homogeneous a -> [a]
allCoefficients n d h@(H d' n' cs) | n' < n = allCoefficients n d (upgrade n h)
allCoefficients n d (H d' n' cs) = A.elems cs
allCoefficients n d Zero = take (hdim' n d) (repeat 0)
