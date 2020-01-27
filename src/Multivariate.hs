{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multivariate where

import Formal as F
import Homogeneous as H
import Data.Array as A

newtype Multivariate a = M { unM :: Formal (Homogeneous a) }
  deriving (Num, Fractional, Floating)

var :: (Show a, Num a) => Int -> Multivariate a
var i = M (F [Zero, make_var i (i + 1)])

coefficient :: (Eq a, Num a, Show a) => Exponent -> Multivariate a -> a
coefficient is (M f) = H.coefficient is (F.coefficient (sum is) f)

integrate :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
integrate i (M xs) = M $ 0 `prepend` fmap (hint i) xs

d :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
d i (M xs) = M $ 0 `prepend` fmap (hint i) xs

-- mprepend :: Homogeneous a -> Multivariate a -> Multivariate a
-- mprepend x (M ys) = M (x `prepend` ys)

-- instance (Show r, Eq r, Num r) => Num (Multivariate r) where
--     M x + M y  = M $ x + y
--     M x - M y  = M $ x - y
--     M x * M y = M $ x * y
--     fromInteger x      = M (fromInteger x)
--     negate (M x)     = M $ negate x
--     signum _ = error "signum only applicable to non-empty lists"
--     abs _   = error "Can't form abs of a power series"
-- 
-- instance (Show r, Eq r, Num r, Fractional r) => Fractional (Multivariate r) where
--     M x / M y  = M $ x / y
--     fromRational x      = M (fromRational x)

instance (Show a, Num a, Eq a) => Show (Multivariate a) where
--   show (M x) = show x
    showsPrec _ (M (F [])) = ("0" ++)
    showsPrec p (M (F x)) = showParen (p >= 6) $ showTerms Initial 0 x where
        showTerms _ _ [] = error "Shouldn't be showing empty list of terms"
        showTerms Initial n ([0]) = ("0" ++)
        showTerms Initial n ([x]) = showTerm n x
        showTerms NonInitial n ([0]) = id
        showTerms NonInitial n ([x]) | signum x == -1 = (" - " ++) . showTerm n (-x)
        showTerms NonInitial n ([x]) = (" + " ++) . showTerm n x
        showTerms position n (0 : xs) = showTerms position (n + 1) xs
        showTerms Initial n (x : xs) = showTerm n x . showTerms NonInitial (n + 1) xs
        showTerms NonInitial n (x : xs) | signum x == -1 =
                (" - " ++) . showTerm n (-x) . showTerms NonInitial (n + 1) xs
        showTerms NonInitial n (x : xs) =
                (" + " ++) . showTerm n x . showTerms NonInitial (n + 1) xs
        showTerm 0 0 = ("0" ++)
        showTerm 0 x = showsPrec 6 x
        showTerm 1 1 = ("x" ++)
        showTerm 1 (-1) = ("- x" ++)
        showTerm 1 x = showsPrec 6 x . (" * x" ++)
        showTerm n (-1) = ("- x^" ++) . showsPrec 6 n
        showTerm n 1 = ("x^" ++) . showsPrec 6 n
        showTerm n x = showsPrec 6 x . (" * x^" ++) . showsPrec 6 n

homoCoefficients :: (Show a, Num a) => Int -> Int -> Homogeneous a -> [a]
homoCoefficients n d h@(H d' n' cs) | n' < n = homoCoefficients n d (upgrade n h)
homoCoefficients n d (H d' n' cs) = elems cs
homoCoefficients n d Zero = take (hdim' n d) (repeat 0)

extractCoeffs :: Int -> Multivariate Q -> [[Q]]
extractCoeffs n (M (F xs)) = 
  let extractCoeffs' n d (x:xs) = homoCoefficients n d x : extractCoeffs' n (d + 1) xs
  in extractCoeffs' n 0 (xs ++ repeat Zero)

sh :: [[a]] -> [[a]]
sh (x : xs) = [head x] : zipWith (:) (tail x) (sh xs)

-- combine :: [Formal Q] -> Multivariate Q
-- combine xs =
--   let ys = map (\x -> unF x ++ repeat 0) xs
--       cs = sh ys
--   in M $ F [H d 2 (listArray (0, d) a) | (d, a) <- zip [0 ..] cs]

makeH n d bs = 
  let s = hdim' n d
  in if s /= length (bs)
    then error ("Wrong # of coefficients d = " ++ show d ++ " n = " ++ show n ++ " s = " ++ show s ++ " len(bs) =" ++ show (length bs))
    else H d n (listArray (0, s - 1) bs)

-- sh :: [[a]] -> [[a]]
-- sh (x : xs) = [head x] : zipWith (:) (tail x) (sh xs)

combine :: [Formal Q] -> Multivariate Q
combine xs =
  let ys = map (\x -> unF x ++ repeat 0) xs
      cs = sh ys
  in M $ F [H d 2 (listArray (0, d) a) | (d, a) <- zip [0 ..] cs]

gf2 :: [Multivariate Q] -> Multivariate Q
gf2 xs =
  let bs' = sh (map (extractCoeffs 2) xs)
      bs = bs'
--       makeH d bs = H d (n + 1) (listArray (0, hdim' (n + 1) d - 1) bs) -- Original
--       gf' d (b : bs) = makeH d (concat b) : gf' (d + 1) bs
      gf' d (b : bs) = makeH 3 d (concat (sh b)) : gf' (d + 1) bs
  -- that sh thing is a gross hack that works for n=2
  in M (F (gf' 0 bs))
