{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multivariate(var,
                    coefficient,
                    integrate,
                    truncate,
                    d,
                    ogf,
                    Multivariate(..)) where

import Prelude hiding (truncate)
import qualified Formal as F
import Formal(Q, Formal(..))
import qualified Homogeneous as H
import Homogeneous(Homogeneous(..))
import Data.Array as A

newtype Multivariate a = M { unM :: F.Formal (Homogeneous a) }
  deriving (Num, Fractional, Floating)

var :: (Show a, Num a) => Int -> Multivariate a
var i = M (F [Zero, H.make_var i (i + 1)])

coefficient :: (Eq a, Num a, Show a) => H.Exponent -> Multivariate a -> a
coefficient is (M f) = H.coefficient is (F.coefficient (sum is) f)

integrate :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
integrate i (M xs) = M $ 0 `F.prepend` fmap (H.hint i) xs

d :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
d i (M xs) = M $ 0 `F.prepend` fmap (H.hint i) xs

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
    showsPrec p (M (F x)) = showParen (p >= 6) $ showTerms F.Initial 0 x where
        showTerms _ _ [] = error "Shouldn't be showing empty list of terms"
        showTerms F.Initial n ([0]) = ("0" ++)
        showTerms F.Initial n ([x]) = showTerm n x
        showTerms F.NonInitial n ([0]) = id
        showTerms F.NonInitial n ([x]) | signum x == -1 = (" - " ++) . showTerm n (-x)
        showTerms F.NonInitial n ([x]) = (" + " ++) . showTerm n x
        showTerms position n (0 : xs) = showTerms position (n + 1) xs
        showTerms F.Initial n (x : xs) = showTerm n x . showTerms F.NonInitial (n + 1) xs
        showTerms F.NonInitial n (x : xs) | signum x == -1 =
                (" - " ++) . showTerm n (-x) . showTerms F.NonInitial (n + 1) xs
        showTerms F.NonInitial n (x : xs) =
                (" + " ++) . showTerm n x . showTerms F.NonInitial (n + 1) xs
        showTerm 0 0 = ("0" ++)
        showTerm 0 x = showsPrec 6 x
        showTerm 1 1 = ("x" ++)
        showTerm 1 (-1) = ("- x" ++)
        showTerm 1 x = showsPrec 6 x . (" * x" ++)
        showTerm n (-1) = ("- x^" ++) . showsPrec 6 n
        showTerm n 1 = ("x^" ++) . showsPrec 6 n
        showTerm n x = showsPrec 6 x . (" * x^" ++) . showsPrec 6 n

extractCoeffs :: Int -> Multivariate Q -> [[Q]]
extractCoeffs n (M (F xs)) = 
  let extractCoeffs' n d (x:xs) = H.allCoefficients n d x : extractCoeffs' n (d + 1) xs
  in extractCoeffs' n 0 (xs ++ repeat Zero)

staggeredColumns :: [[a]] -> [[a]]
staggeredColumns (x : xs) = [head x] : zipWith (:) (tail x) (staggeredColumns xs)

-- combine :: [F.Formal Q] -> Multivariate Q
-- combine xs =
--   let ys = map (\x -> unF x ++ repeat 0) xs
--       cs = sh ys
--   in M $ F [H d 2 (listArray (0, d) a) | (d, a) <- zip [0 ..] cs]

-- sh :: [[a]] -> [[a]]
-- sh (x : xs) = [head x] : zipWith (:) (tail x) (sh xs)

ogf :: [F.Formal Q] -> Multivariate Q
ogf xs =
  let ys = map (\x -> unF x ++ repeat 0) xs
      cs = staggeredColumns ys
  in M $ F [H d 2 (listArray (0, d) a) | (d, a) <- zip [0 ..] cs]

ogf2 :: [Multivariate Q] -> Multivariate Q
ogf2 xs =
  let bs = staggeredColumns (map (extractCoeffs 2) xs)
      ogf2' d (b : bs) = H.fromAllCoefficients 3 d (concat (staggeredColumns b)) : ogf2' (d + 1) bs
  in M (F (ogf2' 0 bs))

truncate :: Int -> Multivariate a -> Multivariate a
truncate i (M as) = M (F.truncate i as)
