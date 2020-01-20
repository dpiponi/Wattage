{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Multivariate where

import Formal as F
import Homogeneous as H

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
