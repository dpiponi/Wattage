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
  show (M x) = show x
