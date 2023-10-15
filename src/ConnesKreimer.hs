{-# LANGUAGE FlexibleInstances #-}

module ConnesKreimer where

import Data.List
import Vector
import qualified VectorSpace as VS
import Control.Arrow

data Tree = T [Tree] deriving (Show, Eq, Ord)

urtimes x y = return (sort $ x ++ y) -- <- XXX commutative!

times2' :: (Num a, Eq a) => Vector a ([Tree], [Tree]) -> Vector a ([Tree], [Tree]) -> Vector a ([Tree], [Tree])
times2' = lift2 $ cotensor2 urtimes urtimes

forestT v = [T v]

-- https://arxiv.org/pdf/hep-th/9808042.pdf
delta' :: Linear Float [Tree] ([Tree], [Tree])
delta' xs = foldr times2' (return ([], [])) $ map delta'' xs
  where delta'' u@(T t) = return ([u], []) VS.+ fmap (second forestT) (delta' t)

instance (Show a, Num a, Eq a) => Num (Vector a [Tree]) where
  fromInteger n = fromList [([], fromInteger n)]
  a + b = a VS.+ b
  a * b = lift2 urtimes a b
  negate = VS.negate
  abs a = a
  signum a = 1

instance Fractional CK where
  fromRational x = fromList [([], fromRational x)]
  recip x = error "recip"
  x / y = error "/"

type CK = Vector Rational [Tree]
