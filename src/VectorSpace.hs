{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module VectorSpace where

import qualified Prelude as P
import Data.Ratio

class P.Num f => VectorSpace f v | v -> f where
  zero :: v
  (+) :: v -> v -> v
  (*) :: f -> v -> v
  negate :: v -> v
  negate v = (-1) * v

instance VectorSpace P.Integer P.Integer where
  zero = 0
  (+) = (P.+)
  (*) = (P.*)

instance VectorSpace P.Float P.Float where
  zero = 0
  (+) = (P.+)
  (*) = (P.*)

instance VectorSpace P.Double P.Double where
  zero = 0
  (+) = (P.+)
  (*) = (P.*)

instance P.Integral a => VectorSpace (Ratio a) (Ratio a) where
  zero = 0
  (+) = (P.+)
  (*) = (P.*)
