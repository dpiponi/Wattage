module Multivariate where

import Formal as F
import Homogeneous as H

type Multivariate a = Formal (Homogeneous a)

var :: (Show a, Num a) => Int -> Multivariate a
var i = F [Zero, make_var i (i + 1)]

coefficient :: (Eq a, Num a, Show a) => Exponent -> Multivariate a -> a
coefficient is f = H.coefficient is (F.coefficient (sum is) f)

integrate :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
integrate i xs = 0 `prepend` mapf (hint i) xs

d :: (Num a, Eq a, Show a, Fractional a) => Int -> Multivariate a -> Multivariate a
d i xs = 0 `prepend` mapf (hint i) xs
