-- {-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.List
import Debug.Trace

import Vector
import qualified VectorSpace as VS
import Formal as F
import qualified ConnesKreimer as CK

bplus = fmap (fmap (\x -> [CK.T x]))

-- type CK = Vector Rational [CK.Tree]
zzz :: Formal CK.CK
zzz = 1 +* bplus (zzz ^ 2)

-- https://arxiv.org/pdf/1512.03027.pdf p.7
zzzz :: Formal CK.CK
zzzz = 1 +* infiniteSum' 1 (\n ->
  (shiftRight (fromInteger n) $ (1 / fromInteger (factorial (n + 1))) VS.*
            (bplus (zzzz ^ fromInteger (n + 1)))))

-- https://arxiv.org/pdf/1512.03027.pdf
f1 :: Formal CK.CK
f1 = 0 +* bplus (exp f1)

star x = e where e = 1 +* (e * ftail x)

f2 :: Formal CK.CK
f2 = 0 +* bplus (star f2)

f3 :: Formal CK.CK
f3 = 0 +* bplus ((1 + f3) ^ 3)

f4 :: Formal CK.CK
f4 = 1 +* bplus (f4 * f4)

f5 :: Formal CK.CK
f5 = 1 +* negate (bplus (1 / f5))

g :: Formal CK.CK
g = 1 +* bplus (g + g * g)

nintegrate :: Integer -> Double -> Double -> (Double -> Double) -> Double
nintegrate n a b f =
  let h = (b - a) / fromInteger n
      in h * (f a + 4 * sum [f (a + (2 * fromInteger i - 1) * h) | i <- [1..n `div` 2]] + 2 * sum [f (a + 2 * fromInteger i * h) | i <- [1..(n `div` 2 - 1)]] + f b) / 3

interpret1 :: CK.Tree -> (Double -> Double) -> (Double -> Double)
interpret1 (CK.T ts) f x = nintegrate 16 0 x (interpret2 ts f)

interpret2 :: [CK.Tree] -> (Double -> Double) -> (Double -> Double)
interpret2 [] f x = f x
interpret2 ts f x = product (map (\t -> interpret1 t f x) ts)

it f x = 1 + nintegrate 16 0 x (\x -> (f x)^2)

main = do
--   print $ F.truncate 5 $ g
--   print $ F.truncate 6 $ zzzz
--   print $ F.truncate 6 $ f1
--   print $ F.truncate 6 $ star z
--   print $ F.truncate 6 $ f3
--   print $ F.truncate 6 $ f4
--   print $ F.truncate 6 $ f2
--   print $ interpret2 [CK.T [CK.T [], CK.T [CK.T [], CK.T []]]] (\x -> 1/(1+x)) 1
--   print $ it (it (it (\x -> 1 / (1 + x)))) 1
--   let q0 = 1 :: Formal CK.CK
--   print $ F.truncate 5 $ q0
--   let q1 = 1 +* bplus (q0 * q0)
--   print $ F.truncate 5 $ q1
--   let q2 = 1 +* bplus (q1 * q1)
--   print $ F.truncate 5 $ q2
  let x = 1 +* bplus (x * x) in print $ F.truncate 5 $ x
