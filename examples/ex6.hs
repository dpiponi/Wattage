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

i :: Num a => Integer -> a
i = fromInteger

sortOfStar x = infiniteSum' 0 (\n -> (shiftRight (i n) $
                                    (x ^ i (n + 2))))
sortOfExp x = infiniteSum' 0 (\n -> (shiftRight (i n) $
                                    (1 / i (factorial (n + 2))) VS.* (x ^ i (n + 2))))

main = do
  -- From https://arxiv.org/abs/1512.03027
  -- ex 1.4
  print $ F.truncate 5 $ let u = 1 +* bplus (u * u) in (u :: Formal CK.CK)
  -- ex 1.5
  print $ F.truncate 5 $ let v = 1 +* bplus (sortOfStar v) in v
  -- ex 1.6
  print $ F.truncate 5 $ let w = 1 +* bplus (sortOfExp w) in w

  -- From https://arxiv.org/pdf/1512.03027.pdf
  -- 9.2.1
  print $ F.truncate 6 $ let p = 0 +* bplus (exp p) in p
  -- 9.2.3
  print $ F.truncate 6 $ let z = 0 +* bplus ((1 + z) ^ 3) in z
