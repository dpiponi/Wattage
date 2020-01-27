{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array
import qualified Data.Array.MArray as M
import Data.Array.ST
import Control.Monad
import qualified Data.List as L
import Control.Monad.ST
import Data.Ratio
import qualified Data.Map as Map

type Z = Integer
type Q = Ratio Integer

type Permutation = Array Z Z

fromList :: Z -> [(Z, Z)] -> Permutation
fromList n as = array (0, n - 1) as

transpose :: Z -> Z -> Z -> Permutation
transpose n a b = fromList n [(i, if i == a then b else (if i == b then a else i)) | i <- [0..n-1]]

identity n = fromList n [(i, i) | i <- [0..n-1]]

compose :: Permutation -> Permutation -> Permutation
compose a b = let m = snd (bounds a)
              in fromList (m + 1) [(i, a ! (b ! i)) | i <- [0 .. m]]

getCycles :: Permutation -> [Z]
getCycles a = L.reverse $ L.sort $ runST $ do
  let m = snd (bounds a)
  flags :: STArray s Z Bool <- M.newArray (0, m) False
  a' <- M.thaw a :: ST s (STArray s Z Z)
  let cycles_from i | i > m = return []
      cycles_from i = do
      j <- readArray flags i
      if j then cycles_from (i + 1)
        else do
          let cycle_length i k | i == k = return 1
              cycle_length i k = do
                writeArray flags k True
                l <- readArray a' k
                n <- cycle_length i l
                return (n + 1)
          writeArray flags i True
          k <- readArray a' i
          r <- cycle_length i k
          c <- cycles_from (i + 1)
          return $ (r : c)
  cycles_from 0

checkIdentity n p = p == identity n

transpositions :: Integer -> [Permutation]
transpositions n = do
  i <- [0 .. n-2]
  j <- [i+1 .. n-1]
  return $ transpose n i j

hurwitz :: Z -> [Z] -> Q
hurwitz m mu =
  let n = sum mu
      ps = (do
              etas <- replicateM (fromIntegral m) (transpositions n)
              let p = foldr compose (identity n) etas
              guard $ getCycles p == mu
              return p)
--   in ps
  in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

hurwitz'' :: Z -> Z -> Q
hurwitz'' m n =
  let ps = (do
              etas <- replicateM (fromIntegral m) (transpositions n)
              let p = foldr compose (identity n) etas
              guard $ checkIdentity n p
              return p)
--   in ps
  in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

-- hurwitz :: Z -> [Z] -> Q
hurwitz' m mu =
  let n = sum mu
      ps = (do
              etas <- replicateM (fromIntegral m) $ do
                    i <- [0 .. n-2]
                    j <- [i+1 .. n-1]
                    return $ transpose n i j
              let p = foldr compose (identity n) etas
              guard $ getCycles p == mu
              return p)
  in ps
--   in (fromIntegral (length ps) / fromIntegral (product [1 .. n]))

fact n = product [1..n]

cycleCounts mu = map snd (Map.toList (Map.fromListWith (+) (zip mu (repeat 1))))

naut mu = product (map fact (cycleCounts mu))

hh m mu =
  let smu = sum mu
      n = m + 2 - smu
  in fromIntegral (fact m) /
    ((fromIntegral (naut mu)) * product [((fromIntegral (mu!!i))^(mu!!i))/(fromIntegral $ fact (mu!!i))*(fromIntegral $ mu!!i)^(n-3) | i <- [0..n-1]])

main = do
--   print $ hurwitz 1 [2]
--   print $ hurwitz 4 [3]
--   print $ hurwitz 2 [2, 2] -- I expect 1/2 ???
--   print $ hurwitz 0 [1, 1, 1]
--   print $ hurwitz 1 [2, 1, 1, 1]
-- 
--   print $ hh 1 [2, 1, 1, 1]

--   print $ hh 2 []

  forM_ [2,4..8] $ \n -> do
    forM_ [1..8] $ \d -> do
      let g = n `div` 2 + 1 - d
      let d0 = fromInteger d
      putStrLn $ "g=" ++ show g ++ " d=" ++ show d ++ " n=" ++ show n ++ ": " ++ show (hurwitz'' n d0 / fromIntegral (fact n))
