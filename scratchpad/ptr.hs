import Data.Array
import Control.Monad
import Homogeneous.Index hiding (up,down)
import qualified Homogeneous as H
import Debug.Trace

down :: [Int] -> [Int]
down [] = []
down [x] = [x]
down (x0 : x1 : xs) = x0 - x1 : down (x1 : xs)

up :: [Int] -> [Int]
up [] = []
up [x] = [x]
up (x0 : xs) = let y0 : ys = up xs in x0 + y0 : y0 : ys

type HPtr = (Int, [[Int]])

ptail :: HPtr -> HPtr
ptail (p, t) = (p, tail t)

-- value :: [[Int]] -> Int
-- value = sum . map head

adjust_down_up :: Int -> Int -> HPtr -> HPtr
adjust_down_up _ _ (p, []) = (p, [])
adjust_down_up i j (p, xs : xss) | i > 0 =
  let (p', xss') = adjust_down_up (i - 1) (j - 1) (p, xss)
  in (p', xs : xss')
adjust_down_up i j ptr | j < 1 = ptr
adjust_down_up i j (p, xs : xss) =
  let (p', xss') = adjust_down_up (i - 1) (j - 1) (p, xss)
      xs' = up xs
  in (p' + head xs', xs' : xss')

adjust_up_down :: Int -> Int -> HPtr -> HPtr
adjust_up_down _ _ (p, []) = (p, [])
adjust_up_down i j (p, xs : xss) | i > 0 =
  let (p', xss') = adjust_up_down (i - 1) (j - 1) (p, xss)
  in (p', xs : xss')
adjust_up_down i j ptr | j < 1 = ptr
adjust_up_down i j (p, xs : xss) =
  let (p', xss') = adjust_up_down (i - 1) (j - 1) (p, xss)
      xs' = down xs
  in (p' - head xs, xs' : xss')


zero :: Int -> HPtr
zero n =
  let zero' 1 = []
      zero' 2 = [[1]]
      zero' n = let xs : xss = zero' (n - 1) in (0 : xs) : xs : xss
  in (0, zero' n)

adjust_up_by ns xs =
  let adjust_by' _ [] xs = xs
      adjust_by' i (0 : ns) xs = adjust_by' (i + 1) ns xs
      adjust_by' i (n : ns) xs = adjust_by' i (n - 1 : ns) $ adjust_down_up 0 i xs
  in adjust_by' 1 (tail ns) xs

adjust_down_by ns xs =
  let adjust_by' _ [] xs = xs
      adjust_by' i (0 : ns) xs = adjust_by' (i + 1) ns xs
      adjust_by' i (n : ns) xs = adjust_by' i (n - 1 : ns) $ adjust_up_down 0 i xs
  in adjust_by' 1 (tail ns) xs

{-
allOfDegree' :: Int -> Int -> [Int] -> IO ()
allOfDegree' d 1 es = print $ (es ++ [d])
allOfDegree' d n es = do
  let loop i | i < 0 = return ()
      loop i = do
        allOfDegree' (d - i) (n-1) (es ++ [i])
        loop (i - 1)
  loop d
-}

allOfDegree' :: Int -> Int -> HPtr -> [Int] -> IO ()
allOfDegree' d 1 p es = print $ (p, es ++ [d])
allOfDegree' d n p es = do
  let loop i d n p es | i < 0 = return ()
      loop i d n p es = do
        allOfDegree' (d - i) (n-1) (ptail p) (es ++ [i])
        let p' = adjust_down_up 0 1 p
        loop (i - 1) d n p' es
  loop d d n p es

-- Assumes i <= 0
adjust_down_up' :: Int -> Int -> HPtr -> HPtr
adjust_down_up' _ _ (p, []) = (p, [])
adjust_down_up' i j ptr | j < 1 = ptr
adjust_down_up' i j (p, xs : xss) =
  let (p', xss') = adjust_down_up' (i - 1) (j - 1) (p, xss)
      xs' = up xs
  in (p' + head xs', xs' : xss')

allOfDegree'' :: Int -> Int -> HPtr -> [Int] -> [(Int, [Int])]
allOfDegree'' d 1 p es = [(fst p, es ++ [d])]
allOfDegree'' d n p es = 
  let loop i d n p es | i < 0 = []
      loop i d n p es = 
        allOfDegree'' (d - i) (n-1) (ptail p) (es ++ [i]) ++
            loop (i - 1) d n (adjust_down_up' 0 1 p) es
  in loop d d n p es

--         0
--       1   2
--     3   4   5
--   6   7   8   9
-- 10  11  12  13  14

-- diff :: (Show a, Num a) => Int -> H.Homogeneous a -> H.Homogeneous a
-- diff i (H.H d n hs) =
--   let delta = [if j == i then 1 else 0 | j <- [0 .. n - 1]]
--       ptrs = allOfDegree'' (d - 1) n (adjust_up_by delta $ zero n) []
--       size = hdim n (d - 1)
--   in H.H (d - 1) n $ listArray (0, size - 1) $
--     let x = [fromIntegral (1 + (es !! i)) * (hs ! j) | (j, es) <- ptrs]
--     in trace (show ("size", size, "hs", hs, "ptrs", ptrs, "x", x)) x

diff :: (Show a, Num a) => Int -> H.Homogeneous a -> H.Homogeneous a
diff i (H.H d n hs) =
  let delta = [if j == i then 1 else 0 | j <- [0 .. n - 1]]
      ptrs = allOfDegree'' (d - 1) n (adjust_up_by delta $ zero n) []
      size = hdim n (d - 1)
  in H.H (d - 1) n $ listArray (0, size - 1) $
    [fromIntegral (1 + (es !! i)) * (hs ! j) | (j, es) <- ptrs]

main = do
  let x0 = H.var 0 :: H.Homogeneous Int
  let x1 = H.var 1 :: H.Homogeneous Int
  let x2 = H.var 2 :: H.Homogeneous Int
  let u = x0 * x0 * x1 + 2 * x1 * x1 * x1 + 3 * x2 * x2 * x2 
  print $ u
  let v = diff 0 u
  print $ v

--   print $ allOfDegree'' 2 3 (adjust_up_by [0, 1, 1] $ zero 3) []

  -- allOfDegree' 2 3 (adjust_up_by [0, 1, 1] $ zero 3) []

  {-
  putStrLn "---"
  print $ adjust_down_up 0 1 $ zero 3
  -- Compute address directly
  print $ addr' 7 6 [1, 1, 0, 1, 0, 2, 1]
  -- Compute address by walking there one cell at a time
  let ptr = adjust_up_by [1, 1, 0, 1, 0, 2, 1] $ zero 7
  print $ ptr
  let ptr' = adjust_down_by [1, 1, 0, 1, 0, 2, 1] $ ptr
  print $ ptr'
  -}

