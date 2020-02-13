import Data.Array
import Control.Monad
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

-- Assumes i <= 0
-- adjust_down_up0 ::  HPtr -> HPtr
-- adjust_down_up0 (p, []) = (p, [])
-- adjust_down_up0 ptr = ptr

adjust_down_up1 ::  HPtr -> HPtr
adjust_down_up1 (p, []) = (p, [])
adjust_down_up1 (p, xs : xss) =
  let xs' = up xs
  in (p + head xs', xs' : xss)

allOfDegree''' :: Int -> Int -> HPtr -> [Int] -> [(Int, [Int])] -> [(Int, [Int])]
allOfDegree''' d 1 p es = ((fst p, es ++ [d]) :)
allOfDegree''' d n p es = 
  let loop i d n p es | i < 0 = id
      loop i d n p es = 
        allOfDegree''' (d - i) (n - 1) (ptail p) (es ++ [i]) .
            loop (i - 1) d n (adjust_down_up1 p) es
  in loop d d n p es

allOfDegree'' :: Int -> Int -> HPtr -> [(Int, [Int])]
allOfDegree'' d n p = allOfDegree''' d n p [] []

generator''' :: Int -> Int -> HPtr -> [Int] -> [(Int, [Int])] -> [(Int, [Int])]
generator''' d 1 p es = ((fst p, es ++ [d]) :)
generator''' d n p es = 
  let loop i d n p es | i < 0 = id
      loop i d n p es = 
        generator''' (d - i) (n - 1) (ptail p) (es ++ [i]) .
            loop (i - 1) d n (adjust_down_up1 p) es
      loop i d n p es = 
        generator''' (d - i) (n - 1) (ptail p) (es ++ [i]) .
            loop (i - 1) d n (adjust_down_up1 p) es
  in loop d d n p es

generator'' :: Int -> Int -> HPtr -> [(Int, [Int])]
generator'' d n p = generator''' d n p [] []

main = do
  print $ allOfDegree'' 2 3 (adjust_up_by [0, 1, 1] $ zero 3) [] []

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

