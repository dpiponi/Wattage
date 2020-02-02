type Z = Integer

fact :: Integer -> Integer
fact n = product [1..n]

pochhammer :: Integer -> Integer -> Integer
pochhammer x n = product [x, x+1 .. x+n-1]

-- Dimension of space of degree d polynomials in n variables
hdim' :: Int -> Int -> Int
hdim' n d = fromInteger (pochhammer (fromIntegral n) (fromIntegral d) `div` fact (fromIntegral d))

type Exponent = [Int]

-- addr :: Int -> Exponent -> Int
-- addr _ [_] = 0
-- addr deg exponents =
--   let m = length exponents - 1
--       r = deg - head exponents
--   in hdim r m + addr r (tail exponents)

addr' :: Int -> Int -> Exponent -> Int
addr' _ _ [] = 0 -- ??? XXX ???
addr' _ _ [_] = 0
addr' n deg (e : es) =
  let r = deg - e
  in hdim' r (n - 1) + addr' (n - 1) r es

decr :: [Int] -> [Int]
decr [] = []
decr [x] = [x]
decr (x0 : x1 : xs) = x0 - x1 : decr (x1 : xs)

incr :: [Int] -> [Int]
incr [] = []
incr [x] = [x]
incr (x0 : xs) = let y0 : ys = incr xs in x0 + y0 : y0 : ys

value :: [[Int]] -> Int
value = sum . map head

adjust_down_up :: Int -> Int -> [[Int]] -> [[Int]]
adjust_down_up _ _ [] = []
adjust_down_up i j (xs : xss) | i > 0 = xs : adjust_down_up (i - 1) (j - 1) xss
adjust_down_up i j xss | j < 1 = xss
adjust_down_up i j (xs : xss) = incr xs : adjust_down_up (i - 1) (j - 1) xss

adjust_up_down :: Int -> Int -> [[Int]] -> [[Int]]
adjust_up_down _ _ [] = []
adjust_up_down i j (xs : xss) | i > 0 = xs : adjust_up_down (i - 1) (j - 1) xss
adjust_up_down i j xss | j < 1 = xss
adjust_up_down i j (xs : xss) = decr xs : adjust_up_down (i - 1) (j - 1) xss

zero :: Int -> [[Int]]
zero 1 = []
zero 2 = [[0, 1]]
zero n = let xs : xss = zero (n - 1) in (0 : xs) : xs : xss

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

main = do
  -- Compute address directly
  print $ addr' 7 6 [1, 1, 0, 1, 0, 2, 1]
  -- Compute address by walking there one cell at a time
  let ptr = adjust_up_by [1, 1, 0, 1, 0, 2, 1] $ zero 7
  print $ value $ ptr
  let ptr' = adjust_down_by [1, 1, 0, 1, 0, 2, 1] $ ptr
  print $ value $ ptr'

