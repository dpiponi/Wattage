-- https://www.math.ucla.edu/~pak/papers/bess8.pdf
--
import Prelude hiding (iterate)
import Formal as F

x = F.var :: Formal Q

-- Sylvester identity
lhs = infiniteProduct [1 + x ^ k | k <- [1 ..]]
rhs = zipWith (/) [x ^ k | k <- scanl1 (+) [1 ..]]
                  --------------------------------------
                  (scanl1 (*) [(1 - x^k) | k <- [1 ..]])

main = do
--   mapM_ print $ take 10 $ map (F.truncate 10) rhs
  print $ F.truncate 200 lhs
  print $ F.truncate 200 $ 1 + infiniteSum rhs
