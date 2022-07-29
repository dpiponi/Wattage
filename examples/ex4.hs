import Prelude hiding (iterate)
import Formal as F

main = do
  let x = F.var :: Formal Q

  let n = 10
  let f = sum [(1 + x)^i | i <- [0 .. n - 1]] - fromIntegral n

  -- A054474 Number of walks on square lattice that start and end at
  -- origin after 2n steps, not touching origin at intermediate stages.
  -- http://oeis.org/A054474
  -- # of valid terms doubles with each iteration so 10 is
  -- good enough for 1000 terms.
  print $ F.truncate 10 $ inverse f
