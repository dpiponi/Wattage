import Prelude hiding (iterate)
import Formal as W

iterate 0 f x = x
iterate n f x = iterate (n - 1) f (f x)

agm (x, y) = ((x + y) / 2, sqrt (x * y))

main = do
  let x = W.var :: Formal Q

  let f = 1 - 4 * x
  let g = 1 + 4 * x

  -- A054474 Number of walks on square lattice that start and end at
  -- origin after 2n steps, not touching origin at intermediate stages.
  -- http://oeis.org/A054474
  print $ W.truncate 1000 $ fst $ iterate 10 agm (f, g)
