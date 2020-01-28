import Data.Array
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
-- import Control.Monad.Plus
import Data.Ratio
import Debug.Trace
import Data.Function.Memoize
import Control.Applicative

fact n = product [1 .. n]

-- Brute Force up to Isomorphism monad.
-- This is a monad for counting the number of ways you can put a
-- structure on [0 .. n-1] in a way that permuting these
-- integers doesn't change whether it's a structure.
--
-- Eg. here's a list of permutations on S_4
--   a = [(0, 1), (1, 2), (3, 4)]
-- This permutation is essentially the same kind of thing:
--   b = [(0, 2), (2, 1), (3, 4)]
-- We just swapped 1 and 2 which were arbitrary labels.
-- When we drew that first 1 in `a` it was a completely 'new' number,
-- so we could have drawn 2 or 3 instead, all three of these
-- numbers are perfectly good representatives.
-- Rather than actually draw all three, we just draw 1 and register
-- in the `WriterT` layer that this represents 3 distinct
-- possibilities.
-- The `StateT` layer is used to record how many 'new' numbers
-- we've drawn so far.
type BFI a = WriterT (Product Q) (StateT Int []) a
runBFI = flip evalStateT 0 . runWriterT
choose :: [a] -> BFI a
choose = lift . lift

-- Draw any integer in range [0 .. n-1] except that if we're
-- drawing a number not draw before we might as well draw the
-- smallest unused number and register how many possibilities
-- it represents.
anyInteger :: Int -> BFI Int
anyInteger n = do
  s <- get
  let left = choose [0 .. s - 1]
  let right = do
      guard (s < n)
      -- Although we drew the number `s`
      -- there were `n - s` numbers we could have
      -- drawn.
      tell (Product (fromIntegral (n - s)))
      modify (+ 1)
      return s
  left <|> right

count :: Num b => [(a, Product b)] -> b
count = sum . map (getProduct . snd)

-- Permutations
type Z = Integer
type Q = Ratio Integer

type Permutation = Array Int Int

fromList :: Int -> [(Int, Int)] -> Permutation
fromList n as = array (0, n - 1) as

identity = memoize $ \n -> fromList n [(i, i) | i <- [0..n-1]]

isIdentity n p = p == identity n

transpositions :: Int -> BFI Permutation
transpositions n = do
  i <- anyInteger n
  j <- anyInteger n
  guard (i /= j)
  -- Using the BFI monad protects against a lot of overcounting
  -- but we still have some double counting because the permutation
  -- (ij) is the same as (ji).
  -- Needs some more thought.
  tell (Product (1 / 2))
  return $ transpose n i j

compose :: Permutation -> Permutation -> Permutation
compose a b =
  let m = snd (bounds a)
  in fromList (m + 1) [(i, a ! (b ! i)) | i <- [0 .. m]]

transpose :: Int -> Int -> Int -> Permutation
transpose n a b =
  fromList n [(i,
    if i == a
      then b
      else (if i == b then a else i)) | i <- [0..n-1]]

-- Doing this so it bails out early rather than
-- doing a count for entire list.
hasExactly 0 [] = True
hasExactly n [] | n > 0 = False
hasExactly n _ | n < 0 = False
hasExactly n (False : xs) = hasExactly n xs
hasExactly n (True : xs) = hasExactly (n - 1) xs

-- Works as long as `Permutation` is well-formed.
isTransposition :: Permutation -> Bool
isTransposition p = hasExactly 2 (zipWith (/=) (elems p) [0 ..])

-- Draw composition of m permutations from S_n
someTranspositions :: Int -> Int -> BFI Permutation
someTranspositions n 0 = return (identity n)
someTranspositions n m =
  compose <$> someTranspositions n (m - 1) <*> transpositions n

hurwitz 0 d = 1 / fromIntegral (fact d)
hurwitz n d =
  -- Instead of multiplying n transpositions to see if we get the
  -- identity we multiply n-1 and see if the result is a transposition.
  let hurwitz' = do
        p <- someTranspositions d (n - 1)
        guard $ isTransposition p
        return ()
  in count (runBFI hurwitz') / fromIntegral (fact d)

main = do
  forM_ [0,2..8::Int] $ \n -> do
    forM_ [0..8::Int] $ \d -> do
      let a = hurwitz n d / fromIntegral (fact n)
      let num = numerator a
      let den = denominator a
      putStrLn $ "+(" ++ show num ++ "/" ++ show den ++
                 ")x^" ++ show n ++ " y^" ++ show d
  putStrLn ""


