{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vector where

import qualified VectorSpace as VS
import qualified Data.Map as M
import Control.Arrow

data Vector f x where
  Pure :: x -> Vector a x
  FromList :: [(x, f)] -> Vector f x
  FromMap :: Ord x => M.Map x f -> Vector f x

vmscale a = fmap (a *)

vlscale a = map (second (a *))

simplify :: (Eq f, Num f) => M.Map x f -> M.Map x f
simplify = M.filter (/= 0)

fromMap :: (Eq f, Num f, Ord x) => M.Map x f -> Vector f x
fromMap = FromMap . simplify

fromList :: (Eq f, Num f, Ord x) => [(x, f)] -> Vector f x
fromList = fromMap . M.fromListWith (+)

vscale :: (Eq f, Num f) => f -> Vector f x -> Vector f x
vscale a (Pure x) = FromList [(x, a)]
vscale a (FromList x) = FromList $ vlscale a x
vscale a (FromMap x) = fromMap $ vmscale a x

plus :: (Num a, Eq a, Eq x, Ord x) => Vector a x -> Vector a x -> Vector a x
x `plus` y = fromList (vectorToList x ++ vectorToList y)

makeFlat :: (Eq f, Num f) => [(Vector f x, f)] -> Vector f x
makeFlat [] = FromList []
makeFlat [(x, f)] = vscale f x
makeFlat ((Pure x, a) : y) = makeFlat ((FromList [(x, 1)], a) : y)
makeFlat ((FromMap x, f) : y) =
  case makeFlat y of
    FromMap z -> fromMap $ M.unionWith (+) (vmscale f x) z
    FromList z -> fromMap $ M.unionWith (+) (vmscale f x) (M.fromListWith (+) z)
makeFlat ((FromList x, f) : y) =
  case makeFlat y of
    FromMap z -> fromMap $ M.unionWith (+) (M.fromListWith (+) $ vlscale f x) z
    FromList z -> FromList $ vlscale f x ++ z

instance Num f => Functor (Vector f) where
  fmap f (FromList x) = FromList (map (first f) x)
  fmap f (FromMap x) = FromList $ map (first f) $ M.toList x

instance (Eq a, Num a) => Applicative (Vector a) where
  pure = Pure
  f <*> x = do
    f0 <- f
    x0 <- x
    return (f0 x0)

instance (Eq a, Num a) => Monad (Vector a) where
  m >>= f = makeFlat . map (first f) $ vectorToList m

vectorToList (Pure x) = [(x, 1)]
vectorToList (FromList x) = x
vectorToList (FromMap x) = M.toList x

instance (Num a, Eq a, Ord x, Eq x) => Eq (Vector a x) where
  a == b = vectorToList a == vectorToList b

instance (Num f, Show f, Show x) => Show (Vector f x) where
  show x = show (vectorToList x)

instance (Eq f, Num f, Ord x) => VS.VectorSpace f (Vector f x) where
  (+) = plus
  (*) = vscale

type Linear a b c = b -> Vector a c

-- XXX We may need to sort, depending on the order
-- this gives us.
tensor :: Monad m => m x -> m y -> m (x, y)
tensor xs ys = do
  x <- xs
  y <- ys
  return (x, y)

lift :: Monad m => (x -> m y) -> (m x -> m y)
lift = flip (>>=)

lift2 :: Monad m => (x -> y -> m z) -> (m x -> m y -> m z)
lift2 f x y = do
  u <- x
  v <- y
  f u v

cotensor2 :: Monad m => (a -> b -> m c) -> (d -> e -> m f) -> (a, d) -> (b, e) -> m (c, f)
cotensor2 f g (u, v) (s, t) = f u s `tensor` g v t
