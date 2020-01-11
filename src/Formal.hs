{-# LANGUAGE TypeOperators #-}

module Formal where

import Poly

import Data.Ratio
import Data.List hiding (union)
import Debug.Trace

(.*.) :: Num a => [a] -> [a] -> [a]
(.*.) = zipWith (*)

(*!) _ 0 = 0
(*!) a b = a*b
(!*) 0 _ = 0
(!*) a b = a*b

a ^+ [] = a
[] ^+ b = b
(a : as) ^+ (b : bs) = (a + b) : (as ^+ bs)

a ^- [] = a
[] ^- b = map negate b
(a : as) ^- (b : bs) = (a - b) : (as ^- bs)

type Q = Rational

type Series a = [a]

fibs :: (Eq a, Num a) => [a]
fibs = 0 : 1 : fibs ^+ tail fibs

fibAnn = [-1, -1, 1]

lucas :: (Eq a, Num a) => [a]
lucas = 2 : 1 : lucas ^+ tail lucas

lucasAnn = [-1, -1, 1]

tribs :: (Eq a, Num a) => [a]
tribs = 0 : 0 : 1 : tribs ^+ tail tribs ^+ tail (tail tribs)

tribAnn = [-1, -1, -1, 1]

perrin :: (Eq a, Num a) => [a]
perrin = 3 : 0 : 2 : perrin ^+ tail perrin

perrinAnn = [-1, -1, 0, 1]

sample a = map (`count` a) [0..16]

[] `convolve` _ = []
_ `convolve` [] = []
(a : as) `convolve` bbs@(b : bs) = (a *! b) :
    (map (a !*) bs) ^+ (as `convolve` bbs)

aas@(a : as) `lconvolve` ~(b : bs) = (a !* b) :
    (map (*! b) as) ^+ (aas `lconvolve` bs)

[] `lconvolve` ~(a : as) = []

as `ann` bs = drop (length as - 1) (reverse as `lconvolve` bs)

compose [] _ = []
compose (f : fs) g@(0 : gs) = f : (gs `convolve` compose fs g)
compose _ _ = error "compose requires two non-empty lists, the second starting with 0"

fcompose (F a) (F b) = F $ compose a b

-- Lagrangian reversion
inverse' (0:f:fs) = u where   u = 0 : x
                              x = map (/f) (1:g)
                              g = map negate ((x `convolve` x) `convolve` compose fs u)

inverse' _ = error "inverse applicable only to non-empty lists starting with 0"
inverse (F xs) = F $ inverse' xs

-- reciprocal
-- untested
invert [] = error "Divide by zero"
invert x = r where r = map (/ x0)  (1 : map negate (r `convolve` xs))
                   x0 : xs = x 

-- Note:
-- Seems like map ((1 / x0) *) would be faster than map (/ x0)
-- But reciprocal doesn't always exist. Eg. for
-- Homogeneous polynomials.
divide _ [] = error "Divide by zero"
divide [] _ = []
divide (y : ys) (x0 : xs) = r where r = map (/ x0)  (y : (ys ^- (r `convolve` xs)))

(^/) (0 : a) (0 : b) = a ^/ b
(^/) a [b] = map (/ b) a
(^/) a b = divide a b

z :: Fractional a => Formal a
z = F [0, 1]

var :: Num a => Formal a
var = F [0, 1]

eval :: Num b => [b] -> b -> b
eval [] x = 0
eval (a:as) x = a+x*eval as x

d (F []) = 0
d (F (_:x)) = F $ zipWith (*) (map fromInteger [1..]) x

dlist (_:x) = zipWith (*) (map fromInteger [1..]) x
dlist _ = error "You can only differentiate non-empty lists"

integrate (F x) = F $ 0 : zipWith (/) x (map fromInteger [1..])

square x = x `convolve` x

newtype Formal a = F { unF :: [a] }

data Position = Initial | NonInitial

instance (Show a, Num a, Eq a, Ord a) => Show (Formal a) where
    showsPrec _ (F []) = ("0" ++)
    showsPrec p (F x) = showParen (p >= 6) $ showTerms Initial 0 x where
        showTerms _ _ [] = error "Shouldn't be showing empty list of terms"
        showTerms Initial n ([0]) = ("0" ++)
        showTerms Initial n ([x]) = showTerm n x
        showTerms NonInitial n ([0]) = id
        showTerms NonInitial n ([x]) | x < 0 = (" - " ++) . showTerm n (-x)
        showTerms NonInitial n ([x]) = (" + " ++) . showTerm n x
        showTerms position n (0 : xs) = showTerms position (n + 1) xs
        showTerms Initial n (x : xs) = showTerm n x . showTerms NonInitial (n + 1) xs
        showTerms NonInitial n (x : xs) | x < 0 = (" - " ++) . showTerm n (-x) . showTerms NonInitial (n + 1) xs
        showTerms NonInitial n (x : xs) = (" + " ++) . showTerm n x . showTerms NonInitial (n + 1) xs
        showTerm 0 0 = ("0" ++)
        showTerm 0 x = showsPrec 6 x
        showTerm 1 1 = ("x" ++)
        showTerm 1 (-1) = ("- x" ++)
        showTerm 1 x = showsPrec 6 x . (" * x" ++)
        showTerm n (-1) = ("- x^" ++) . showsPrec 6 n
        showTerm n 1 = ("x^" ++) . showsPrec 6 n
        showTerm n x = showsPrec 6 x . (" * x^" ++) . showsPrec 6 n


-- Only good for finite sequences
instance (Num a, Eq a) => Eq (Formal a) where
    F x == F [] = all (== 0) x
    F [] == F x = all (== 0) x
    F (x : xs) == F (y : ys) = x == y && F xs == F ys

mapf :: (a -> b) -> Formal a -> Formal b
mapf f (F xs) = F $ map f xs

instance (Eq r, Num r) => Num (Formal r) where
    F x+F y  = F $ x ^+ y
    F x - F y  = F $ x ^- y
    F x * F y = F $ x `convolve` y
    fromInteger x      = F [fromInteger x]
    negate (F x)     = F $ map negate x
    signum _ = error "signum only applicable to non-empty lists"
    abs _   = error "Can't form abs of a power series"

instance (Show r, Eq r, Fractional r) => Fractional (Formal r) where
    F x/F y = F $ x ^/ y
    fromRational x    = F [fromRational x]

sqrt' x = 1:rs where rs = map (/ 2) (xs ^- (0 : (rs `convolve` rs)))
                     _:xs = x

-- Test performance. XXX
-- It may be that the naive approach of summing powers may work
-- better than some of these differential equation tricks.
instance (Show r, Eq r, Fractional r) => Floating (Formal r) where
    sqrt (F (1:x)) = F $ sqrt' (1:x)
    sqrt _      = error "Can only find sqrt when leading term is 1"
    exp x      = e where e = 1+integrate (e * d x) -- XXX throws away leading term
    log x      = integrate (d x / x)
    sin (F []) = F []
    sin x@(F (0:_))      = integrate (cos x * d x)
    sin _ = error "Formal power series of sin requires zero first term"
    cos (F []) = F [1]
    cos x@(F (0:_))      = [1] ... negate (integrate (sin x * d x))
    cos _ = error "Formal power series of cos requires first term zero"
    asin (F []) = F []
    asin x@(F (0:_))      = integrate (d x / sqrt (1-x*x))
    asin _ = error "Formal power series of asin requires zero first term"
    atan x      = integrate (d x / (1+x*x))
    acos x      = error "Unable to form power series for acos"
    sinh x      = integrate (cosh x * d x)
    cosh x      = [1] ... integrate (sinh x * d x)
    asinh x      = integrate (d x / sqrt (1+x*x))
    atanh x      = integrate (d x / (1-x*x))
    acosh x      = error "Unable to form power series for acosh"
    pi       = error "There is no formal power series for pi"

cbrt x = exp (mapf (/ 3) $ log x)

t :: (Eq a, Num a) => Formal a
t = F [0, 1]
t' :: Formal Rational
t' = t

lead [] x = x
lead (a:as) x = a : lead as (tail x)
a ... F x = F $ lead a x

one = t'
list x     = 1/(1-x)
set     = exp
ring x     = -log(1-x)
pair x     = x*x
oneOf a b   = a+b
necklace x  = -log(1-x)/2+x/2+x*x/4
union a b   = a*b

-- Filter
(//) :: Fractional a => [a] -> (Integer -> Bool) -> [a]
(//) a c = zipWith (\a b -> if (c a :: Bool) then b else 0) [(0::Integer)..] a

nonEmpty a = a // (/= 0)

count n a = (a!!fromInteger n) * factorial (fromInteger n)

tree x = p where p = [0] ... union (set p) x

graph = F [2^(n*(n-1) `div` 2) / product (map fromInteger [1..n]) | n <- [0..]] :: Formal Rational

connectedGraph = 1 + log graph

delta (g : gs) h = let g' = delta gs h
                   in (0 : ((1 : h) `convolve` g')) ^+ gs
delta _ _ = error "First argument to delta must be non-empty"

-- fsqrt (0 : 1 : fs) =
--     let gs = (fs-(0 : gs*((0 : delta gs gs)+((2 : gs)*(gs*g)))))/2
--         g = 0 : 1 : gs
--     in g

p f t = (t `compose` f) ^- t

-- |The 'itlog' function computes the iterative logarithm of
--  its argument.
--  See https://www.math.ucla.edu/~matthias/pdf/zvonkine.pdf
--  `itlog` has these properties:
--  `itlog . itexp = id`
--  `itexp . itlog = id`
--  `itexp (n * itlog f) = f . f . ... n times ... f`
--  `itexp (-itlog f) = invert f`
itlog :: (Eq a, Fractional a) => Formal a -> Formal a
itlog f@(F (0 : 1 : _)) = F $ itlog' (unF f) 1 (unF 0) (unF z)
         where itlog' f n t z = take (n+1) t `lead`
                    let pz = p f z
                    in itlog' f (n+1) (t ^- map ( ((-1)^n / fromIntegral n) *) pz) pz
itlog _ = error "itlog only applicable to series starting z+..."

-- |The 'itexp' function computes the inverse of the iterative logarithm of
--  its argument.
--  See https://www.math.ucla.edu/~matthias/pdf/zvonkine.pdf
itexp f@(F (0 : 0 : _)) = F $ itexp' (unF f) (unF 0) (unF t') 1
itexp _ = error "itexp only applicable to series starting a*z^2+..."
itexp' :: (Num a, Eq a, Fractional a) => [a] -> [a] -> [a] -> Int -> [a]
itexp' f total term n = take (n - 1) total `lead`
            itexp' f (total ^+ term) (map (/fromIntegral n) (f `convolve` dlist term)) (n+1)

itsqrt x = itexp (itlog x / 2)
itpow x n = itexp (itlog x * n)

prepend :: a -> Formal a -> Formal a
prepend a (F bs) = F (a : bs)

ftail :: Formal a -> Formal a
ftail (F []) = error "Can't get ftail of empty list"
ftail (F (_ : xs)) = F xs

ftake :: Int -> Formal a -> [a]
ftake i (F as) = take i as

truncate :: Int -> Formal a -> Formal a
truncate i (F as) = F $ take i as

trunc :: Int -> Formal a -> Formal a
trunc i (F as) = F (take i as)

-- hypergeometric
f01 :: (Num a, Fractional a, Eq a) => a -> Formal a -> Formal a
f01 a x = let f an a n = an `prepend` f (an/(a*n)) (a+1) (n+1)
          in  f 1 a 1  `fcompose` x

f21 :: (Num a, Fractional a, Eq a) => a -> a -> a -> Formal a -> Formal a
f21 a b c x = let f abcn a b c n = abcn `prepend` f (abcn*a*b/(c*n)) (a+1) (b+1) (c+1) (n+1)
              in  f 1 a b c 1 `fcompose` x

hypergeometric :: (Num a, Fractional a, Eq a) => [a] -> [a] -> Formal a -> Formal a
hypergeometric a b x = let f abn a b n = abn `prepend` f (abn*product a/(n * product b)) (map (+1) a) (map (+1) b) (n+1)
                       in  f 1 a b 1 `fcompose` x

-- 2/pi * ellipticK
scaledEllipticK x = f21 (1/2) (1/2) 1 (x*x)
scaledEllipticE x = f21 (-1/2) (1/2) 1 (x*x)

dilog x = integrate $ -log (1-x)*d x/x

-- Theta constants
-- θ₂(z) = z^(1/4)*modifiedTheta2 z
modifiedTheta2 (F q) = let t = 2 : intercalate [2] (map (`replicate` 0) [1, 3..])
                       in F $ t `compose` q

theta3 (F q) = let t = 1 : intercalate [2] (map (`replicate` 0) [0, 2..])
               in F $ t `compose` q

theta4 (F q) = let t = 1 : intercalate [2] (map (`replicate` 0) [0, 2..])
               in F $ zipWith (*) t (cycle [1, -1]) `compose` q

-- sumOfSquares = (theta3 z)^2

-- (theta4 z)⁴-(theta3 z)⁴+z*(modifiedTheta2 z)⁴ = 0

-- 2-1/(f21 (1/2) (1/2) (1) (16*z*z)) :: [Rational]

data FreeNum = Var String
             | FreeNum :+ FreeNum
             | FreeNum :- FreeNum
             | FreeNum :* FreeNum
             | FromInteger Integer
             | Negate FreeNum
             | Signum FreeNum
             | Abs FreeNum
             | FreeNum :/ FreeNum
             | FromRational Rational
             deriving (Eq)

instance Show FreeNum where
    show (Var x) = x
    show (a :+ b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (a :- b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (a :* b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show (a :/ b) = "(" ++ show a ++ "/" ++ show b ++ ")"
    show (Negate a) = "(-" ++ show a ++ ")"
    show (Abs a) = "abs(" ++ show a ++ ")"
    show (Signum a) = "signum(" ++ show a ++ ")"
    show (FromInteger i) = show i
    show (FromRational r) = show r

instance Num FreeNum where
    (+) = (:+)
    (-) = (:-)
    (*) = (:*)
    fromInteger = FromInteger
    negate = Negate
    signum = Signum
    abs = Abs

instance Fractional FreeNum where
    (/) = (:/)
    fromRational = fromRational

-- main = do
--     -- OK, I'm not the first to produce this sequence of coefficients
--     -- http://math.stackexchange.com/a/209653
--     let a = itlog (t'+t'^2)
--     mapM_ print $ take 20 a
-- 
--     let b = itlog (sin t')
--     let c = itexp (b/2)
--     -- http://oeis.org/A048602
--     -- http://oeis.org/A048603
--     mapM_ print $ take 20 c
--     let d = itexp (-b)
--     -- http://oeis.org/A055786
--     -- http://oeis.org/A002595
--     mapM_ print $ take 20 d
--     -- http://oeis.org/A052132
--     -- http://oeis.org/A052135
--     let e = itexp (b/3)
--     mapM_ print $ take 20 e
--     
--     -- See bottom of page 14
--     -- http://www.springer.com/cda/content/document/cda_downloaddocument/9783642004490-c1.pdf
--     -- or top of page 8
--     -- https://www.itp.uni-hannover.de/~flohr/papers/w-cft-survivalkit.pdf
--     let f = itlog (t'/(1-t'))
--     mapM_ print $ take 20 f
-- 
--     let g = itlog (t'/sqrt(1-2*t'^2))
--     mapM_ print $ take 20 g
--     let h = itlog (t'/cbrt(1-3*t'^3))
--     mapM_ print $ take 20 h
-- 
--     -- http://oeis.org/A004148
--     let k = itexp (t'^2/(1-t'^2))
--     mapM_ print $ take 20 k
-- 
--     mapM_ print $ take 20 $ itexp ((sin t')^2)
--     mapM_ print $ take 20 $ atan (tan t'/(1-tan t'))

-- factorial :: (Eq n, Num n) => n -> n
factorial 0 = 1
factorial n = n*factorial (n-1)

besselJ :: (Show x, Eq x, Fractional x) => Integer -> Formal x -> Formal x
besselJ n x = let scale = 1/fromRational (fromIntegral (factorial n))
              in mapf (scale *) $ f01 (fromInteger n + 1) (-x^2 / 4) * (x / 2)^n

coefficient :: (Eq a, Num a) => Int -> Formal a -> a
coefficient _ (F []) = 0
coefficient 0 (F (x : _)) = x
coefficient i (F (x : xs)) = coefficient (i - 1) (F xs)
