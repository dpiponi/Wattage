{-# LANGUAGE TypeOperators #-}

module Wattage where

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
(^+) a b = zipWithIdentity (+) 0 a b
(^-) a b = zipWithIdentity (-) 0 a b

type Q = Rational

type Series a = [a]

fibs :: (Eq a, Num a) => [a]
fibs = 0 : 1 : fibs + tail fibs

fibAnn = [-1, -1, 1]

lucas :: (Eq a, Num a) => [a]
lucas = 2 : 1 : lucas + tail lucas

lucasAnn = [-1, -1, 1]

tribs :: (Eq a, Num a) => [a]
tribs = 0 : 0 : 1 : tribs + tail tribs + tail (tail tribs)

tribAnn = [-1, -1, -1, 1]

perrin :: (Eq a, Num a) => [a]
perrin = 3 : 0 : 2 : perrin + tail perrin

perrinAnn = [-1, -1, 0, 1]

sample a = map (flip count a) [0..20]

-- convolve a b
-- a must not be finite, b may be
-- result is not finite
~(a : as) `convolve` bbs@(b : bs) = (a *! b) :
    ((map (a !*) bs) ^+ (as `convolve` bbs))

~(a : as) `convolve` [] = repeat 0

aas@(a : as) `lconvolve` ~(b : bs) = (a !* b) :
    ((map (*! b) as) ^+ (aas `lconvolve` bs))

[] `lconvolve` ~(a : as) = repeat 0

as `ann` bs = drop (length as - 1) (reverse as `lconvolve` bs)

compose (f : fs) (0 : gs) = f : (gs `convolve` (compose fs (0 : gs)))

inverse (0:f:fs) = x where x     = map (recip f *) (0:1:g)
                           _:_:g    = map negate (compose (0:0:fs) x)
invert x = r where r = map (/x0)  ((1:repeat 0) ^- (r `convolve` (0:xs)))
                   x0:xs = x 

(^/) (0:a) (0:b) = a ^/ b
(^/) a b = a `convolve` (invert b)

z :: Fractional a => [a]
z = 0:1:repeat 0

eval :: Num b => [b] -> b -> b
eval [] x = 0
eval (a:as) x = a+x*eval as x

d (_:x) = zipWith (*) (map fromInteger [1..]) x

integrate x = 0 : zipWith (/) x (map fromInteger [1..])

square x = x `convolve` x

instance (Eq r, Num r) => Num [r] where
    x+y  = zipWith (+) x y
    x-y  = zipWith (-) x y
    ~x*y = x `convolve` y
    fromInteger x      = fromInteger x:repeat 0
    negate x     = map negate x
    signum (x:_) = signum x:repeat 0
    abs (x:xs)   = error "Can't form abs of a power series"

instance (Eq r, Fractional r) => Fractional [r] where
    x/y = x ^/ y
    fromRational x    = fromRational x:repeat 0

sqrt' x = 1:rs where rs = map (/2) (xs ^- (rs `convolve` (0:rs)))
                     _:xs = x
instance (Eq r, Fractional r) => Floating [r] where
    sqrt (1:x) = sqrt' (1:x)
    sqrt _      = error "Can only find sqrt when leading term is 1"
    exp x      = e where e = 1+integrate (e * d x)
    log x      = integrate (d x/x)
    sin x      = integrate ((cos x)*(d x))
    cos x      = [1] ... negate (integrate ((sin x)*(d x)))
    asin x      = integrate (d x/sqrt(1-x*x))
    atan x      = integrate (d x/(1+x*x))
    acos x      = error "Unable to form power series for acos"
    sinh x      = integrate ((cosh x)*(d x))
    cosh x      = [1] ... integrate ((sinh x)*(d x))
    asinh x      = integrate (d x/sqrt(1+x*x))
    atanh x      = integrate (d x/(1-x*x))
    acosh x      = error "Unable to form power series for acosh"
    pi       = error "There is no formal power series for pi"

cbrt x = exp (map (/ 3) $ log x)

t :: (Eq a, Num a) => ([a])
t = 0:1:repeat 0
t' :: [Rational]
t' = t

lead [] x = x
lead (a:as) x = a : (lead as (tail x))
a ... x = lead a x

one = t'
list x     = 1/(1-x)
set x     = exp x
ring x     = -log(1-x)
pair x     = x*x
oneOf a b   = a+b
necklace x  = -log(1-x)/2+x/2+x*x/4
union a b   = a*b

(//) :: Fractional a => [a] -> (Integer -> Bool) -> [a]
(//) a c = zipWith (\a-> \b->(if (c a :: Bool) then b else 0)) [(0::Integer)..] a

nonEmpty a = a // (/= 0)

count n a = ((a!!(fromInteger n)) * (factorial (fromInteger n)))

tree x = p where p = [0] ... union (set p) x

graph = [2^((n*(n-1) `div` 2)) / product (map fromInteger [1..n]) | n <- [0..]] :: [Rational]

connectedGraph = 1 + log graph

delta (g : gs) h = let g' = delta gs h
                   in (0 : ((1 : h) * g')) + gs

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
itlog :: (Eq a, Fractional a) => [a] -> [a]
itlog f@(0 : 1 : _) = itlog' f 1 0 z
         where itlog' f n t z = take (n+1) t ... 
                    let pz = p f z
                    in itlog' f (n+1) (t ^- map (((-1)^n / fromIntegral n) *) pz) pz

-- |The 'itexp' function computes the inverse of the iterative logarithm of
--  its argument.
--  See https://www.math.ucla.edu/~matthias/pdf/zvonkine.pdf
itexp f@(0 : 0 : _) = itexp' f 0 t' 1
itexp' f total term n = take (n - 1) total ...
            itexp' f (total + term) (map (/fromIntegral n) (f*d term)) (n+1)

itsqrt x = itexp ((itlog x) / 2)
itpow x n = itexp ((itlog x) * n)

-- hypergeometric
f01 :: (Num a, Fractional a, Eq a) => a -> [a] -> [a]
f01 a x = let f an a n = an : f (an/(a*n)) (a+1) (n+1)
          in  f 1 a 1  `compose` x

f21 :: (Num a, Fractional a, Eq a) => a -> a -> a -> [a] -> [a]
f21 a b c x = let f abcn a b c n = abcn : f (abcn*a*b/(c*n)) (a+1) (b+1) (c+1) (n+1)
              in  f 1 a b c 1 `compose` x

hypergeometric :: (Num a, Fractional a, Eq a) => [a] -> [a] -> Series a -> Series a
hypergeometric a b x = let f abn a b n = abn : f (abn*product a/(n * product b)) (map (+1) a) (map (+1) b) (n+1)
                       in  f 1 a b 1 `compose` x

-- 2/pi * ellipticK
scaledEllipticK x = f21 (1/2) (1/2) 1 (x*x)
scaledEllipticE x = f21 (-1/2) (1/2) 1 (x*x)

dilog x = integrate $ -log (1-x)*d x/x

-- Theta constants
-- θ₂(z) = z^(1/4)*modifiedTheta2 z
modifiedTheta2 q = let t = 2 : (intercalate [2] $ map (flip replicate 0) [1, 3..])
                   in t `compose` q

theta3 q = let t = 1 : (intercalate [2] $ map (flip replicate 0) [0, 2..])
           in t `compose` q

theta4 q = let t = 1 : (intercalate [2] $ map (flip replicate 0) [0, 2..])
           in (zipWith (*) t (cycle [1, -1])) `compose` q

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

besselJ :: (Eq x, Fractional x) => Integer -> [x] -> [x]
besselJ n x = let scale = 1/fromRational (fromIntegral (factorial n))
              in map (scale *) $ f01 (fromInteger n + 1) (-x^2 / 4) * (x / 2)^n
