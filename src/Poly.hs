module Poly where

newtype Poly a = Poly [a] deriving (Eq, Show)

zipWithIdentity f e (a : as) (b : bs) = f a b : zipWithIdentity f e as bs
zipWithIdentity f e [] bs = fmap (f e) bs
zipWithIdentity f e as [] = fmap (flip f e) as

-- Finite convolution
(a : as) `fconvolve` bbs@(b : bs) = a * b :
    (zipWithIdentity (+) 0 (map (a *) bs) (as `fconvolve` bbs))

[] `fconvolve` _ = []

_ `fconvolve` [] = []

del _ [] = error "del only applicable to non-empty lists"
del 0 (_ : xs) = xs
del n (x : xs) = x : del (n-1) xs

padRight 0 xs = xs
padRight n [] = take n $ repeat 0
padRight n (x : xs) = x : padRight (n-1) xs

pad n = take n $ repeat 0

det :: Num a => [[a]] -> a
det [[a]] = a
det (m : ms) = sum $
    [ a * s * (det (map (del i) ms)) |
     (i, s, a) <- zip3 [0..] (cycle [1, -1]) m]
det _ = error "det only applicable to lists of lists"

instance (Eq r, Num r) => Num (Poly r) where
    Poly x + Poly y = Poly $ zipWithIdentity (+) 0 x y
    Poly x - Poly y = Poly $ zipWithIdentity (-) 0 x y
    Poly x*Poly y   = Poly $ x `fconvolve` y
    fromInteger x   = Poly [fromInteger x]
    negate (Poly x) = Poly $ map negate x
    signum _        = error "Can't form signum of a polynomial"
    abs _           = error "Can't form abs of a power series"

x :: Num a => Poly a
x = Poly [0, 1]

constantPoly :: Num a => a -> Poly a
constantPoly a = Poly [a]

sylvester (Poly p) (Poly q) = 
    let m = length p
        n = length q
        s = (m - 1) + (n - 1)
        prows = [padRight s (pad i ++ p) | i <- [0..(s - m)]]
        qrows = [padRight s (pad i ++ q) | i <- [0..(s - n)]]
    in prows ++ qrows

resultant p q = det $ sylvester p q

polyReverse (Poly p) = Poly (reverse p)
polyLift (Poly p) = Poly $ map constantPoly p

sumAnnihilator p q = p * q

transpose [] = []
transpose [xs] = map return xs
transpose (x : xs) = zipWith (:) x (transpose xs)

unPoly (Poly a) = a

polySwap (Poly p) = Poly (map Poly (transpose (map unPoly p)))

hadamardProductAnnihilator :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
hadamardProductAnnihilator (Poly p) (Poly q) =
    let p' = map constantPoly p
        m = length q
        q' = zipWith (*) (map constantPoly (reverse q)) [x^i | i <- [m, (m-1)..]]
    in resultant (Poly p') (Poly q')
