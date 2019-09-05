module Main where

import Poly
import Wattage

main :: IO ()
main = do
    print $ take 20 fibs

    print $ take 20 $ (reverse $ unPoly $ (hadamardProductAnnihilator (x^2-x-1) (x^2-x-1))) `lconvolve` (zipWith (*) fibs fibs)
    print $ take 20 $ (reverse $ unPoly $ (hadamardProductAnnihilator (x^3-x^2-x-1) (x^3-x^2-x-1))) `lconvolve` (zipWith (*) tribs tribs)

    print $ take 20 $ (reverse $ unPoly $ (hadamardProductAnnihilator (x^3-x^2-x-1) (x^2-x-1))) `lconvolve` (tribs .*. fibs)
