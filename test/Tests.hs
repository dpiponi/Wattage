{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (truncate)
import Poly
import Formal as F hiding ((:+), (:-), (:*))
import Homogeneous as H hiding (test)
import Multivariate as M
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment
import Test.Tasty.QuickCheck

infixl 6 :+
infixl 6 :-
infixl 7 :*

main = do
  defaultMain tests

tests = testGroup "Tests"
    [ testBasic,
      testTranscendental,
      testAnnihilators,
      testHypergeometric,
      testHomogeneous,
      testMultivariate,
      testItlog,
      testOutput,
      testFormalProperties ]

testFormalProperties = testGroup "Formal properties" 
    [ testAddCommutes,
      testMultiplyCommutes,
      testSubtraction,
      testSinAsinProperty,
      testCosSinProperty,
      testLogExpProperty ]

-- XXX Should also allow infinite streams.
instance Arbitrary a => Arbitrary (Formal a) where
    arbitrary = F <$> arbitrary

testAddCommutes = testProperty "Addition commutes" $ \x y -> (x :: Formal Q) + (y :: Formal Q) == y + x

testMultiplyCommutes = testProperty "Multiplication commutes" $ \x y -> (x :: Formal Q) * (y :: Formal Q) == y * x

testSubtraction = testProperty "Subtraction" $ \x -> (x :: Formal Q) - x == 0

testSinAsinProperty = testProperty "sin . asin == id" $
    \x -> let y = z * x :: Formal Q
          in truncate 5 (sin (asin y)) == truncate 5 y

testCosSinProperty = testProperty "cos^2 x + sin^2 x == 1" $
    \x -> let y = z * x :: Formal Q
          in truncate 5 (cos y ^ 2 + sin y ^ 2) == 1

testLogExpProperty = testProperty "log . exp == id" $
    \x -> let y = z * x :: Formal Q
          in truncate 5 (log (exp y)) == truncate 5 y

testOutput = testGroup "Tests of string representation"
    [ testCase "Single variable" testSingleVariable,
      testCase "Homogeneous polynomials" testHomogeneousOut
    ]

-- This is used to ensure precedence rules are correct for Show
data Expr a = Const a | a :+ a | a :- a | a :* a deriving Show

testSingleVariable = do
    show (0 :: Formal Q) @?= "0"
    show (z - z :: Formal Q) @?= "0"
    show (z^16 - z^32 - z^16 + z^32 :: Formal Q) @?= "0"
    show (1 :: Formal Q) @?= "1 % 1"
    show (z :: Formal Q) @?= "x"
    show (1 + z :: Formal Q) @?= "1 % 1 + x"
    show (1 + z^2 :: Formal Q) @?= "1 % 1 + x^2"
    show (1 - z^2 :: Formal Q) @?= "1 % 1 - x^2"
    show (1 + 2*z^2 :: Formal Q) @?= "1 % 1 + 2 % 1 * x^2"
    show (2 + 3*z + 5*z^2 :: Formal Q) @?= "2 % 1 + 3 % 1 * x + 5 % 1 * x^2"
    show (-1 :: Formal Q) @?= "(-1) % 1"
    show (-z :: Formal Q) @?= "- x"
    show (-z^2 :: Formal Q) @?= "- x^2"
    show (-z^2 - z^3 :: Formal Q) @?= "- x^2 - x^3"
    show (1 - z :: Formal Q) @?= "1 % 1 - x"
    show (1 - 2*z :: Formal Q) @?= "1 % 1 - 2 % 1 * x"
    show (-1 - 2*z :: Formal Q) @?= "(-1) % 1 - 2 % 1 * x"
    show (-1 - 2*z + 3*z^2 :: Formal Q) @?= "(-1) % 1 - 2 % 1 * x + 3 % 1 * x^2"
    show (z^10 - z^20 - z^10 :: Formal Q) @?= "- x^20"
    show (1 + z^20 - z^20 :: Formal Q) @?= "1 % 1"
    show (-1 + z^20 - z^20 :: Formal Q) @?= "(-1) % 1"
    show (truncate 6 (sin z) :: Formal Q) @?= "x - 1 % 6 * x^3 + 1 % 120 * x^5"
    show (truncate 5 (asin z) :: Formal Q) @?= "x + 1 % 6 * x^3"
    take 33 (show (1 / (1 - z) :: Formal Q)) @?= "1 % 1 + x + x^2 + x^3 + x^4 + x^5"
    take 33 (show (1 / (1 + z) :: Formal Q)) @?= "1 % 1 - x + x^2 - x^3 + x^4 - x^5"
    take 27 (show (-z / (1 + z) :: Formal Q)) @?= "- x + x^2 - x^3 + x^4 - x^5"
    show (1 - z^10 :: Formal Q) @?= "1 % 1 - x^10"

testHomogeneousOut = do
  let x0 = H.var 0 :: Homogeneous Q
  let x1 = H.var 1 :: Homogeneous Q
  let x2 = H.var 2 :: Homogeneous Q
  show Zero @?= "0"
  show x0 @?= "x0"
  show (x0 + x1) @?= "x0 + x1"
  show (x0 - x1) @?= "x0 - x1"
  show (- x0) @?= "- x0"
  show (-2 * x0) @?= "- (2 % 1) * x0"
--   show (x0 :+ x0) @?= "x0 :+ x0"

-- Basic functionality
testBasic = testGroup "Tests of basic functionality"
    [ testCase "Extract coefficients" testCoefficients,
      testCase "Finite convolution 0*0" testFiniteConvolution00,
      testCase "Finite convolution 0*1" testFiniteConvolution01,
      testCase "Finite convolution 1*0" testFiniteConvolution10,
      testCase "Finite convolution 1*1" testFiniteConvolution11,
      testCase "Finite convolution (x+1)²" testFiniteConvolutionPoly1,
      testCase "Finite convolution (x+2)(2x+3)" testFiniteConvolutionPoly2
    ]

testCoefficients = do
    F.coefficient 0 1 @?= 1
    F.coefficient 1 1 @?= 0
    F.coefficient 0 z @?= 0
    F.coefficient 1 z @?= 1
    F.coefficient 2 z @?= 0
    F.coefficient 0 (z * z) @?= 0
    F.coefficient 1 (z * z) @?= 0
    F.coefficient 2 (z * z) @?= 1
    F.coefficient 3 (z * z) @?= 0

testFiniteConvolution00 = [] `fconvolve` []  @?= []
testFiniteConvolution01 = [] `fconvolve` [1]  @?= []
testFiniteConvolution10 = [1] `fconvolve` []  @?= []
testFiniteConvolution11 = [1] `fconvolve` [1]  @?= [1]
testFiniteConvolutionPoly1 = [1, 1] `fconvolve` [1, 1]  @?= [1, 2, 1]
testFiniteConvolutionPoly2 = [1, 2] `fconvolve` [2, 4]  @?= [2, 8, 8]

-- Transcendental functions
testTranscendental = testGroup "Tests of transcendental functions"
    [ testCase "exp . log" testExpLog,
      testCase "log . exp" testLogExp,
      testCase "exp is homomorphism" testExpIsHomomorphism,
      testCase "log is homomorphism" testLogIsHomomorphism,
      testCase "sqrt via log" testSqrtViaLog,
      testCase "sin . asin" testSinAsin,
      testCase "asin . sin" testAsinSin,
      testCase "tan . atan" testTanAtan,
      testCase "atan . tan" testAtanTan,
      testCase "sin `compose` sin " testComposeSin,
      testCase "asin `compose` exp " testComposeAsinExp,
      testCase "inverse sin == asin" testInverseSin,
      testCase "inverse (exp x-1) == log (1+x)" testInverseExp
    ]

testInverseSin =
    truncate 5 (inverse (sin z)) @?= truncate 5 (asin z)

testInverseExp =
    truncate 5 (inverse (exp z - 1)) @?= (truncate 5 (log (1 + z)) :: Formal Q)

testComposeSin =
    truncate 20 (((sin (sin z)))) @?= (truncate 20 (((sin z) `fcompose` (sin z)) :: Formal Q))

testComposeAsinExp =
    truncate 20 (((asin (exp z-1)))) @?= (truncate 20 (((asin z) `fcompose` (exp z-1)) :: Formal Q))

testLogExp =
    let u = - z^2 + z^3 - z^4 :: Formal Q
    in truncate 5 (log (exp u)) @?= truncate 5 u

testExpLog =
    let u = 1 - z^2 + z^3 - z^4 :: Formal Q
    in truncate 5 (exp (log u)) @?= truncate 5 u

testSinAsin =
    let u = z + z^2 - z^3 :: Formal Q
    in trunc 5 (sin (asin u)) @?= trunc 5 u

testAsinSin =
    let u = z + z^2 - z^3 :: Formal Q
    in trunc 5 (asin (sin u)) @?= trunc 5 u

testTanAtan =
    let u = z + z^2 - z^3 :: Formal Q
    in trunc 5 (tan (atan u)) @?= trunc 5 u

testAtanTan =
    let u = z + z^2 - z^3 :: Formal Q
    in trunc 5 (atan (tan u)) @?= trunc 5 u

testExpIsHomomorphism =
    let u = z + z^3 - z^4 :: Formal Q
        v = z^2 - 2 * z^3 + 5 * z^4
    in truncate 5 (exp (u + v)) @?= truncate 5 (exp u * exp v)

testLogIsHomomorphism =
    let u = 1 - z + z^3 - z^4 :: Formal Q
        v = 1 + 3 * z - 2 * z^2 - 5 * z^4
    in truncate 5 (log (u * v)) @?= truncate 5 (log u + log v)

testSqrtViaLog =
    let u = 1 - z + z^3 - z^4 :: Formal Q
    in truncate 5 (sqrt u) @?= truncate 5 (exp ((1 / 2) * log u))

-- Annihilators (this is work in progress and may be deleted)
testAnnihilators = testGroup "Tests of annihilators"
    [ testCase "Fibonacci" testFibonacciAnnihilator,
      testCase "Tribonacci" testTribonacciAnnihilator
    ]

zero20 = take 20 (repeat 0)

testFibonacciAnnihilator =
    take 20 (drop 2 (reverse (unPoly (x^2 - x - 1)) `lconvolve` fibs)) @?= zero20

testTribonacciAnnihilator =
    take 20 (drop 3 (reverse (unPoly (x^3 - x^2 - x - 1)) `lconvolve` tribs)) @?= zero20

-- Hypergeometric tests
testHypergeometric = testGroup "Hypergeometric tests" 
    [ testCase "dilogarithm is hypergeometric" testDilogarithmHypergeometric,
      testCase "1/(1-z)^a is hypergeometric" testRationalHypergeometric,
      testCase "asin is hypergeometric" testAsinHypergeometric,
      testCase "exp is hypergeometric" testExpHypergeometric,
      testCase "log(1+x) is hypergeometric" testLogHypergeometric,
      testCase "Clausen's formula" testClausenFormula,
      testCase "Kummer's relation" testKummerRelation,
      testCase "Identity 3" testIdentity3,
      testCase "theta identities" testThetaIdentities]

testThetaIdentities = do
    truncate 20 ((theta3 (z^2))^2 :: Formal Q) @?= truncate 20 (((theta3 z)^2 + (theta4 z)^2) / 2)
    truncate 20 ((theta4 (z^2))^2 :: Formal Q) @?= truncate 20 (sqrt ((theta3 z)^2 * (theta4 z)^2))

testDilogarithmHypergeometric =
    truncate 20 (dilog z :: Formal Q) @?= truncate 20 (z*hypergeometric [1, 1, 1] [2, 2] z)

testRationalHypergeometric =
    let a = 1/3 :: Q
    in truncate 20 (hypergeometric [a] [] z) @?= truncate 20 (exp (log (1-z)*(-fromRational a)))

testAsinHypergeometric =
    truncate 20 (asin z) @?= truncate 20 (z * f21 (1 / 2) (1 / 2) (3 / 2) (z ^ 2))

testLogHypergeometric =
    truncate 20 (log (1 + z)) @?= truncate 20 (z * f21 1 1 2 (-z))

testExpHypergeometric =
    truncate 20 (hypergeometric [] [] z) @?= truncate 20 (exp z)

testIdentity3 =
    let a = 1/3
        b = 1/5
        lhs = exp(-z/2)*hypergeometric [a, 1+b] [2*a+1, b] z :: Formal Q
        term1 = hypergeometric [] [a+1/2] (z^2/16)
        term2 = mapf ((1-(2*a)/b)/(2*(2*a+1)) *) (z*hypergeometric [] [a+3/2] (z^2/16)) :: Formal Q
        rhs = term1 - term2
    in truncate 20 lhs @?= truncate 20 rhs

-- http://mathworld.wolfram.com/KummersRelation.html
testKummerRelation =
  let a = 1/3
      b = 1/5
      u = f21 (2*a) (2*b) (a+b+1/2) z - f21 a b (a+b+1/2) (4*z*(1-z)) :: Formal Q
  in truncate 20 u @?= 0

-- https://en.wikipedia.org/wiki/Clausen%27s_formula
testClausenFormula =
 let a = 1/2
     b = 1/3
     c = 1/4
     s = 1/5
     u = hypergeometric [2*c-2*s-1,2*s,c-1/2] [2*c-1,c] z - (f21 (c-s-1/2) s c z)^2 :: Formal Q
 in truncate 20 u @?= 0

-- Homogeneous polynomial tests
testHomogeneous = testGroup "Homogeneous polynomial tests"
    [testCase "Match zero" testMatchZero,
     testCase "Division" testDivision,
     testCase "Extract coefficients" testHomogeneousCoefficients,
     testCase "Integration 1" testHomogeneousIntegration1,
     testCase "Integration 2" testHomogeneousIntegration2,
     testCase "Integration 3" testHomogeneousIntegration3,
     testCase "Implicit variables" testImplicitVariables]

testDivision = do
    let x0 = make_var 0 2
    let x1 = make_var 1 2
    x0 / x0 @?= 1
    x0 / 1 @?= x0
    x1 / x1 @?= 1
    x1 * x1 / x1 @?= x1
    0 / x0 @?= 0
    0 / x1 @?= 0
    (x0 - x0) * x1 / x1 @?= 0
    (x0 - x1) / (x0 - x1) @?= 1
    (x0^2 - 2 * x0 * x1 + x1^2) /(x0 - x1) @?= x0 - x1
    x0^10 * x1^11 /(x0^5 * x1^5) @?= x0^5 * x1^6
    0 / (x0 + x1) @?= 0
    (x0 + x1)^10 / (x0 + x1)^9 @?= x0 + x1

testHomogeneousCoefficients = do
    let x0 = make_var 0 2
    let x1 = make_var 1 2
    H.coefficient [1, 0] x0 @?= 1
    H.coefficient [0, 1] x0 @?= 0
    H.coefficient [1, 0] x1 @?= 0
    H.coefficient [0, 1] x1 @?= 1
    H.coefficient [2, 0] (x0 * x0) @?= 1
    H.coefficient [1, 1] (x0 * x0) @?= 0
    H.coefficient [0, 2] (x0 * x0) @?= 0
    H.coefficient [2, 0] (x0 * x1) @?= 0
    H.coefficient [1, 1] (x0 * x1) @?= 1
    H.coefficient [0, 2] (x0 * x1) @?= 0
    H.coefficient [2, 0] (x1 * x1) @?= 0
    H.coefficient [1, 1] (x1 * x1) @?= 0
    H.coefficient [0, 2] (x1 * x1) @?= 1

testMatchZero = do
    let x0 = make_var 0 2
    let x1 = make_var 1 2
    let checkZero 0 = True
        checkZero _ = False
    assertBool "checkZero Zero" $ checkZero Zero
    assertBool "checkZero 0" $ checkZero 0
    assertBool "not (checkZero 1)" $ not (checkZero 1)
    assertBool "not (checkZero x0)" $ not (checkZero x0)
    assertBool "checkZero (x0 - x0)" $ checkZero (x0 - x0)
    assertBool "not (checkZero (x0 - x1))" $ not (checkZero (x0 - x1))

testImplicitVariables = do
    let x01 = make_var 0 1
    let x02 = make_var 0 2
    let x12 = make_var 1 2
    assertEqual "x01 == x02" x01 x02
    assertEqual "x01 - x01 == 0" (x01 -x02) 0
    assertEqual "x01 * x02 == x02 * x02" (x01 * x02) (x02 * x02)
    assertEqual "H.d 0 x01 == H.d 0 x02" (H.d 0 x01) (H.d 0 x02)
    assertEqual "H.d 1 x01 == H.d 1 x02" (H.d 1 x01) (H.d 1 x02)
    assertEqual "H.d 2 x01 == H.d 2 x02" (H.d 2 x01) (H.d 2 x02)
    assertEqual "H.d 0 (x01 * x01) == H.d 0 (x02 * x02)" (H.d 0 (x01 * x01)) (H.d 0 (x02 * x02))
    assertEqual "H.d 1 (x01 * x01 * x12) == H.d 1 (x02 * x02 * x12)"
                 (H.d 0 (x01 * x01 * x12))
                 (H.d 0 (x02 * x02 * x12))

testHomogeneousIntegration1 = 
  let x0 = make_var 0 2
      x1 = make_var 1 2
  in H.integrate 0 x1 @?= x0 * x1

testHomogeneousIntegration2 = 
  let x0 = make_var 0 1
  in H.integrate 0 1 @?= x0

testHomogeneousIntegration3 = 
  let x0 = make_var 0 1
      x1 = make_var 1 1
      x2 = make_var 2 1
  in H.integrate 2 (x0 * x0 * x1 * x1 * x2 * x2) @?= (1 / 3) * x0 * x0 * x1 * x1 * x2 * x2 * x2

-- Multivariate tests
testMultivariate = testGroup "Multivariate tests" 
    [ testCase "Multivariate coefficients" testMultivariateCoefficients,
      testCase "Hurwitz numbers" testHurwitzNumbers]

testMultivariateCoefficients = do
    let x0 = M.var 0
    let x1 = M.var 1
    let x2 = M.var 2
    M.coefficient [] 1 @?= 1
    M.coefficient [1] 1 @?= 0
    M.coefficient [1] x0 @?= 1
    M.coefficient [0, 1] x0 @?= 0
    M.coefficient [0, 1] x1 @?= 1
    M.coefficient [1, 0] x0 @?= 1
    M.coefficient [1, 0] x1 @?= 0
    M.coefficient [1, 1, 1] x0 @?= 0
    M.coefficient [1, 1, 1] (x0 ^ 2) @?= 0
    M.coefficient [1, 1, 1] (x0 ^ 3) @?= 0
    M.coefficient [1, 1, 1] (x0 * x1 * x2) @?= 1
    M.coefficient [1, 1] (x0 * x1 * x2) @?= 0
    M.coefficient [1] (x0 * x1 * x2) @?= 0
    M.coefficient [] (x0 * x1 * x2) @?= 0
    M.coefficient [] (1 + x0 * x1 * x2) @?= 1
    M.coefficient (take 10 $ repeat 1) x0 @?= 0

testHurwitzNumbers =
  let x0 = H.var 0
      x1 = H.var 1
      z0 = M.var 0 :: Multivariate Rational
      z1 = M.var 1 :: Multivariate Rational
      x = z0
      y = z1
      intY = pint 1
      pint i (M xs) = M $ 0 `prepend` fmap (H.integrate i) xs
      h x y = intY (intY (exp (h x (y * exp x) - 2 * h x y + h x (y * exp (-x)))) / y)
      term10 = F.coefficient 10 (unM $ h x y)
  in term10 @?= (1 / 80640) * x0^8 * x1^2 + (1 / 6) * x0^6 * x1^4

-- iterative logarithm
testItlog = testGroup "Iterative logarithm"
    [ testCase "itlog . itexp" testItlogItExp,
      testCase "itexp . itlog" testItExpItLog,
      testCase "iterative power" testItPower,
      testCase "fractional iterative power" testFractionalItPower,
      testCase "iterative asin" testItAsin]

testItAsin =
    truncate 5 (itexp (- itlog (sin z))) @?= truncate 5 (asin z)

testItlogItExp =
    truncate 5 (itlog (itexp (z^2 + z^3))) @?= z^2 + z^3

testItExpItLog =
    truncate 5 (itexp (itlog (z + z^2 + z^3))) @?= z + z^2 + z^3

testItPower =
    let f = sin (tan z)
        g = itexp (itlog f * 5)
    in truncate 5 g @?= truncate 5 (f `fcompose` f `fcompose` f `fcompose` f `fcompose` f)

testFractionalItPower =
    let f = sin (tan z)
        g = itexp (itlog f / 5)
    in truncate 5 f @?= truncate 5 (g `fcompose` g `fcompose` g `fcompose` g `fcompose` g)
