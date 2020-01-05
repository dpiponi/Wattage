import Poly
import Wattage as W
import Homogeneous as H hiding (test)
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

main = do
  defaultMain tests

tests = testGroup "Tests"
    [ testBasic,
      testTranscendental,
      testAnnihilators,
      testHypergeometric,
      testHomogeneous,
      testMultivariate ]

-- Basic functionality
testBasic = testGroup "Tests of basic functionality"
    [ testCase "Finite convolution 0*0" testFiniteConvolution00,
      testCase "Finite convolution 0*1" testFiniteConvolution01,
      testCase "Finite convolution 1*0" testFiniteConvolution10,
      testCase "Finite convolution 1*1" testFiniteConvolution11,
      testCase "Finite convolution (x+1)²" testFiniteConvolutionPoly1,
      testCase "Finite convolution (x+2)(2x+3)" testFiniteConvolutionPoly2
    ]

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
      testCase "inverse sin == asin" testInverseSin
    ]

testInverseSin =
    ftake 5 (inverse (sin z)) @?= ftake 5 (asin z)

testComposeSin =
    take 20 ((unF (sin (sin z)))) @?= (take 20 ((unF (sin z) `compose` unF (sin z)) :: [Rational]))

testComposeAsinExp =
    take 20 ((unF (asin (exp z-1)))) @?= (take 20 ((unF (asin z) `compose` unF (exp z-1)) :: [Rational]))

testLogExp =
    let u = - z^2 + z^3 - z^4 :: Formal Q
    in ftake 5 (log (exp u)) @?= ftake 5 u

testExpLog =
    let u = 1 - z^2 + z^3 - z^4 :: Formal Q
    in ftake 5 (exp (log u)) @?= ftake 5 u

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
    in ftake 5 (exp (u + v)) @?= ftake 5 (exp u * exp v)

testLogIsHomomorphism =
    let u = 1 - z + z^3 - z^4 :: Formal Q
        v = 1 + 3 * z - 2 * z^2 - 5 * z^4
    in ftake 5 (log (u * v)) @?= ftake 5 (log u + log v)

testSqrtViaLog =
    let u = 1 - z + z^3 - z^4 :: Formal Q
    in ftake 5 (sqrt u) @?= ftake 5 (exp ((1 / 2) * log u))

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
      testCase "exp is hypergeometric" testExpHypergeometric,
      testCase "Clausen's formula" testClausenFormula,
      testCase "Kummer's relation" testKummerRelation,
      testCase "Identity 3" testIdentity3]

testDilogarithmHypergeometric =
    ftake 10 (dilog z :: Formal Q) @?= ftake 10 (z*hypergeometric [1, 1, 1] [2, 2] z)

testRationalHypergeometric =
    let a = 1/3 :: Q
    in ftake 10 (hypergeometric [a] [] z) @?= ftake 10 (exp (log (1-z)*(-fromRational a)))

testExpHypergeometric =
    ftake 10 (hypergeometric [] [] z) @?= ftake 10 (exp z)

testIdentity3 =
    let a = 1/3
        b = 1/5
        lhs = exp(-z/2)*hypergeometric [a, 1+b] [2*a+1, b] z :: Formal Q
        term1 = hypergeometric [] [a+1/2] (z^2/16)
        term2 = mapf ((1-(2*a)/b)/(2*(2*a+1)) *) (z*hypergeometric [] [a+3/2] (z^2/16)) :: Formal Q
        rhs = term1 - term2
    in ftake 10 lhs @?= ftake 10 rhs

-- http://mathworld.wolfram.com/KummersRelation.html
testKummerRelation =
  let a = 1/3
      b = 1/5
      u = f21 (2*a) (2*b) (a+b+1/2) z - f21 a b (a+b+1/2) (4*z*(1-z)) :: Formal Q
  in ftake 10 u @?= take 10 (repeat 0)

-- https://en.wikipedia.org/wiki/Clausen%27s_formula
testClausenFormula =
 let a = 1/2
     b = 1/3
     c = 1/4
     s = 1/5
     u = hypergeometric [2*c-2*s-1,2*s,c-1/2] [2*c-1,c] z - (f21 (c-s-1/2) s c z)^2 :: Formal Q
 in ftake 10 u @?= take 10 (repeat 0)

-- Homogeneous polynomial tests
testHomogeneous = testGroup "Homogeneous polynomial tests"
    [testCase "Match zero" testMatchZero,
     testCase "Integration 1" testHomogeneousIntegration1,
     testCase "Integration 2" testHomogeneousIntegration2,
     testCase "Integration 3" testHomogeneousIntegration3,
     testCase "Implicit variables" testImplicitVariables]

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
    assertEqual "hderiv 0 x01 == hderiv 0 x02" (hderiv 0 x01) (hderiv 0 x02)
    assertEqual "hderiv 1 x01 == hderiv 1 x02" (hderiv 1 x01) (hderiv 1 x02)
    assertEqual "hderiv 2 x01 == hderiv 2 x02" (hderiv 2 x01) (hderiv 2 x02)
    assertEqual "hderiv 0 (x01 * x01) == hderiv 0 (x02 * x02)" (hderiv 0 (x01 * x01)) (hderiv 0 (x02 * x02))
    assertEqual "hderiv 1 (x01 * x01 * x12) == hderiv 1 (x02 * x02 * x12)"
                 (hderiv 0 (x01 * x01 * x12))
                 (hderiv 0 (x02 * x02 * x12))

testHomogeneousIntegration1 = 
  let x0 = make_var 0 2
      x1 = make_var 1 2
  in hint 0 x1 @?= x0 * x1

testHomogeneousIntegration2 = 
  let x0 = make_var 0 1
  in hint 0 1 @?= x0

testHomogeneousIntegration3 = 
  let x0 = make_var 0 1
      x1 = make_var 1 1
      x2 = make_var 2 1
  in hint 2 (x0 * x0 * x1 * x1 * x2 * x2) @?= (1 / 3) * x0 * x0 * x1 * x1 * x2 * x2 * x2

-- Multivariate tests
testMultivariate = testGroup "Multivariate tests" 
    [ testCase "Hurwitz numbers" testHurwitzNumbers]

type MFormal a = Formal (Homogeneous a)

testHurwitzNumbers =
  let x0 = H.var 0
      x1 = H.var 1
      z0 = F $ Zero : x0 : repeat Zero :: MFormal Rational
      z1 = F $ Zero : x1 : repeat Zero :: MFormal Rational
      x = z0
      y = z1
      intY = pint 1
      pint i xs = 0 `prepend` mapf (hint i) xs
      h x y = intY (intY (exp (h x (y * exp x) - 2 * h x y + h x (y * exp (-x)))) / y)
      term10 = unF (h x y) !! 10
  in term10 @?= (1 / 80640) * x0 * x0 * x0 * x0 * x0 * x0 * x0 * x0 * x1 * x1 + (1 / 6) * x0 * x0 * x0 * x0 * x0 * x0 * x1 * x1 * x1 * x1
