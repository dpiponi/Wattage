import Wattage
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

main = do
  defaultMain tests

testDilogarithmHypergeometric =
    take 10 (dilog z :: [Q]) @?= take 10 (z*hypergeometric [1, 1, 1] [2, 2] z)

testRationalHypergeometric =
    let a = 1/3 :: Q
    in take 10 (hypergeometric [a] [] z) @?= take 10 (exp (log (1-z)*(-fromRational a)))

testExpHypergeometric =
    take 10 (hypergeometric [] [] z) @?= take 10 (exp z)

testIdentity3 =
    let a = 1/3
        b = 1/5
        lhs = exp(-z/2)*hypergeometric [a, 1+b] [2*a+1, b] z :: [Q]
        term1 = hypergeometric [] [a+1/2] (z^2/16)
        term2 = map ((1-(2*a)/b)/(2*(2*a+1)) *) (z*hypergeometric [] [a+3/2] (z^2/16)) :: [Q]
        rhs = term1 - term2
    in take 10 lhs @?= take 10 rhs

testKummerRelation =
  let a = 1/3
      b = 1/5
      u = f21 (2*a) (2*b) (a+b+1/2) z - f21 a b (a+b+1/2) (4*z*(1-z)) :: [Q]
  in take 10 u @?= take 10 (repeat 0)

testClausenFormula =
 let a = 1/2
     b = 1/3
     c = 1/4
     s = 1/5
     u = hypergeometric [2*c-2*s-1,2*s,c-1/2] [2*c-1,c] z - (f21 (c-s-1/2) s c z)^2 :: [Q]
 in take 10 u @?= take 10 (repeat 0)

tests = testGroup "Tests" 
    [ testCase "dilogarithm is hypergeometric" testDilogarithmHypergeometric,
      testCase "1/(1-z)^a is hypergeometric" testRationalHypergeometric,
      testCase "exp is hypergeometric" testExpHypergeometric,
      testCase "Clausen's formula" testClausenFormula,
      testCase "Kummer's relation" testKummerRelation,
      testCase "Identity 3" testIdentity3]
