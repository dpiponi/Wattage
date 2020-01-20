# Wattage
A small library for working with formal power series.
Supports both univariate and multivariate power series.
Multivariate power series are basically univariate power series where the coefficient of xⁿ is a homogeneous polynomial of total degree n.

See `examples/` for examples for things like:

  * `ex1`: computing Gromov-Witten invariants
  * `ex2`: computing numbers of heaps of tetris-like pieces
  * `ex3`: counting numbers of walks on lattices

From `ex2`:

  ```haskell
    -- |  □| 
    -- |  □| y2
    -- |   |
    -- | □ | 
    -- | □ | y1
    -- |   |
    -- |□  | 
    -- |□  | y0
    -- |   |
    -- | □□| x1
    -- |   |
    -- |□□ | x0
    -- +---+
    let trivial = 1 - x0 - x1 - y0 - y1 - y2
                    + x0*y2 + y0*x1 + y0*y1 + y0*y2 + y1*y2
                    - y0*y1*y2

    let heaps = 1 / trivial

An example of use. Say we weant to find `w(z)` where `w*exp(w) == z`. This is Lagrange reversion:

    > :m Formal
    > let x = var :: Formal Q -- Formal series with rational coefficients
    > F.truncate 15 $ inverse $ (x * exp x)
    x - x² + 3 % 2 * x³ - 8 % 3 * x⁴ + 125 % 24 * x⁵ - 54 % 5 * x⁶ + 16807 % 720 * x⁷ - 16384 % 315 * x⁸ + 531441 % 4480 * x⁹ - 156250 % 567 * x¹0 + 2357947691 % 3628800 * x¹1 - 2985984 % 1925 * x¹2 + 1792160394037 % 479001600 * x¹3 - 7909306972 % 868725 * x¹4

These are the coefficients of the [Lambert W function](https://en.wikipedia.org/wiki/Lambert_W_function).
