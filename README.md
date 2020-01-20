# Wattage
A small library for working with formal power series.
Supports both univariate and multivariate power series.
Multivariate power series are basically univariate power series where the coefficient of xⁿ is a homogeneous polynomial of total degree n.

See `examples/` for examples for things like:

  * `ex1`: computing Gromov-Witten invariants
  * `ex2`: computing numbers of heaps of tetris-like pieces
  * `ex3`: counting numbers of walks on lattices

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
