# Combine R Objects by Rows or Columns

S3 methods for `units` objects (see
[`cbind`](https://rdrr.io/r/base/cbind.html)).

## Usage

``` r
# S3 method for class 'units'
cbind(..., deparse.level = 1)

# S3 method for class 'units'
rbind(..., deparse.level = 1)
```

## Arguments

- ...:

  (generalized) vectors or matrices. These can be given as named
  arguments. Other R objects may be coerced as appropriate, or S4
  methods may be used: see sections ‘Details’ and ‘Value’. (For the
  `"data.frame"` method of `cbind` these can be further arguments to
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) such as
  `stringsAsFactors`.)

- deparse.level:

  integer controlling the construction of labels in the case of
  non-matrix-like arguments (for the default method):  
  `deparse.level = 0` constructs no labels;  
  the default `deparse.level = 1` typically and `deparse.level = 2`
  always construct labels from the argument names, see the ‘Value’
  section below.

## Examples

``` r
x <- set_units(1, m/s)
y <- set_units(1:3, m/s)
z <- set_units(8:10, m/s)
(m <- cbind(x, y)) # the '1' (= shorter vector) is recycled
#> Units: [m/s]
#>      x y
#> [1,] 1 1
#> [2,] 1 2
#> [3,] 1 3
(m <- cbind(m, z)[, c(1, 3, 2)]) # insert a column
#> Units: [m/s]
#>      x  z y
#> [1,] 1  8 1
#> [2,] 1  9 2
#> [3,] 1 10 3
(m <- rbind(m, z)) # insert a row
#> Units: [m/s]
#>   x  z  y
#>   1  8  1
#>   1  9  2
#>   1 10  3
#> z 8  9 10
```
