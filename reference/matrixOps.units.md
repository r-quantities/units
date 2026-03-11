# S3 matrixOps Group Generic Functions for units objects

matrixOps functions for units objects.

## Usage

``` r
# S3 method for class 'units'
matrixOps(x, y)
```

## Arguments

- x:

  object of class `units`, or something that can be coerced to it by
  `as_units(x)`

- y:

  object of class `units`, or something that can be coerced to it by
  `as_units(y)`

## Value

object of class `units`

## Examples

``` r
a = set_units(1:5, m)
a %*% a
#> Units: [m^2]
#>      [,1]
#> [1,]   55
a %*% t(a)
#> Units: [m^2]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    2    3    4    5
#> [2,]    2    4    6    8   10
#> [3,]    3    6    9   12   15
#> [4,]    4    8   12   16   20
#> [5,]    5   10   15   20   25
a %*% 1:5
#> Units: [m]
#>      [,1]
#> [1,]   55
1:5 %*% a
#> Units: [m]
#>      [,1]
#> [1,]   55
```
