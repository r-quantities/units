# S3 Ops Group Generic Functions for units objects

Ops functions for units objects, including comparison, product and
divide, add, subtract.

## Usage

``` r
# S3 method for class 'units'
Ops(e1, e2)
```

## Arguments

- e1:

  object of class `units`, or something that can be coerced to it by
  `as_units(e1)`

- e2:

  object of class `units`, or something that can be coerced to it by
  `as_units(e2)`, or in case of power a number (integer n or 1/n)

## Value

object of class `units`

## Details

Users are advised against performing arithmetical operations with
temperatures in different units. The units package ensure that
results 1) are arithmetically correct, and 2) satisfy dimensional
analysis, but could never ensure that results are physically meaningful.
Temperature units are special because there is an absolute unit, Kelvin,
and relative ones, Celsius and Fahrenheit degrees. Arithmetic operations
between them are meaningless from the physical standpoint. Users are
thus advised to convert all temperatures to Kelvin before operating.

## Examples

``` r
a <- set_units(1:3, m/s)
b <- set_units(1:3, m/s)
a + b
#> Units: [m/s]
#> [1] 2 4 6
a * b
#> Units: [m^2/s^2]
#> [1] 1 4 9
a / b
#> Units: [1]
#> [1] 1 1 1
a <- as_units("kg m-3")
b <- set_units(1, kg/m/m/m)
a + b
#> 2 [kg/m^3]
a = set_units(1:5, m)
a %/% a
#> Units: [1]
#> [1] 1 1 1 1 1
a %/% set_units(2)
#> Units: [m]
#> [1] 0 1 1 2 2
set_units(1:5, m^2) %/% set_units(2, m)
#> Units: [m]
#> [1] 0 1 1 2 2
a %% a
#> Units: [m]
#> [1] 0 0 0 0 0
a %% set_units(2)
#> Units: [m]
#> [1] 1 0 1 0 1
```
