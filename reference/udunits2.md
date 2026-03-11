# udunits2 utilities

Some udunits2 utilities are exposed to the user. These functions are
useful for checking whether units are convertible or converting between
units without having to create units objects. Arguments are recycled if
necessary.

## Usage

``` r
ud_are_convertible(from, to, ...)

ud_convert(x, from, to)
```

## Arguments

- from, to:

  character vector or object of class `symbolic_units`, for the
  symbol(s) of the original unit(s) and the unit to convert to
  respectively.

- ...:

  unused.

- x:

  numeric vector

## Value

`ud_are_convertible` returns `TRUE` if both units exist and are
convertible, `FALSE` otherwise.

`ud_convert` returns a numeric vector with `x` converted to new unit.

## Examples

``` r
ud_are_convertible(c("m", "mm"), "km")
#> [1] TRUE TRUE
ud_convert(c(100, 100000), c("m", "mm"), "km")
#> [1] 0.1 0.1

a <- set_units(1:3, m/s)
ud_are_convertible(units(a), "km/h")
#> [1] TRUE
ud_convert(1:3, units(a), "km/h")
#> [1]  3.6  7.2 10.8

ud_are_convertible("degF", "degC")
#> [1] TRUE
ud_convert(32, "degF", "degC")
#> [1] 3.552714e-14
```
