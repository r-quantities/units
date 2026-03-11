# Apply a function keeping units

Helper function to apply a function to a `units` object and then restore
the original units.

## Usage

``` r
keep_units(FUN, x, ..., unit = units(x))
```

## Arguments

- FUN:

  the function to be applied.

- x:

  first argument of `FUN`, of class `units`.

- ...:

  optional arguments to `FUN`.

- unit:

  symbolic unit to restore after `FUN`.

## Value

An object of class `units`.

## Details

Provided for incompatible functions that do not preserve units. The user
is responsible for ensuring the correctness of the output.

If `x` is not a `units` object and `unit` is not provided by the user, a
warning is issued, and the output will also have no units (see
examples).

## Examples

``` r
x <- set_units(1:5, m)
keep_units(drop_units, x)
#> Units: [m]
#> [1] 1 2 3 4 5

# An example use case is with random number generating functions:
mu <- as_units(10, "years")
keep_units(rnorm, n = 1, x = mu)
#> 9.57062 [years]

# units can be directly specified if needed; for example, with
# `rexp()`, the units of the rate parameter are the inverse of
# the units of the output:
rate <- as_units(3, "1/year")
keep_units(rexp, n = 1, x = rate, unit = units(1/rate))
#> 0.04721004 [year]

# if `x` does not actually have units, a warning is issued,
# and the output has no units:
rate2 <- 3
keep_units(rexp, n = 1, x = rate2)
#> Warning: wrong `unit` specification.
#> [1] 0.4112932
```
