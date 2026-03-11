# Convert units to their base units

Convert the units of a `units` object to their base units, as defined by
the udunits database (SI units).

## Usage

``` r
convert_to_base(x, simplify = TRUE, keep_fraction = TRUE)
```

## Arguments

- x:

  object of class `units`.

- simplify:

  logical; if TRUE (default), the resulting units are simplified.

- keep_fraction:

  logical; if TRUE (default), the result is kept as a fraction.

## Value

object of class `units` with units converted to base units.

## Examples

``` r
x <- set_units(32, mJ/g)
convert_to_base(x)
#> 32 [J/kg]
convert_to_base(x, keep_fraction=FALSE)
#> 32 [Gy]
convert_to_base(x, simplify=FALSE)
#> 32 [m^2*kg/(s^2*kg)]
convert_to_base(x, simplify=FALSE, keep_fraction=FALSE)
#> 32 [m^2/s^2]
```
