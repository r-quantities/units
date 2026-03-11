# Mathematical operations for units objects

Mathematical operations for units objects

## Usage

``` r
# S3 method for class 'units'
Math(x, ...)
```

## Arguments

- x:

  object of class units

- ...:

  parameters passed on to the Math functions

## Details

Logarithms receive a special treatment by the underlying udunits2
library. If a natural logarithm is applied to some `unit`, the result is
`ln(re 1 unit)`, which means *natural logarithm referenced to `1 unit`*.
For base 2 and base 10 logarithms, the output `lb(...)` and `lg(...)`
respectively instead of `ln(...)`.

This is particularly important for some units that are typically
expressed in a logarithmic scale (i.e., *bels*, or, more commonly,
*decibels*), such as Watts or Volts. For some of these units, the
default udunits2 database contains aliases: e.g., `BW` (bel-Watts) is an
alias of `lg(re 1 W)`; `Bm` (bel-milliWatts) is an alias of
`lg(re 0.001 W)`; `BV` is an alias of `lg(re 1 V)` (bel-Volts), and so
on and so forth (see the output of [`valid_udunits()`](valid_udunits.md)
for further reference).

Additionally, the units package defines `B`, the *bel*, by default
(because it is not defined by udunits2) as an alias of `lg(re 1)`,
unless a user-provided XML database already contains a definition of
`B`, or the `define_bel` option is set to `FALSE` (see
[`help(units_options)`](units_options.md)).

## Examples

``` r
# roundings, cummulative functions
x <- set_units(sqrt(1:10), m/s)
signif(x, 2)
#> Units: [m/s]
#>  [1] 1.0 1.4 1.7 2.0 2.2 2.4 2.6 2.8 3.0 3.2
cumsum(x)
#> Units: [m/s]
#>  [1]  1.000000  2.414214  4.146264  6.146264  8.382332 10.831822 13.477573
#>  [8] 16.306001 19.306001 22.468278

# trigonometry
sin(x) # not meaningful
#> Warning: Operation sin not meaningful for units
#>  [1]  0.84147098  0.98776595  0.98702664  0.90929743  0.78674913  0.63815764
#>  [7]  0.47577184  0.30807174  0.14112001 -0.02068353
x <- set_units(sqrt(1:10), rad)
sin(x)
#> Units: [1]
#>  [1]  0.84147098  0.98776595  0.98702664  0.90929743  0.78674913  0.63815764
#>  [7]  0.47577184  0.30807174  0.14112001 -0.02068353
cos(x)
#> Units: [1]
#>  [1]  0.5403023  0.1559437 -0.1605565 -0.4161468 -0.6172729 -0.7699057
#>  [7] -0.8795687 -0.9513631 -0.9899925 -0.9997861
x <- set_units(seq(0, 1, 0.1), 1)
asin(x)
#> Units: [rad]
#>  [1] 0.0000000 0.1001674 0.2013579 0.3046927 0.4115168 0.5235988 0.6435011
#>  [8] 0.7753975 0.9272952 1.1197695 1.5707963
acos(x)
#> Units: [rad]
#>  [1] 1.5707963 1.4706289 1.3694384 1.2661037 1.1592795 1.0471976 0.9272952
#>  [8] 0.7953988 0.6435011 0.4510268 0.0000000

# logarithms
x <- set_units(sqrt(1:10), W)
log(x) # base exp(1)
#> Units: [ln(re 1 W)]
#>  [1] 0.0000000 0.3465736 0.5493061 0.6931472 0.8047190 0.8958797 0.9729551
#>  [8] 1.0397208 1.0986123 1.1512925
log(x, base = 3)
#> Units: [0.910239226626837 ln(re 1 W)]
#>  [1] 0.0000000 0.3154649 0.5000000 0.6309298 0.7324868 0.8154649 0.8856219
#>  [8] 0.9463946 1.0000000 1.0479516
log2(x)
#> Units: [lb(re 1 W)]
#>  [1] 0.0000000 0.5000000 0.7924813 1.0000000 1.1609640 1.2924813 1.4036775
#>  [8] 1.5000000 1.5849625 1.6609640
log10(x)
#> Units: [lg(re 1 W)]
#>  [1] 0.0000000 0.1505150 0.2385606 0.3010300 0.3494850 0.3890756 0.4225490
#>  [8] 0.4515450 0.4771213 0.5000000
set_units(x, dBW) # decibel-watts
#> Units: [dBW]
#>  [1] 0.000000 1.505150 2.385606 3.010300 3.494850 3.890756 4.225490 4.515450
#>  [9] 4.771213 5.000000
set_units(x, dBm) # decibel-milliwatts
#> Units: [dBm]
#>  [1] 30.00000 31.50515 32.38561 33.01030 33.49485 33.89076 34.22549 34.51545
#>  [9] 34.77121 35.00000
```
