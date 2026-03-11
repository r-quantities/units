# seq method for units objects

seq method for units objects

## Usage

``` r
# S3 method for class 'units'
seq(from, to, by = ((to - from)/(length.out - 1)),
  length.out = NULL, along.with = NULL, ...)
```

## Arguments

- from:

  see [seq](https://rdrr.io/r/base/seq.html)

- to:

  see [seq](https://rdrr.io/r/base/seq.html)

- by:

  see [seq](https://rdrr.io/r/base/seq.html)

- length.out:

  see [seq](https://rdrr.io/r/base/seq.html)

- along.with:

  see [seq](https://rdrr.io/r/base/seq.html)

- ...:

  see [seq](https://rdrr.io/r/base/seq.html)

## Details

arguments with units are converted to have units of the first argument
(which is either `from` or `to`)

## Examples

``` r
seq(to = set_units(10, m), by = set_units(1, m), length.out = 5)
#> Units: [m]
#> [1]  6  7  8  9 10
seq(set_units(10, m), by = set_units(1, m), length.out = 5)
#> Units: [m]
#> [1] 10 11 12 13 14
seq(set_units(10, m), set_units(19, m))
#> Units: [m]
#>  [1] 10 11 12 13 14 15 16 17 18 19
seq(set_units(10, m), set_units(.1, km), set_units(10000, mm))
#> Units: [m]
#>  [1]  10  20  30  40  50  60  70  80  90 100
```
