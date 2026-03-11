# Create or convert to a mixed units list-column

Create or convert to a mixed units list-column

## Usage

``` r
mixed_units(x, values, ...)

# S3 method for class 'mixed_units'
units(x) <- value
```

## Arguments

- x:

  numeric, or vector of class `units`

- values:

  character vector with units encodings, or list with symbolic units of
  class `mixed_symbolic_units`

- ...:

  ignored

- value:

  see values

## Details

if `x` is of class `units`, `values` should be missing or of class
`mixed_symbolic_units`; if `x` is numeric, `values` should be a
character vector the length of `x`.

## Examples

``` r
a <- 1:4
u <- c("m/s", "km/h", "mg/L", "g")
mixed_units(a, u)
#> Mixed units: m/s (1), km/h (1), mg/L (1), g (1) 
#> 1 [m/s], 2 [km/h], 3 [mg/L], 4 [g] 
units(a) = as_units("m/s")
mixed_units(a) # converts to mixed representation
#> Mixed units: m/s (4) 
#> 1 [m/s], 2 [m/s], 3 [m/s], 4 [m/s] 
```
