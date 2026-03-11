# deparse unit to string in product power form (e.g. km m-2 s-1)

deparse unit to string in product power form (e.g. km m-2 s-1)

## Usage

``` r
deparse_unit(x)
```

## Arguments

- x:

  object of class units

## Value

length one character vector

## Examples

``` r
u = as_units("kg m-2 s-1")
u
#> 1 [kg/(m^2*s)]
deparse_unit(u)
#> [1] "kg m-2 s-1"
```
