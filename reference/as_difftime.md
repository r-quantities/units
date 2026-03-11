# convert units object into difftime object

convert units object into difftime object

## Usage

``` r
as_difftime(x)
```

## Arguments

- x:

  object of class `units`

## Examples

``` r
t1 = Sys.time()
t2 = t1 + 3600
d = t2 - t1
du <- as_units(d)
dt = as_difftime(du)
class(dt)
#> [1] "difftime"
dt
#> Time difference of 1 hours
```
