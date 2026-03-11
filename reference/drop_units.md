# Drop Units

Drop units attribute and class.

## Usage

``` r
drop_units(x)

# S3 method for class 'units'
drop_units(x)

# S3 method for class 'data.frame'
drop_units(x)

# S3 method for class 'mixed_units'
drop_units(x)
```

## Arguments

- x:

  an object with units metadata.

## Value

the numeric without any units attributes, while preserving other
attributes like dimensions or other classes.

## Details

Equivalent to `units(x) <- NULL`, or the pipe-friendly version
`set_units(x, NULL)`, but `drop_units` will fail if the object has no
units metadata. Use the alternatives if you want this operation to
succeed regardless of the object type.

A `data.frame` method is also provided, which checks every column and
drops units if any.

## Examples

``` r
x <- 1
y <- set_units(x, m/s)

# this succeeds
drop_units(y)
#> [1] 1
set_units(y, NULL)
#> [1] 1
set_units(x, NULL)
#> [1] 1

if (FALSE) { # \dontrun{
# this fails
drop_units(x)
} # }

df <- data.frame(x=x, y=y)
df
#>   x       y
#> 1 1 1 [m/s]
drop_units(df)
#>   x y
#> 1 1 1
```
