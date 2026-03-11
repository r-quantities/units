# boxplot for unit objects

boxplot for unit objects

## Usage

``` r
# S3 method for class 'units'
boxplot(x, ..., horizontal = FALSE)
```

## Arguments

- x:

  object of class units, for which we want to plot the boxplot

- ...:

  parameters passed on to
  [boxplot.default](https://rdrr.io/r/graphics/boxplot.html)

- horizontal:

  logical indicating if the boxplots should be horizontal; default FALSE
  means vertical boxes.

## Examples

``` r
units_options(parse = FALSE) # otherwise we break on the funny symbol!
u = set_units(rnorm(100), degree_C)
boxplot(u)
```
