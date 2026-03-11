# histogram for unit objects

histogram for unit objects

## Usage

``` r
# S3 method for class 'units'
hist(x, xlab = NULL, main = paste("Histogram of", xname),
  ...)
```

## Arguments

- x:

  object of class units, for which we want to plot the histogram

- xlab:

  character; x axis label

- main:

  character; title of histogram

- ...:

  parameters passed on to
  [hist.default](https://rdrr.io/r/graphics/hist.html)

## Examples

``` r
units_options(parse = FALSE) # otherwise we break on the funny symbol!
u = set_units(rnorm(100), degree_C)
hist(u)
```
