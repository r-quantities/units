# Units of Measurement for R Vectors: an Introduction

R has little support for physical measurement units. The exception is
formed by time differences: time differences objects of class `difftime`
have a `units` attribute that can be modified:

``` r
t1 = Sys.time() 
t2 = t1 + 3600 
d = t2 - t1
class(d)
## [1] "difftime"
units(d)
## [1] "hours"
d
## Time difference of 1 hours
units(d) = "secs"
d
## Time difference of 3600 secs
```

We see here that the `units` method is used to retrieve and modify the
unit of time differences.

The `units` package generalizes this idea to other physical units,
building upon the
[udunits2](https://www.unidata.ucar.edu/software/udunits) C library. The
`udunits2` library provides the following operations:

- validating whether an expression, such as `m/s` is a valid physical
  unit
- verifying whether two units such as `m/s` and `km/h` are convertible
- converting values between two convertible units
- providing names and symbols for specific units
- handle different character encodings (utf8, ascii, iso-8859-1 and
  latin1)

The `units` R package uses the
[udunits2](https://www.unidata.ucar.edu/software/udunits) C library to
extend R with functionality for manipulating numeric vectors that have
physical measurement units associated with them, in a similar way as
`difftime` objects behave.

## Setting units, unit conversion

We can set units to numerical values by `set_units`:

``` r
library(units)
## udunits database from /usr/share/xml/udunits/udunits2.xml
(a <- set_units(runif(10),  m/s))
## Units: [m/s]
##  [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441 0.466393497
##  [7] 0.497777389 0.289767245 0.732881987 0.772521511
```

the result, e.g. 

``` r
set_units(10, m/s)
## 10 [m/s]
```

literally means “10 times 1 m divided by 1 s”. In writing, the “1”
values are omitted, and the multiplication is implicit.

### Unit conversion

When conversion is meaningful, such as hours to seconds or meters to
kilometers, conversion can be done explicitly by setting the units of a
vector

``` r
b = a
units(b) <- make_units(km/h)
b
## Units: [km/h]
##  [1] 0.29070050 3.00359893 2.16273919 0.56595039 0.02663799 1.67901659
##  [7] 1.79199860 1.04316208 2.63837515 2.78107744
```

## Basic manipulations

### Arithmetic operations

Arithmetic operations verify units, and create new ones

``` r
a + a
## Units: [m/s]
##  [1] 0.16150028 1.66866607 1.20152177 0.31441688 0.01479888 0.93278699
##  [7] 0.99555478 0.57953449 1.46576397 1.54504302
a * a
## Units: [m^2/s^2]
##  [1] 6.520585e-03 6.961116e-01 3.609136e-01 2.471449e-02 5.475173e-05
##  [6] 2.175229e-01 2.477823e-01 8.396506e-02 5.371160e-01 5.967895e-01
a ^ 2
## Units: [m^2/s^2]
##  [1] 6.520585e-03 6.961116e-01 3.609136e-01 2.471449e-02 5.475173e-05
##  [6] 2.175229e-01 2.477823e-01 8.396506e-02 5.371160e-01 5.967895e-01
a ** -2
## Units: [s^2/m^2]
##  [1]   153.360480     1.436551     2.770746    40.462087 18264.262998
##  [6]     4.597217     4.035800    11.909716     1.861795     1.675633
```

and convert to the units of the first argument if necessary:

``` r
a + b # m/s + km/h -> m/s
## Units: [m/s]
##  [1] 0.16150028 1.66866607 1.20152177 0.31441688 0.01479888 0.93278699
##  [7] 0.99555478 0.57953449 1.46576397 1.54504302
```

Currently, powers are only supported for integer powers, so using
`a ** 2.5` would result in an error.

### Unit simplification

There are some basic simplification of units:

``` r
t <- make_units(s)
a * t
## Units: [m]
##  [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441 0.466393497
##  [7] 0.497777389 0.289767245 0.732881987 0.772521511
```

which also work when units need to be converted before they can be
simplified:

``` r
t <- make_units(min)
a * t
## Units: [m]
##  [1]  4.8450083 50.0599822 36.0456532  9.4325065  0.4439665 27.9836098
##  [7] 29.8666433 17.3860347 43.9729192 46.3512907
```

Simplification to unit-less values gives the “1” as unit:

``` r
m <- make_units(m)
a * t / m
## Units: [1]
##  [1]  4.8450083 50.0599822 36.0456532  9.4325065  0.4439665 27.9836098
##  [7] 29.8666433 17.3860347 43.9729192 46.3512907
```

Allowed operations that require convertible units are `+`, `-`, `==`,
`!=`, `<`, `>`, `<=`, `>=`. Operations that lead to new units are `*`,
`/`, and the power operations `**` and `^`.

### Mathematical functions

Mathematical operations allowed are: `abs`, `sign`, `floor`, `ceiling`,
`trunc`, `round`, `signif`, `log`, `cumsum`, `cummax`, `cummin`.

``` r
signif(a ** 2 / 3, 3)
## Units: [m^2/s^2]
##  [1] 2.17e-03 2.32e-01 1.20e-01 8.24e-03 1.83e-05 7.25e-02 8.26e-02 2.80e-02
##  [9] 1.79e-01 1.99e-01
cumsum(a)
## Units: [m/s]
##  [1] 0.08075014 0.91508317 1.51584406 1.67305250 1.68045194 2.14684544
##  [7] 2.64462283 2.93439007 3.66727206 4.43979357
log(a) # base defaults to exp(1)
## Units: [ln(re 1 m.s-1)]
##  [1] -2.5163956 -0.1811226 -0.5095583 -1.8501827 -4.9063508 -0.7627256
##  [7] -0.6976023 -1.2386773 -0.3107706 -0.2580954
log(a, base = 10)
## Units: [lg(re 1 m.s-1)]
##  [1] -1.09285673 -0.07866056 -0.22129835 -0.80352414 -2.13080108 -0.33124751
##  [7] -0.30296483 -0.53795071 -0.13496595 -0.11208942
log(a, base = 2)
## Units: [lb(re 1 m.s-1)]
##  [1] -3.6303915 -0.2613047 -0.7351372 -2.6692494 -7.0783680 -1.1003804
##  [7] -1.0064274 -1.7870336 -0.4483472 -0.3723530
```

### Summary functions

Summary functions `sum`, `min`, `max`, and `range` are allowed:

``` r
sum(a)
## 4.439794 [m/s]
min(a)
## 0.007399441 [m/s]
max(a)
## 0.834333 [m/s]
range(a)
## Units: [m/s]
## [1] 0.007399441 0.834333037
```

### Printing

Following `difftime`, printing behaves differently for length-one
vectors:

``` r
a
## Units: [m/s]
##  [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441 0.466393497
##  [7] 0.497777389 0.289767245 0.732881987 0.772521511
a[1]
## 0.08075014 [m/s]
```

### Subsetting

The usual subsetting rules work:

``` r
a[2:5]
## Units: [m/s]
## [1] 0.834333037 0.600760886 0.157208442 0.007399441
a[-(1:9)]
## 0.7725215 [m/s]
```

### Concatenation

``` r
c(a,a)
## Units: [m/s]
##  [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441 0.466393497
##  [7] 0.497777389 0.289767245 0.732881987 0.772521511 0.080750138 0.834333037
## [13] 0.600760886 0.157208442 0.007399441 0.466393497 0.497777389 0.289767245
## [19] 0.732881987 0.772521511
```

concatenation converts to the units of the first argument, if necessary:

``` r
c(a,b) # m/s, km/h -> m/s
## Units: [m/s]
##  [1] 0.080750138 0.834333037 0.600760886 0.157208442 0.007399441 0.466393497
##  [7] 0.497777389 0.289767245 0.732881987 0.772521511 0.080750138 0.834333037
## [13] 0.600760886 0.157208442 0.007399441 0.466393497 0.497777389 0.289767245
## [19] 0.732881987 0.772521511
c(b,a) # km/h, m/s -> km/h
## Units: [km/h]
##  [1] 0.29070050 3.00359893 2.16273919 0.56595039 0.02663799 1.67901659
##  [7] 1.79199860 1.04316208 2.63837515 2.78107744 0.29070050 3.00359893
## [13] 2.16273919 0.56595039 0.02663799 1.67901659 1.79199860 1.04316208
## [19] 2.63837515 2.78107744
```

## Conversion to/from `difftime`

From `difftime` to `units`:

``` r
t1 = Sys.time() 
t2 = t1 + 3600 
d = t2 - t1
(du = as_units(d))
## 1 [h]
```

vice versa:

``` r
(dt = as_difftime(du))
## Time difference of 1 hours
class(dt)
## [1] "difftime"
```

## units in `matrix` objects

``` r
set_units(matrix(1:4,2,2), m/s)
## Units: [m/s]
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
set_units(matrix(1:4,2,2), m/s * m/s)
## Units: [m^2/s^2]
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
```

but

``` r
set_units(matrix(1:4,2,2), m/s) %*% set_units(4:3, m/s)
## Units: [m^2/s^2]
##      [,1]
## [1,]   13
## [2,]   20
```

strips units.

## units objects in `data.frame`s

units in `data.frame` objects are printed, but do not appear in
`summary`:.

``` r
set.seed(131)
d <- data.frame(x = runif(4), 
                    y = set_units(runif(4), s), 
                    z = set_units(1:4, m/s))
d
##           x             y       z
## 1 0.2064370 0.8463468 [s] 1 [m/s]
## 2 0.1249422 0.5292048 [s] 2 [m/s]
## 3 0.2932732 0.5186254 [s] 3 [m/s]
## 4 0.3757797 0.2378545 [s] 4 [m/s]
summary(d)
##        x                y                z       
##  Min.   :0.1249   Min.   :0.2379   Min.   :1.00  
##  1st Qu.:0.1861   1st Qu.:0.4484   1st Qu.:1.75  
##  Median :0.2499   Median :0.5239   Median :2.50  
##  Mean   :0.2501   Mean   :0.5330   Mean   :2.50  
##  3rd Qu.:0.3139   3rd Qu.:0.6085   3rd Qu.:3.25  
##  Max.   :0.3758   Max.   :0.8463   Max.   :4.00
d$yz = with(d, y * z)
d
##           x             y       z            yz
## 1 0.2064370 0.8463468 [s] 1 [m/s] 0.8463468 [m]
## 2 0.1249422 0.5292048 [s] 2 [m/s] 1.0584095 [m]
## 3 0.2932732 0.5186254 [s] 3 [m/s] 1.5558761 [m]
## 4 0.3757797 0.2378545 [s] 4 [m/s] 0.9514180 [m]
d[1, "yz"]
## 0.8463468 [m]
```

## Formatting

Units are often written in the form `m2 s-1`, for square meter per
second. This can be defined as unit, and also parsed by `as_units`:

``` r
(x = 1:10 * as_units("m2 s-1"))
## Units: [m^2/s]
##  [1]  1  2  3  4  5  6  7  8  9 10
```

udunits understands such string, and can convert them

``` r
y = 1:10 * make_units(m^2/s)
x + y
## Units: [m^2/s]
##  [1]  2  4  6  8 10 12 14 16 18 20
```

Printing units in this form is done by

``` r
deparse_unit(x)
## [1] "m2 s-1"
```

## Plotting

Base scatter plots and histograms support automatic unit placement in
axis labels. In the following example we first convert to SI units.
(Unit `in` needs a bit special treatment, because `in` is a reserved
word in R.)

``` r
mar = par("mar") + c(0, .3, 0, 0)
displacement = mtcars$disp * as_units("in")^3
units(displacement) = make_units(cm^3)
weight = mtcars$wt * 1000 * make_units(lb)
units(weight) = make_units(kg)
par(mar = mar)
plot(weight, displacement)
```

![](units_files/figure-html/unnamed-chunk-26-1.png)

We can change grouping symbols from `[ ]` into `( )`:

``` r
units_options(group = c("(", ")") )  # parenthesis instead of square brackets
par(mar = mar)
plot(weight, displacement)
```

![](units_files/figure-html/unnamed-chunk-27-1.png)

We can also remove grouping symbols, increase space between variable
name and unit by:

``` r
units_options(sep = c("~~~", "~"), group = c("", ""))  # no brackets; extra space
par(mar = mar)
plot(weight, displacement)
```

![](units_files/figure-html/unnamed-chunk-28-1.png)

More complex units can be plotted either with negative powers, or as
divisions, by modifying one of `units`’s global options using
`units_options`:

``` r
gallon = as_units("gallon")
consumption = mtcars$mpg * make_units(mi/gallon)
units(consumption) = make_units(km/l)
par(mar = mar)
plot(displacement, consumption) # division in consumption
```

![](units_files/figure-html/unnamed-chunk-29-1.png)

``` r
units_options(negative_power = TRUE) # division becomes ^-1
plot(displacement, consumption) # division in consumption
```

![](units_files/figure-html/unnamed-chunk-29-2.png)

As usual, units modify automatically in expressions:

``` r
units_options(negative_power = TRUE) # division becomes ^-1
par(mar = mar)
plot(displacement, consumption)
```

![](units_files/figure-html/unnamed-chunk-30-1.png)

``` r
plot(1/displacement, 1/consumption)
```

![](units_files/figure-html/unnamed-chunk-30-2.png)
