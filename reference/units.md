# Handle measurement units

A number of functions are provided for handling unit objects.

- `` `units<-` `` and `units` are the basic functions to set and
  retrieve units.

- `as_units`, a generic with methods for a character string and for
  quoted language. Note, direct usage of this function by users is
  typically not necessary, as coercion via `as_units` is automatically
  done with `` `units<-` `` and `set_units`.

- `make_units`, constructs units from bare expressions.
  `make_units(m/s)` is equivalent to `as_units(quote(m/s))`.

- `set_units`, a pipe-friendly version of `` `units<-` ``. By default it
  operates with bare expressions, but this behavior can be disabled by a
  specifying `mode = "standard"` or setting
  `units_options(set_units_mode = "standard")`. If `value` is missing or
  set to `1`, the object becomes unitless.

## Usage

``` r
# S3 method for class 'numeric'
units(x) <- value

# S3 method for class 'units'
units(x) <- value

# S3 method for class 'logical'
units(x) <- value

# S3 method for class 'units'
units(x)

# S3 method for class 'symbolic_units'
units(x)

set_units(x, value, ..., mode = units_options("set_units_mode"))

make_units(bare_expression, check_is_valid = TRUE)

as_units(x, ...)

# Default S3 method
as_units(x, value = unitless, ...)

# S3 method for class 'units'
as_units(x, value, ...)

# S3 method for class 'symbolic_units'
as_units(x, value, ...)

# S3 method for class 'difftime'
as_units(x, value, ...)

# S3 method for class 'character'
as_units(x, ..., check_is_valid = TRUE,
  force_single_symbol = FALSE)

# S3 method for class 'call'
as_units(x, ...)

# S3 method for class 'expression'
as_units(x, ...)

# S3 method for class 'name'
as_units(x, ...)

# S3 method for class 'POSIXt'
as_units(x, value, ...)

# S3 method for class 'Date'
as_units(x, value, ...)
```

## Arguments

- x:

  numeric vector, or object of class `units`.

- value:

  object of class `units` or `symbolic_units`, or in the case of
  `set_units` expression with symbols (see examples).

- ...:

  passed on to other methods.

- mode:

  if `"symbols"` (the default), then unit is constructed from the
  expression supplied. Otherwise, if`mode = "standard"`, standard
  evaluation is used for the supplied value This argument can be set via
  a global option `units_options(set_units_mode = "standard")`

- bare_expression:

  a bare R expression describing units. Must be valid R syntax (reserved
  R syntax words like `in` must be backticked)

- check_is_valid:

  throw an error if all the unit symbols are not either recognized by
  udunits2, or a custom user defined via
  [`install_unit()`](install_unit.md). If `FALSE`, no check for validity
  is performed.

- force_single_symbol:

  Whether to perform no string parsing and force treatment of the string
  as a single symbol.

## Value

An object of class `units`.

The `units` method retrieves the units attribute, which is of class
`symbolic_units`.

## Details

If `value` is of class `units` and has a value unequal to 1, this value
is ignored unless `units_options("simplifiy")` is `TRUE`. If `simplify`
is `TRUE`, `x` is multiplied by this value.

## Note

By default, unit names are automatically substituted with unit names
(e.g., kilogram –\> kg). To turn off this behavior, set
`units_options(auto_convert_names_to_symbols = FALSE)`

## Character strings

Generally speaking, there are 3 types of unit strings are accepted in
`as_units` (and by extension, `` `units<-` ``).

The first type, and likely most common, is a "standard" format unit
specification where the relationship between unit symbols or names is
specified explicitly with arithmetic symbols for division `/`,
multiplication `*` and power exponents `^`.

The second type of unit string accepted is one with implicit exponents.
In this format, `/`, `*`, and `^`, may not be present in the string, and
unit symbol or names must be separated by a space. Each unit symbol may
optionally be followed by a single number, specifying the power. For
example `"m2 s-2"` is equivalent to `"(m^2)*(s^-2)"`.

If the string supplied fails to parse, then the string is treated as a
single symbolic unit and `symbolic_unit(chr)` is used as a fallback with
a warning. In that case, automatic unit simplification may not work
properly when performing operations on unit objects, but unit conversion
and other Math operations should still give correct results so long as
the unit string supplied returns `TRUE` for `ud_is_parsable()`.

It must be noted that prepended numbers are supported too, but are not
treated as magnitudes. For example, `"1000 m"` is interpreted as a
prefixed unit, and it is equivalent to `"km"` to all effects.

The third type of unit string format accepted is the special case of
udunits time duration with a reference origin, for example
`"hours since 1970-01-01 00:00:00"`. Note, that the handling of time and
calendar operations via the udunits library is subtly different from the
way R handles date and time operations. This functionality is mostly
exported for users that work with udunits time data, e.g., with NetCDF
files. Users are otherwise encouraged to use `R`'s date and time
functionality provided by `Date` and `POSIXt` classes.

## See also

[`install_unit`](install_unit.md), [`valid_udunits`](valid_udunits.md)

## Examples

``` r
x = 1:3
class(x)
#> [1] "integer"
units(x) <- as_units("m/s")
class(x)
#> [1] "units"
y = 2:5
a <- set_units(1:3, m/s)
units(a) <- make_units(km/h)
a
#> Units: [km*h^-1]
#> [1]  3.6  7.2 10.8
# convert to a mixed_units object:
units(a) <- c("m/s", "km/h", "km/h")
a
#> Mixed units: m*s^-1 (1), km*h^-1 (2) 
#> 1 [m*s^-1], 7.2 [km*h^-1], 10.8 [km*h^-1] 
# The easiest way to assign units to a numeric vector is like this:
x <- y <- 1:4
units(x) <- "m/s"  # meters / second

# Alternatively, the easiest pipe-friendly way to set units:
if(requireNamespace("magrittr", quietly = TRUE)) {
  library(magrittr)
  y %>% set_units(m/s)
}
#> Units: [m*s^-1]
#> [1] 1 2 3 4

# these are different ways of creating the same unit:
# meters per second squared, i.e, acceleration
x1 <- make_units(m/s^2)
x2 <- as_units(quote(m/s^2))
x2 <- as_units("m/s^2")
x3 <- as_units("m s-2") # in product power form, i.e., implicit exponents = T
x4 <- set_units(1,  m/s^2) # by default, mode = "symbols"
x5 <- set_units(1, "m/s^2",   mode = "standard")
x6 <- set_units(1, x1,        mode = "standard")
x7 <- set_units(1, units(x1), mode = "standard")
x8 <- as_units("m") / as_units("s")^2

all_identical <- function(...) {
  l <- list(...)
  for(i in seq_along(l)[-1])
    if(!identical(l[[1]], l[[i]]))
      return(FALSE)
  TRUE
}
all_identical(x1, x2, x3, x4, x5, x6, x7, x8)
#> [1] TRUE

# Note, direct usage of these unit creation functions is typically not
# necessary, since coercion is automatically done via as_units(). Again,
# these are all equivalent ways to generate the same result.

x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- x8 <- 1:4
units(x1) <- "m/s^2"
units(x2) <- "m s-2"
units(x3) <- quote(m/s^2)
units(x4) <- make_units(m/s^2)
units(x5) <- as_units(quote(m/s^2))
x6 <- set_units(x6, m/s^2)
x7 <- set_units(x7, "m/s^2", mode = "standard")
x8 <- set_units(x8, units(x1), mode = "standard")

all_identical(x1, x2, x3, x4, x5, x6, x7, x8)
#> [1] TRUE


# Both unit names or symbols can be used. By default, unit names are
# automatically converted to unit symbols.
make_units(degree_C)
#> 1 [°C]
make_units(kilogram)
#> 1 [kg]
make_units(ohm)
#> 1 [Ω]

## Arithmetic operations and units
# conversion between unit objects that were defined as symbols and names will
# work correctly, although unit simplification in printing may not always occur.
x <- 500 * make_units(micrograms/liter)
y <- set_units(200, ug/l)
x + y
#> 700 [micrograms*L^-1]
x * y # numeric result is correct, but units not simplified completely
#> 1e+05 [micrograms*ug*L^-2]

# note, plural form of unit name accepted too ('liters' vs 'liter'), and
# denominator simplification can be performed correctly
x * set_units(5, liters)
#> 2500 [micrograms]

# unit conversion works too
set_units(x, grams/gallon)
#> 0.001892706 [g*gallon^-1]

## Creating custom, user defined units
# For example, a microbiologist might work with counts of bacterial cells
# make_units(cells/ml) # by default, throws an ERROR
# First define the unit, then the newly defined unit is accepted.
install_unit("cells")
make_units(cells/ml)
#> 1 [cells*ml^-1]

# Note that install_unit() adds support for defining relationships between
# the newly created symbols or names and existing units.

## set_units()
# set_units is a pipe friendly version of `units<-`.
if(requireNamespace("magrittr", quietly = TRUE)) {
  library(magrittr)
  1:5 %>% set_units(N/m^2)
  # first sets to m, then converts to km
  1:5 %>% set_units(m) %>% set_units(km)
}
#> Units: [km]
#> [1] 0.001 0.002 0.003 0.004 0.005

# set_units has two modes of operation. By default, it operates with
# bare symbols to define the units.
set_units(1:5, m/s)
#> Units: [m*s^-1]
#> [1] 1 2 3 4 5

# use `mode = "standard"` to use the value of supplied argument, rather than
# the bare symbols of the expression. In this mode, set_units() can be
# thought of as a simple alias for `units<-` that is pipe friendly.
set_units(1:5, "m/s", mode = "standard")
#> Units: [m*s^-1]
#> [1] 1 2 3 4 5
set_units(1:5, make_units(m/s), mode = "standard")
#> Units: [m*s^-1]
#> [1] 1 2 3 4 5

# the mode of set_units() can be controlled via a global option
# units_options(set_units_mode = "standard")

# To remove units use
units(x) <- NULL
# or
set_units(x, NULL)
#> [1] 500
# or
drop_units(y)
#> [1] 200
s = Sys.time()
d  = s - (s+1)
as_units(d)
#> -1 [s]
```
