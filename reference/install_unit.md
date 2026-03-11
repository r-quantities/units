# Define or remove units

Installing new symbols and/or names allows them to be used in
`as_units`, `make_units` and `set_units`. Optionally, a relationship can
be defined between such symbols/names and existing ones (see details and
examples).

## Usage

``` r
install_unit(symbol = character(0), def = character(0),
  name = character(0))

remove_unit(symbol = character(0), name = character(0))
```

## Arguments

- symbol:

  a vector of symbols to be installed/removed.

- def:

  either

  - an empty definition, which defines a new base unit;

  - `"unitless"`, which defines a new dimensionless unit;

  - a relationship with existing units (see details for the syntax).

- name:

  a vector of names to be installed/removed.

## Details

At least one symbol or name is expected, but multiple symbols and/or
names can be installed (and thus mapped to the same unit) or removed at
the same time. The `def` argument enables arbitrary relationships with
existing units using UDUNITS-2 syntax:

|                 |                       |                   |                                                     |
|-----------------|-----------------------|-------------------|-----------------------------------------------------|
| **String Type** | **Using Names**       | **Using Symbols** | **Comment**                                         |
| Simple          | meter                 | m                 |                                                     |
| Raised          | meter^2               | m2                | higher precedence than multiplying or dividing      |
| Product         | newton meter          | N.m               |                                                     |
| Quotient        | meter per second      | m/s               |                                                     |
| Scaled          | 60 second             | 60 s              |                                                     |
| Prefixed        | kilometer             | km                |                                                     |
| Offset          | kelvin from 273.15    | K @ 273.15        | lower precedence than multiplying or dividing       |
| Logarithmic     | lg(re milliwatt)      | lg(re mW)         | "lg" is base 10, "ln" is base e, and "lb" is base 2 |
| Grouped         | (5 meter)/(30 second) | (5 m)/(30 s)      |                                                     |

The above may be combined, e.g., `"0.1 lg(re m/(5 s)^2) @ 50"`. You may
also look at the `<def>` elements in the units database to see examples
of string unit specifications.

## Examples

``` r
# define a fortnight
install_unit("fn", "2 week", "fortnight")
year <- as_units("year")
set_units(year, fn)        # by symbol
#> 26.08873 [fn]
set_units(year, fortnight) # by name
#> 26.08873 [fn]
# clean up
remove_unit("fn", "fortnight")

# working with currencies
install_unit("dollar")
install_unit("euro", "1.22 dollar")
install_unit("yen", "0.0079 euro")
set_units(as_units("dollar"), yen)
#> 103.756 [yen]
# clean up
remove_unit(c("dollar", "euro", "yen"))

# an example from microbiology
cfu_symbols <- c("CFU", "cfu")
cfu_names <- c("colony_forming_unit", "ColonyFormingUnit")
install_unit("cell")
install_unit(cfu_symbols, "3.4 cell", cfu_names)
cell <- set_units(2.5e5, cell)
vol <- set_units(500, ul)
set_units(cell/vol, "cfu/ml")
#> 147058.8 [CFU/ml]
set_units(cell/vol, "CFU/ml")
#> 147058.8 [CFU/ml]
set_units(cell/vol, "colony_forming_unit/ml")
#> 147058.8 [CFU/ml]
set_units(cell/vol, "ColonyFormingUnit/ml")
#> 147058.8 [CFU/ml]
# clean up
remove_unit(c("cell", cfu_symbols), cfu_names)
```
