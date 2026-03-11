# set one or more units global options

set units global options, mostly related how units are printed and
plotted

## Usage

``` r
units_options(..., sep, group, negative_power, parse, set_units_mode,
  strict_tokenizer, auto_convert_names_to_symbols, simplify, allow_mixed,
  unitless_symbol, define_bel)
```

## Arguments

- ...:

  named options (character) for which the value is queried

- sep:

  character length two; default `c("~", "~")`; space separator between
  variable and units, and space separator between two different units

- group:

  character length two; start and end group, may be two empty strings, a
  parenthesis pair, or square brackets; default: square brackets.

- negative_power:

  logical, default `FALSE`; should denominators have negative power, or
  follow a division symbol?

- parse:

  logical, default `TRUE`; should the units be made into an expression
  (so we get subscripts)? Setting to `FALSE` may be useful if
  [parse](https://rdrr.io/r/base/parse.html) fails, e.g. if the unit
  contains symbols that assume a particular encoding

- set_units_mode:

  character; either `"symbols"` or `"standard"`; see
  [set_units](units.md); default is `"symbols"`

- strict_tokenizer:

  logical, default `FALSE`; non-strict tokenization attaches constants
  to the following symbol.

- auto_convert_names_to_symbols:

  logical, default `TRUE`: should names, such as `degree_C` be converted
  to their usual symbol?

- simplify:

  logical, default `NA`; simplify units in expressions?

- allow_mixed:

  logical; if `TRUE`, combining mixed units creates a `mixed_units`
  object, if `FALSE` it generates an error

- unitless_symbol:

  character; set the symbol to use for unitless (1) units

- define_bel:

  logical; if `TRUE`, define the unit `B` (i.e., the *bel*, widely used
  with the *deci-* prefix as `dB`, *decibel*) as an alias of `lg(re 1)`.
  `TRUE` by default, unless `B` is already defined in the existing XML
  database.

## Value

in case options are set, invisibly a named list with the option values
that are being set; if an option is queried, the current option value.

## Details

This sets or gets units options. Set them by using named arguments, get
them by passing the option name.

The default `NA` value for `simplify` means units are not simplified in
[set_units](units.md) or [as_units](units.md), but are simplified in
arithmetical expressions.

## Examples

``` r
old = units_options(sep = c("~~~", "~"), group = c("", "")) # more space, parenthesis
old
#> $sep
#> [1] "~"  "~~"
#> 
#> $group
#> [1] "[" "]"
#> 
## set back to defaults:
units_options(sep = c("~", "~"), group = c("[", "]"), negative_power = FALSE, parse = TRUE)
units_options("group")
#> [1] "[" "]"
```
