# version 0.6-3

* improve platform dependent encodings handling; #183

* don't force `as.numeric` when unnecessary; #182 addressing #181

* fix valgrind issues on CRAN and tidy up tests; #193 addressing #192

* new method `drop_units` for data frames; #191 addressing #187

# version 0.6-2

* fix support for logarithms and decibels; #177 addressing #176
* add delayed S3 registration mechanism for R >= 3.6.0

# version 0.6-1

* vectors with mixed units are now supported; #145

* `NA` values for units now trigger a proper error message; #163

# version 0.6-0

* print units as [unit] more consistently, e.g. for single unit and in data.frames; #132

* improve printing of unitless units; provide option to print something else than 1; #150

* fix printing unitless in labels when `negative_power` is `TRUE`; #133

* `install_symbolic_unit` now adds a dimensionless unit, integrated in the units system, meaning that prefixes on it work as well; #71

* `install_conversion_constant` and `install_conversion_offset` now install a new unit that is a function of an existing udunits unit.; #71, #84

* unit simplification can now be user-controlled by `units_options`; #89

* `set_units(15, mg/kg)` is now no longer simplified to 1e-9 unitless; #89

* directly uses the udunits2 C library; drop dependence on R package `udunits2`, fixing R package `udunits2` memory leaks; #135

* drops `%*%`, no longer gives warning when loading

# version 0.5-1

# version 0.5-0

* deal with trigonometric functions for units degree; return units rad on inverse trigonometric functions.

* Unit creation has been significantly refactored. `units<-` now accepts strings
or quoted language objects on the right hand side, powered by new S3 methods for
`as_units`. All valid unit symbols and unit names recognized by package 'udunits2' are 
now accepted. New user facing function `make_units()` (plural s) is also
provided. See `?as_units` for details. @t-kalinowski

* new functions `valid_udunits()` and `valid_udunits_prefixes()` generate tidy
dataframes listing all the valid unit names, symbols, and prefixes recognized by
udunits. @t-kalinowski

* new function `install_symbolic_unit()` for adding custom, user-defined units. 
@t-kalinowski

* `make_unit` and `parse_unit` (singular unit) have been deprecated, please use 
`as_units` instead.

* `ud_units` is no longer necessary and is soft-deprecated, and may be removed
in a future release.

* add `%*%` as an S3 generic; #54 

* add `%%` and `%/%` to `Ops.units`

* support unary + and - ; #56

* add `seq` method for `units`, converting units to those of the first argument

* Deprecate `as.dt` for `as_difftime`, `as.units` for `as_units` and `as_cf` for `deparse_unit`

# version 0.4-6

* add `all.equal` method for `units`; #51

* add `deparse_unit` to replace `as_cf`

* add calender/time conversions between `udunits` time units like `minutes from 1900-0-0`, and R's `POSIXct` and `Date`

* add `as_units` to replace `as.units`

* rename `as.dt` to `as_difftime`

# version 0.4-5

* add support for user-defined unit conversion; #31

* allow for 1/n integer powers, as in `set_units(1:10, m^-2) ^ 0.5`; #29

* properly set log units after log transform; #33

* `sin`, `cos` and `tan` no longer complain when units is `rad`, and return `unitless`; #40 

* now allow for `set_units(1:3, "°C")` and also `set_units(1:3, "degree_Celsius")` by resolving names to symbols first; #43

* `set_units(x)` with `x` numeric sets units to `unitless`; #41

# version 0.4-4

* fix a result units bug when multiplying or dividing units vectors of different length, #34

* add a `rep` method for `units` vectors

# version 0.4-3

* support for `set_units(1:10, m)` which does not require to declare or define, `m` (`m` is resolved automatically from `ud_units`)
