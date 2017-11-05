# version 0.5-0

* Unit creation has been significantly refactored. New user facing unit creation
functions are `make_units()` and `parse_units()`, which parse units as R
expressions. see `?make_units` for details. @t-kalinowski

* `make_unit` has been depracated, please use `symbolic_unit` instead.

* `parse_unit` has been depracated, please use `parse_units(implicit_exponents = TRUE)` instead.

* `set_units` is no longer an exported generic. Instead, please define methods for `units<-`.

* `ud_units` is no longer necessary and is soft-deprecated, and may be removed in a future release.

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
