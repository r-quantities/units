library(units)
library(testthat)

ref = set_units(1, m)

# character input
expect_equal(set_units(1, "m", mode = "standard"), ref)
# units input
expect_equal(set_units(1, as_units("m"), mode = "standard"), ref)
# symbolic_units input
expect_equal(set_units(1, units(ref), mode = "standard"), ref)

set_units(1, m/s)

# use object from parent frame, non-ambiguous:
foo = ref
expect_equal(set_units(1, foo, mode = "standard"), ref)
foo = "m"
expect_equal(set_units(1, foo, mode = "standard"), ref)
foo = as_units("m")
expect_equal(set_units(1, foo, mode = "standard"), ref)

# ambiguous, should warn:
m = "u"
set_units(1, m) # m by NSE
m = as_units("u")
set_units(1, m, mode = "standard") # u by units
set_units(1, m) # m by NSE

degree_C = "m"
set_units(1, degree_C)

degree_C = as_units("m")

set_units(1, degree_C)

# OK:
set_units(1, "bar", mode = "standard")
(tenbar <- set_units(10, bar))
(set_units(1, units(tenbar), mode = "standard"))
(set_units(1, tenbar, mode = "standard"))

set_units(10, uu <- degree_C, mode = "standard")
