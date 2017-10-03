library(units)
library(testthat)

ref = set_units(1, m)

# character input
expect_equal(set_units(1, "m"), ref)
# units input
expect_equal(set_units(1, make_unit("m")), ref)
# symbolic_units input
expect_equal(set_units(1, units(ref)), ref)

set_units(1, m/s)

# use object from parent frame, non-ambiguous:
foo = ref
expect_equal(set_units(1, foo), ref)
foo = "m"
expect_equal(set_units(1, foo), ref)
foo = make_unit("m")
expect_equal(set_units(1, foo), ref)

# ambiguous, should warn:
m = "u"
set_units(1, m) # m by NSE
m = make_unit("u")
set_units(1, m) # m by NSE

degree_C = "m"
set_units(1, degree_C)

degree_C = make_unit("m")
(dc <- set_units(1, degree_C))

degree_C = units(dc)
set_units(1, degree_C)

# OK:
set_units(1, "bar")
(tenbar <- set_units(10, bar))
(set_units(1, units(tenbar)))
(set_units(1, tenbar))

# still worrying:
set_units(1, units(degree_C))
udunits2::ud.is.parseable("units(degree_C)")
# no warning, because units(degree_C) does not eval
set_units(1, units(m))
udunits2::ud.is.parseable("units(m)")
set_units(10, uu <- degree_C)
