context("Date/time")

test_that("we can convert from udunits time/date to R", {
  #skip("set_units doesn't accept strings")
  # rewrite this to 
	expect_equal(as.Date("1999-10-31"), 
	             as.Date(set_units(1, symbolic_unit("days since 1999-10-30"), mode = "units")))
	expect_equal(as.POSIXct("1999-10-31 01:00:00", tz = "UTC"), 
		as.POSIXct(set_units(25, symbolic_unit("hours since 1999-10-30"), mode = "units")))
})

test_that("we can convert from R's time/date to udunits", {
  #skip("set_units doesn't accept strings")
	expect_equal(
		as_units(as.Date("1970-01-10")),
		set_units(9, symbolic_unit("days since 1970-01-01"), mode = "units"))
	expect_equal(
		as_units(as.Date("1999-10-31"), "days since 1999-10-30"),
		set_units(1, symbolic_unit("days since 1999-10-30"), mode = "units"))
	expect_equal(
		as_units(as.POSIXct("1970-01-01 01:00:00", tz = "UTC")), 
		set_units(3600, symbolic_unit("seconds since 1970-01-01 00:00:00 +00:00"), mode = "units"))
	expect_equal(
		as_units(as.POSIXct("1999-10-31 01:00:00", tz = "UTC"), "hours since 1999-10-30"), 
		set_units(25, symbolic_unit("hours since 1999-10-30"), mode = "units"))
})
