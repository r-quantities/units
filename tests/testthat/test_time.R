context("Date/time")

test_that("we can convert from udunits time/date to R", {
	expect_equal(as.Date("1999-10-31"), as.Date(set_units(1, "days since 1999-10-30")))
	expect_equal(as.POSIXct("1999-10-31 01:00:00", tz = "UTC"), 
		as.POSIXct(set_units(25, "hours since 1999-10-30")))
})

test_that("we can convert from R's time/date to udunits", {
	expect_equal(
		as_units(as.Date("1970-01-10")),
		set_units(9, "days since 1970-01-01"))
	expect_equal(
		as_units(as.Date("1999-10-31"), "days since 1999-10-30"),
		set_units(1, "days since 1999-10-30"))
	expect_equal(
		as_units(as.POSIXct("1970-01-01 01:00:00", tz = "UTC")), 
		set_units(3600, "seconds since 1970-01-01 00:00:00 +00:00"))
	expect_equal(
		as_units(as.POSIXct("1999-10-31 01:00:00", tz = "UTC"), "hours since 1999-10-30"), 
		set_units(25, "hours since 1999-10-30"))
})
