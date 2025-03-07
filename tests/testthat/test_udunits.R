test_that("ud_are_convertible return the expected value", {
  x <- 1:10 * as_units("m")
  expect_type(ud_are_convertible("m", "km"), "logical")
  expect_true(ud_are_convertible("m", "km"))
  expect_true(ud_are_convertible(units(x), "km"))
  expect_false(ud_are_convertible("s", "kg"))
})

test_that("ud_convert works with simple conversions", {
  x <- 1:10 * as_units("m")
  expect_equal(ud_convert(1, "m", "km"), 1/1000)
  expect_equal(ud_convert(as.numeric(x), units(x), "km"), as.numeric(x)/1000)
  expect_equal(ud_convert(1, "km", "m"), 1000)
  expect_equal(ud_convert(32, "degF", "degC"), 0)
  expect_equal(ud_convert(0, "degC", "K"), 273.15)
})

test_that("ud_convert works with vectors", {
  expect_equal(ud_convert(1:2, "m", "km"), 1:2/1000)
  expect_equal(ud_convert(c(32, 212), "degF", "degC"), c(0, 100))
})

test_that("ud_convert returns Error for incompatible units", {
  expect_error(ud_convert(100, "m", "kg"), "Units not convertible")
})
