test_that("ud_are_convertible return the expected value", {
  x <- 1:10 * as_units("m")
  expect_type(ud_are_convertible("m", "km"), "logical")
  expect_true(ud_are_convertible("m", "km"))
  expect_true(ud_are_convertible(units(x), "km"))
  expect_false(ud_are_convertible("s", "kg"))

  x <- c("m", "l")
  y <- c("km", "ml", "cm", "kg")
  conv <- c(TRUE, TRUE, TRUE, FALSE)
  expect_equal(ud_are_convertible(x, y), conv)
  expect_equal(ud_are_convertible(y, x), conv)
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
  expect_equal(ud_convert(1:2, c("m", "mm"), "km"), 1:2/c(1e3,1e6))
  expect_equal(ud_convert(c(32, 212), "degF", "degC"), c(0, 100))
})

test_that("ud_convert returns Error for incompatible units", {
  expect_error(ud_convert(100, "m", "kg"), "Units not convertible")
})
