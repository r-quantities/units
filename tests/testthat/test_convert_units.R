test_that("convert_units works with simple conversions", {
  expect_equal(convert_units(1, "m", "km"), 1/1000)
  expect_equal(convert_units(1, "km", "m"), 1000)
  expect_equal(convert_units(32, "degF", "degC"), 0)
  expect_equal(convert_units(0, "degC", "K"), 273.15)
})

test_that("convert_units works with vectors", {
  expect_equal(convert_units(1:2, "m", "km"), 1:2/1000)
  expect_equal(convert_units(c(32, 212), "degF", "degC"), c(0, 100))
})

test_that("convert_units returns Error for incompatible units", {
  expect_error(convert_units(100, "m", "kg"), "Units not convertible")
})

test_that("convert_units produces same results as .Call", {
  x <- 1:3
  from <- "m"
  to <- "km"
  
  .call_result <- .Call("_units_ud_convert", PACKAGE = "units", x, from, to)
  fn_result <- convert_units(x, from, to)
  
  expect_identical(fn_result, .call_result)
})
