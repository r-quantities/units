test_that("ud_convert works with simple conversions", {
  expect_equal(ud_convert(1, "m", "km"), 1/1000)
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

test_that("ud_convert produces same results as .Call", {
  x <- 1:3
  from <- "m"
  to <- "km"
  
  .call_result <- .Call("_units_ud_convert", PACKAGE = "units", x, from, to)
  fn_result <- ud_convert(x, from, to)
  
  expect_identical(fn_result, .call_result)
})
