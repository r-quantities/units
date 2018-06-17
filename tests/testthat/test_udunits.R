context("udunits2")

test_that("udunits error messages", {
  expect_error(set_units(1:3, "qqq"), "qqq")
})

test_that("udunits low-level functions work", {
  expect_silent(units:::R_ut_get_dimensionless_unit_one(character(0)))
  a <- units:::R_ut_parse("m")
  b <- units:::R_ut_parse("g")
  expect_error(units:::R_convert_doubles(a, b, 1:10), "not convertible")

  u = units:::R_ut_offset("kg", "g10", 10)
  expect_equal(set_units(set_units(1, kg), g10), set_units(11, g10))

  expect_silent(units:::R_ut_divide(a, b))
  expect_silent(units:::R_ut_multiply(a, b))
  expect_silent(sq <- units:::R_ut_raise(a, 2L))
  expect_silent(units:::R_ut_log(a, 10.0))
  expect_silent(units:::R_ut_format(a))
  expect_silent(units:::R_ut_format(sq, names = TRUE))
  expect_silent(units:::R_ut_format(sq, definition = TRUE))
  expect_silent(units:::R_ut_set_encoding("ascii"))
  expect_silent(units:::R_ut_set_encoding("iso-8859-1"))
  expect_silent(units:::R_ut_set_encoding("latin1"))
  expect_silent(units:::R_ut_set_encoding("utf8"))
  expect_error(units:::R_ut_set_encoding("foo"))
  expect_error(units:::R_ut_get_symbol("foo"), "string unit representation contains unknown word")
  expect_error(units:::R_ut_get_name("foo"), "R_ut_get_name")
})
