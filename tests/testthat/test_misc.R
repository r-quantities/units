context("Misc. utility functions")

test_that("We can concatenate units if they have the same unit", {
  x <- 1:4 * make_unit("m")
  y <- 5:8 * make_unit("m")
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(x, z[1:4])
  expect_equal(y, z[1:4 + 4])
})

test_that("We can't concatenate units if they have different units", {
  x <- 1:4 * make_unit("m")
  y <- 5:8 * make_unit("s")
  expect_error(c(x, y))
})

test_that("We can concatenate units if their units can be converted", {
  x <- 1:4 * make_unit("m")
  y <- 5:8 * make_unit("km")
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(as.character(units(z)), "m")
  expect_equal(x, z[1:4])
  expect_equal(as.units(y, units(make_unit("m"))), z[1:4 + 4])
})

test_that("We can use diff on a units object", {
  x = 1:10 * make_unit("m")
  expect_equal(diff(x), rep(1, 9))
})
