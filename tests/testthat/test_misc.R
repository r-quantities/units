context("Misc. utility functions")

test_that("We can concatenate units if they have the same unit", {
  x <- 1:4 * ud_units$m
  y <- 5:8 * ud_units$m
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(x, z[1:4])
  expect_equal(y, z[1:4 + 4])
})

test_that("We can't concatenate units if they have different units", {
  x <- 1:4 * ud_units$m
  y <- 5:8 * ud_units$s
  expect_error(c(x, y))
})

test_that("We can concatenate units if their units can be converted", {
  x <- 1:4 * ud_units$m
  y <- 5:8 * ud_units$km
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(as.character(units(z)), "m")
  expect_equal(x, z[1:4])
  expect_equal(as.units(y, units(ud_units$m)), z[1:4 + 4])
})

