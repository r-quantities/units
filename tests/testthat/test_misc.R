context("Misc. utility functions")

test_that("We can concatenate units if they have the same unit", {
  x <- as.units(1:4, "m")
  y <- as.units(5:8, "m")
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(x, z[1:4])
  expect_equal(y, z[1:4 + 4])
})

test_that("We can't concatenate units if they have different units", {
  x <- as.units(1:4, "m")
  y <- as.units(1:4, "s")
  expect_error(c(x, y))
})

test_that("We can concatenate units if their units can be converted", {
  x <- as.units(1:4, "m")
  y <- as.units(5:8, "km")
  z <- c(x, y)
  
  expect_equal(length(z), length(x) + length(y))
  expect_equal(units(z), "m")
  expect_equal(x, z[1:4])
  expect_equal(as.units(y, "m"), z[1:4 + 4])
})