context("Unit conversion")

test_that("we can convert numbers to unit-less units", {
  x <- as.units(1:4)
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.numeric(x), equals(1:4))
  
  y <- 1:4
  units(y) <- "1"
  expect_equal(x, y)
})

test_that("we can convert numbers to physical units", {
  x <- as.units(1:4, "m")
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(units(x), equals("m"))
  expect_equal(as.numeric(x), 1:4)
  
  y <- 1:4
  units(y) <- "m"
  expect_equal(x, y)
})