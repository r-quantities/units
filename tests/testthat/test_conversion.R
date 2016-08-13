context("Unit conversion")

test_that("we can convert numbers to units", {
  x <- as.units(1:4)
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.numeric(x), equals(1:4))
  
  y <- 1:4
  units(y) <- "1"
  expect_equal(x, y)
})