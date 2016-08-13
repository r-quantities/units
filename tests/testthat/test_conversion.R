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

test_that("we can convert between two units that can be converted", {
  x <- y <- as.units(1:4, "m")
  units(x) <- "km"
  expect_equal(as.numeric(y), 1000 * as.numeric(x))
})

test_that("we can convert difftime objects to units", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as.units(d)
  expect_equal(as.numeric(x), as.numeric(d))
  
  week <- as.difftime(1, units = "weeks")
  units_week <- as.units(week)
  expect_equal(units(units_week), "d")
  expect_equal(as.numeric(units_week), 7)
})

test_that("we can subscript units", {
  x <- 1:4
  y <- as.units(x, "m")
  expect_equal(as.numeric(y[1]), x[1])
  expect_equal(class(y[1]), class(y))
})