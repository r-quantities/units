context("Unit conversion")

test_that("we can convert numbers to unit-less units", {
  x <- as.units(1:4)
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.numeric(x), equals(1:4))
  
  y <- 1:4
  units(y) <- unitless
  expect_equal(x, y)
})

test_that("we can convert numbers to physical units", {
  m <- make_unit("m")
  x <- 1:4 * m
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.character(units(x)), equals("m"))
  expect_equal(as.numeric(x), 1:4)
  
  y <- 1:4
  units(y) <- m
  expect_equal(x, y)
  
  z <- 1:4 / m
  expect_that(length(z), equals(4))
  expect_that(class(z), equals("units"))
  expect_that(as.character(units(z)), equals("1/m"))
  expect_equal(as.numeric(z), 1:4)
})

test_that("we can convert between two units that can be converted", {
  m <- make_unit("m")
  km <- make_unit("km")
  x <- y <- 1:4 * m
  units(x) <- km
  expect_equal(as.numeric(y), 1000 * as.numeric(x))
  library(magrittr)
  y %>% set_units(km) -> z
  expect_equal(x, z)
})

test_that("we can't convert between two units that can't be converted", {
  m <- make_unit("m")
  s <- make_unit("s")
  expect_error(units(m) <- s)
})

test_that("we can convert difftime objects to units", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as.units(d)
  expect_equal(as.numeric(x), as.numeric(d))
  
  week <- as.difftime(1, units = "weeks")
  units_week <- as.units(week)
  expect_equal(as.character(units(units_week)), "d")
  expect_equal(as.numeric(units_week), 7)
})

test_that("we can convert units objects to difftime objects", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as.units(d)
  y <- as.dt(x)
  
  expect_equal(d, y)
})

#test_that("we can convert units objects to and from hms objects", {
#  s <- Sys.time()
#  library(hms)
#  d <- as.hms(s - (s + 1))
#  x <- as.units(d)
#  y <- as.hms(x)
#  
#  expect_equal(d, y)
#})

test_that("we can subscript units", {
  x <- 1:4
  y <- x * make_unit("m")
  expect_equal(as.numeric(y[1]), x[1])
  expect_equal(class(y[1]), class(y))
})

test_that("m + m*s is an error", {
  m <- make_unit("m")
  s <- make_unit("s")
  expect_error(m + m * s)
})

test_that("we can convert between units that are not simply a scalar from each other", {
  m <- 0 * parse_unit("degC")
  units(m) <- parse_unit("degK")
  expect_equal(as.numeric(m), udunits2::ud.convert(0, "degC", "degK"))  
  expect_equal(as.character(units(m)), "degK")
  
  temp <- 75 * parse_unit('degF')
  units(temp) <- parse_unit('degK')
  result <- temp / parse_unit('degF')
  expect_equal(as.numeric(result), 75)
  expect_equal(units(result), unitless)
})

