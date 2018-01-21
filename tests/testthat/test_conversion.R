context("Unit conversion")

test_that("we can convert numbers to unit-less units", {
  x <- as_units(1:4)
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.numeric(x), equals(1:4))
  
  y <- 1:4
  units(y) <- unitless
  expect_equal(x, y)
})

test_that("we can convert numbers to physical units", {
  m <- as_units("m")
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

test_that("we can convert NA values to physical units", {
  m <- as_units("m")
  x <- NA * m
  expect_equal(class(x), "units")
  expect_equal(as.character(units(x)), "m")
  expect_equal(as.numeric(x), as.numeric(NA))

  x <- set_units(NA,m/s)
  expect_equal(as.character(units(x)), "m/s")
  expect_equal(x + set_units(5,m/s), set_units(NA,m/s))
  expect_error(x + set_units(5,m))

  x <- NA
  units(x) <- m
  expect_that(as.character(units(x)), equals('m'))

  x <- rep(NA,5)
  s <- as_units("s")
  units(x) <- make_units(m/s)
  expect_equal(length(x),5)
  expect_equal(units(x),units(m/s))
  expect_equal(x,5 * x)
  expect_error(x + 1)
})

test_that("we can convert between two units that can be converted", {
  m <- as_units("m")
  km <- as_units("km")
  x <- y <- 1:4 * m
  units(x) <- km
  expect_equal(as.numeric(y), 1000 * as.numeric(x))
  library(magrittr)
  y %>% set_units(km) -> z
  expect_equal(x, z)
})

test_that("we can't convert between two units that can't be converted", {
  m <- as_units("m")
  s <- as_units("s")
  expect_error(units(m) <- s)
})

test_that("we can convert difftime objects to units", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as_units(d)
  expect_equal(as.numeric(x), as.numeric(d))
  
  week <- as.difftime(1, units = "weeks")
  units_week <- as_units(week)
  expect_equal(as.character(units(units_week)), "d")
  expect_equal(as.numeric(units_week), 7)
})

test_that("we can convert units objects to difftime objects", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as_units(d)
  y <- as_difftime(x)
  
  expect_equal(d, y)
})

#test_that("we can convert units objects to and from hms objects", {
#  s <- Sys.time()
#  library(hms)
#  d <- as.hms(s - (s + 1))
#  x <- as_units(d)
#  y <- as.hms(x)
#  
#  expect_equal(d, y)
#})

test_that("we can subscript units", {
  x <- 1:4
  y <- x * as_units("m")
  expect_equal(as.numeric(y[1]), x[1])
  expect_equal(class(y[1]), class(y))
})

test_that("m + m*s is an error", {
  m <- as_units("m")
  s <- as_units("s")
  expect_error(m + m * s)
})

test_that("we can convert between units that are not simply a scalar from each other", {
  m <- 0 * as_units("degC")
  units(m) <- as_units("degK")
  expect_equal(as.numeric(m), units:::ud_convert(0, "degC", "degK"))  
  expect_equal(as.character(units(m)), "K")
  
  temp <- 75 * as_units('degF')
  units(temp) <- as_units('degK')
  result <- temp / as_units('degF')
  expect_equal(as.numeric(result), 75)
  expect_equal(units(result), unitless)
})


test_that("dim propagates", {
  y = x = set_units(matrix(1:4,2), m)
  units(y) = make_unit("mm")
  expect_equal(dim(x), dim(y))
})
