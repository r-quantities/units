context("Arithmetic")

test_that("we can compare vectors with equal units", {
  x <- 1:4 * as_units("m")
  y <- 1:4 * as_units("m")
  z <- 2 * y
  
  expect_true(all(x == y))
  expect_true(all(x <= y))
  expect_true(all(x >= y))
  
  expect_true(all(x < z))
  expect_true(all(x <= z))
  expect_true(all(z > x))
  expect_true(all(z >= x))
  
  expect_false(any(x > y))
  expect_false(any(x != y))
  expect_true(all(x != z))
})

test_that("we can scale units with scalars", {
  x <- 1:4
  ux <- x * as_units("m")
  
  expect_equal(as.numeric(10 * ux), 10 * x)
  expect_equal(as.numeric(ux / 10), x / 10)
})

test_that("we can multiply and divide units", {
  x <- 1:4 ; y <- 5:8
  m <- x * as_units("m")
  s <- y * as_units("s")
  
  expect_equal(as.numeric(m * s), x * y)
  expect_equal(as.numeric(m / s), x / y)
  
  # FIXME: There ought to be a test that the expressions get the right units
  # but I am not entirely sure how that should be wrapped. Just checking string
  # equality would give problems if units are equivalent but needs to be converted
  # first...
})

test_that("we can take powers of units", {
  x <- 1:4
  ux <- x * as_units("m")
  
  expect_equal(as.numeric(ux ** 2), x ** 2)
  expect_equal(as.numeric(ux ^ 2), x ^ 2)
  expect_equal(as.character(units(ux ** 2)), "m^2")
  expect_equal(as.character(units(ux ^ 2)), "m^2")
  
  expect_error(ux ^ 1.3)
  expect_error(ux ^ 0.3)
  expect_error(ux ^ ux)
  expect_error(ux ^ x)
  
  expect_equal(as.numeric(ux ** -2), x ** -2)
  expect_equal(as.numeric(ux ^ -2), x ^ -2)
  expect_equal(as.character(units(ux ** -2)), "1/m^2")
  expect_equal(as.character(units(ux ^ -2)), "1/m^2")
  
  expect_equal(as.numeric(ux ** 0), x ** 0)
  expect_equal(as.numeric(ux ^ 0), x ^ 0)
  expect_identical(units(ux ** 0), set_units(1))
  expect_identical(units(ux ^ 0), set_units(1))
})

test_that("we support unary +/-", {
  expect_equal(-set_units(10, m), set_units(-10, m))
  expect_equal(+set_units(10, m), set_units(10, m))
})

test_that("we can convert units and simplify after multiplication", {
  x <- 1:4
  y <- 1:4
  z <- 1:4
  m <- as_units("m")
  s <- as_units("s")
  km <- as_units("km")
  ux <- x * m
  uy <- y * s
  uz <- z * km
  
  expect_equal(as.numeric(ux/ux), x/x)
  expect_equal(as.character(units(ux/ux)), "1")
  
  expect_equal(as.numeric(ux*uy), x*y)
  expect_equal(as.character(units(ux*uy)), "m*s")
  expect_equal(as.numeric(ux*uz), x*z)
  expect_equal(as.character(units(ux*uz)), "km*m")
  expect_equal(as.numeric(set_units(ux*uz, km * km)), (x/1000)*z)
  expect_equal(as.character(units(set_units(ux*uz, km * km))), "km^2")
  
  expect_equal(as.numeric(ux/ux), x/x)
  expect_equal(as.character(units(ux/ux)), "1")
  expect_equal(as.numeric(ux/uy), x/y)
  expect_equal(as.character(units(ux/uy)), "m/s")
  expect_equal(as.numeric(ux/uz), x/(1000*z))
  expect_equal(as.character(units(ux/uz)), "1")
  expect_equal(as.numeric(ux/uy/uz), x/y/z/1000)
  expect_equal(as.character(units(ux/uy/uz)), "1/s")
})

test_that("unit one is handled correctly", {
  one <- set_units(1)
  onem <- set_units(1, m)

  expect_equal(one * one, one)
  expect_equal(onem * one, onem)
  expect_equal(one * one * one, one) 
  expect_equal(onem * one * one, onem)
  expect_equal(one / one, one)
  expect_equal(onem / one, onem)
  expect_equal(one ^ 3, one)
#  expect_equal(one ^ pi, one)
#  expect_equal(one ^ -pi, one)
})

test_that("we can compute powers +/- 1/n for integer n", {
  expect_equal(as.numeric(set_units(1:10, m^2) ^ 0.5), (1:10) ^ .5)
  expect_error(as.numeric(set_units(1:10, m^3) ^ 0.5), "units not divisible")
  expect_error(as.numeric(set_units(1:10, m^3) ^ 1.5))
  expect_equal(as.numeric(set_units(1:10, m^-3) ^ (-1/3)), (1:10) ^ (-1/3))
  expect_equal(as.numeric(set_units(1:10, m^-2) ^ (-1/2)), (1:10) ^ (-1/2))
  expect_equal(as.numeric(set_units(1:10, m^-2) ^ (1/2)),  (1:10) ^ (1/2))
  expect_error(set_units(1:10, m^2) ^ (1/pi), "not a integer divisor")
  expect_error(set_units(1:10, m^3) ^ -1.5, "currently you can only take integer powers of units below -1")
})

test_that("%*%, %/% and %% work", {
  a = set_units(1:5, m)
  expect_equal(a %/% set_units(2, m), set_units(c(0,1,1,2,2)))
  expect_equal(a %% set_units(2, m), set_units(c(1,0,1,0,1), m))
  mat = set_units(matrix(1:5, 1), m)
  # expect_equal(mat %*% t(mat), set_units(matrix(55), m^2))
  # expect_equal(t(mat) %*% mat, set_units(t(unclass(mat)) %*% unclass(mat), m^2))
})

test_that("The order of multiplication for basic units is commutative", {
  a <- set_units(1:4, m^-3)
  b <- set_units(1:4, mm)
  
  expect_equal(a * b, b * a)
  expect_true(units(a * b) == units(b * a))
  
  a <- set_units(1:4, m)
  b <- set_units(1:4, mm)
  expect_equal(a * b, b * a)
  expect_true(units(a * b) == units(b * a))
})

test_that("Division gets the right scaling and units", {
  a <- set_units(1:4, m^-3)
  b <- set_units(1:4, mm)
  
  expect_true(units(a / b) != units(b / a))
  expect_equal(a / b, 1/(b / a))
})
