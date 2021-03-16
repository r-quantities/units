test_that("We can replace parts if they have an equivalent unit", {
  x <- 1:4 * as_units("km")
  y <- c(3000, 4000) * as_units("m")
  z <- 5000 * as_units("m")
  x[3:4] <- y
  x[[5]] <- z

  expect_length(x, 5)
  expect_equal(as.numeric(x), 1:5)
  expect_equal(as.character(units(x)), "km")
})

test_that("We can't replace parts if they have different unit", {
  x <- 1:4 * as_units("km")
  y <- c(3000, 4000) * as_units("s")
  z <- 5000 * as_units("s")

  expect_error(x[3:4] <- y)
  expect_error(x[[5]] <- z)
})

test_that("We can concatenate units if they have the same unit", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("m")
  z <- c(x, y)

  expect_equal(length(z), length(x) + length(y))
  expect_equal(x, z[1:4])
  expect_equal(y, z[1:4 + 4])
})

test_that("We can't concatenate units if they have different units", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("s")
  expect_error(c(x, y))
})

test_that("We can concatenate units if their units can be converted", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("km")
  z <- c(x, y)

  expect_equal(length(z), length(x) + length(y))
  expect_equal(as.character(units(z)), "m")
  expect_equal(x, z[1:4])
  expect_equal(set_units(y, units(as_units("m")), mode = "standard"), z[1:4 + 4])
})

test_that("We can use diff on a units object", {
  x = 1:10 * as_units("m")
  y = rep(1,9) * as_units("m")
  expect_equal(diff(x), y)
})

test_that("parse_unit works", {
  kg = as_units("kg")
  m = as_units("m")
  s = as_units("s")
  u0 = kg/m/m/s
  u = as_units("kg m-2 s-1", implicit_exponents = TRUE)
  expect_equal(u, u0)
  J = as_units("J")
  u0 = make_units(kg*kg*kg*m*m*J/s/s/s/s/s)
  u = as_units("kg3 m2 s-5 J", implicit_exponents = TRUE)
  expect_equal(u, u0)
})

test_that("deparse_unit works", {
  str = "kg m-2 s-1"
  u = as_units(str, implicit_exponents = TRUE)
  str0 = deparse_unit(u)
  expect_equal(str, str0)
})

test_that("set_units(x, u) is a short form for x * make_units(u)", {
  expect_equal(set_units(1:10, m/s), 1:10 * make_units(m/s))
  x = set_units(1:5, m/s)
  y = x
  units(y) = set_units(1, km/h)
  expect_identical(y, set_units(x, km/h))
})

test_that("rep.units works", {
  expect_equal(rep(set_units(1:2, m/s), 2), set_units(c(1,2,1,2), m/s))
})

test_that("set_units works with symbols in character data, and resolves names", {
  skip_on_cran()
  deg = paste0(enc2native(intToUtf8(176)), "C")
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, "degree_C", mode = "standard"))
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, "degree_Celsius", mode = "standard"))
  expect_equal(set_units(1:2, "degree_C", mode = "standard"), set_units(1:2, "degree_Celsius", mode = "standard"))
  expect_equal(set_units(1:2, degree_C),                      set_units(1:2, degree_Celsius))
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, degree_Celsius))
  x = set_units(1:3, km)
  y <- set_units(x, "meter", mode = "standard")
  expect_equal(y, set_units(c(1000,2000,3000), m))
})

test_that("all.equal works", {
  expect_true(all.equal(set_units(1, m/s), set_units(3.6, km/h)))
  expect_true(set_units(1, m/s) == set_units(3.6, km/h))
  expect_true(all.equal(set_units(3.6, km/h), set_units(1, m/s)))
  expect_false(set_units(3.6, km/h) == set_units(1, m/s))
})

test_that("seq works", {
  seq(to = set_units(10, m), by = set_units(1, m), length.out = 5)
  seq(set_units(10, m), by = set_units(1, m), length.out = 5)
  seq(set_units(10, m), set_units(19, m))
  seq(set_units(10, m), set_units(.02, km))
})

test_that("str works", {
  str(set_units(1/1:3, m/s))
})
