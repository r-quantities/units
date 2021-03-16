test_that("we can make symbolic units", {
  m <- units:::.symbolic_units("m")
  expect_true(inherits(m, "symbolic_units"))
  expect_equal(as.character(m), "m")
})

test_that("we can multiply and divide symbolic units", {
  m <- units:::.symbolic_units("m")
  s <- units:::.symbolic_units("s")
  expect_equal(as.character(units(units:::.multiply_symbolic_units(1, m, m))), "m^2")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, s))), "m/s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, m, s)), s))), "m/s^2")
})

test_that("we can simplify basic units", {
  m <- units:::.symbolic_units("m")
  s <- units:::.symbolic_units("s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, m))), units_options("unitless_symbol"))
  opt = units_options(unitless_symbol = "-")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, m))), "-")
  units_options(unitless_symbol = opt$unitless_symbol)
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, m))), units_options("unitless_symbol"))
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, m, s)), m))), "1/s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.multiply_symbolic_units(1, m, m)), m))), "m")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, s, m)), s))), "1/m")
})

test_that("we can simplify basic units", {
  m <- units:::.symbolic_units("m")
  s <- units:::.symbolic_units("s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, m))), units_options("unitless_symbol"))
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, m, s)), m))), "1/s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.multiply_symbolic_units(1, m, m)), m))), "m")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, s, m)), s))), "1/m")
})

test_that("we can simplify basic units with conversion", {
  m <- units:::.symbolic_units("m")
  s <- units:::.symbolic_units("s")
  km <- units:::.symbolic_units("km")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, m))), units_options("unitless_symbol"))
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, m, km))), units_options("unitless_symbol"))
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.divide_symbolic_units(1, m, s)), km))), "1/s")
  expect_equal(as.character(units(units:::.divide_symbolic_units(1, units(units:::.multiply_symbolic_units(1, m, m)), km))), "m")
})

test_that("we can compare units", {
  g <- as_units("g")
  m <- as_units("m")
  s <- as_units("s")
  d <- as_units("day")
  e1 <- 1:10 * g * m / s / d
  e2 <- 1:10 * m / s
  expect_equal(units:::.same_units(units(e1),units(e1)), TRUE)
  expect_equal(units:::.same_units(units(e1),units(e2)), FALSE)
  expect_equal(units(e1) == units(e1), TRUE)
  expect_equal(units(e1) != units(e1), FALSE)
  expect_equal(units(e1) == units(e2), FALSE)
  expect_equal(units(e1) != units(e2), TRUE)
})

test_that("symbolic_unit works", {
  expect_error(units:::symbolic_unit("qqq"))
  expect_silent(units:::symbolic_unit("m"))
})
