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

  expect_equal(as.character(units(as_units("1/m2/kg/L"))), "1/(m^2*kg*L)")
  expect_equal(deparse_unit(as_units("1/m2/kg/L")), "m-2 kg-1 L-1")
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

test_that("convert_to_base works", {
  u <- set_units(1:5, "kJ/kg", mode = "standard")
  expect_equal(convert_to_base(u), set_units(u, "J/kg"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE), set_units(u, "Gy"))
  expect_equal(convert_to_base(u, keep_fraction=TRUE, simplify=FALSE), set_units(u, "m^2*kg/s^2/kg"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE, simplify=FALSE), set_units(u, "m^2/s^2"))

  u <- set_units(1:5, "celsius", mode = "standard")
  expect_equal(convert_to_base(u), set_units(u, "K"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE), set_units(u, "K"))
  expect_equal(convert_to_base(u, keep_fraction=TRUE, simplify=FALSE), set_units(u, "K"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE, simplify=FALSE), set_units(u, "K"))

  u <- set_units(1:5, "kJ/(kg*fahrenheit)", mode = "standard")
  expect_equal(convert_to_base(u), set_units(u, "J/kg/K"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE), set_units(u, "m^2/s^2/K"))
  expect_equal(convert_to_base(u, keep_fraction=TRUE, simplify=FALSE), set_units(u, "m^2*kg/s^2/kg/K"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE, simplify=FALSE), set_units(u, "m^2/s^2/K"))

  u <- set_units(1:5, "J/s", mode = "standard")
  expect_equal(convert_to_base(u), set_units(u, "J/s"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE), set_units(u, "W"))
  expect_equal(convert_to_base(u, keep_fraction=TRUE, simplify=FALSE), set_units(u, "m^2*kg/s^3"))
  expect_equal(convert_to_base(u, keep_fraction=FALSE, simplify=FALSE), set_units(u, "m^2*kg/s^3"))
})
