context("Symbolic unit manipulation")

test_that("we can make symbolic units", {
  m <- units:::.make_symbolic_units("m")
  expect_true(inherits(m, "symbolic_units"))
  expect_equal(as.character(m), "m")
})

test_that("we can multiply and divide symbolic units", {
  m <- units:::.make_symbolic_units("m")
  s <- units:::.make_symbolic_units("s")
  expect_equal(as.character(m*m), "m*m")
  expect_equal(as.character(m/s), "m/s")
  expect_equal(as.character(m/s/s), "m/s/s")
})

test_that("we can simplify basic units", {
  m <- units:::.make_symbolic_units("m")
  s <- units:::.make_symbolic_units("s")
  expect_equal(as.character(m/m), "1")
  expect_equal(as.character(m/s/m), "1/s")
  expect_equal(as.character(m*m/m), "m")
  expect_equal(as.character(s/m/s), "1/m")
})