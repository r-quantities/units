context("Helpers")

test_that("keep_units restores units", {
  x <- set_units(1:5, m)

  expect_identical(x, keep_units(drop_units, x))
})
