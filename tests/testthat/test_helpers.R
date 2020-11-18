context("Helpers")

test_that("keep_units restores units", {
  x <- set_units(1:5, m)
  
  expect_identical(x, keep_units(drop_units, x))
})

test_that("adapt_units converts units", {
  x <- set_units(1000, m)
  y <- set_units(1, km)
  
  expect_identical(adapt_units(x, y), y)
})
