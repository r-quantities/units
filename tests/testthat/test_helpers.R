test_that("keep_units restores units", {
  x <- set_units(1:5, m)

  expect_identical(x, keep_units(drop_units, x))
})

test_that("keep_units warns when no units are provide", {
  x <- 1:5

  expect_warning(keep_units(sum, x))

  expect_identical(suppressWarnings(keep_units(sum, x)), sum(x))
})
