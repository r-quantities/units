test_that("keep_units restores units", {
  x <- set_units(1:5, m)

  expect_identical(x, keep_units(drop_units, x))
})

test_that("keep_units warns when no units are provided", {
  x <- 1:5

  expect_warning(keep_units(sum, x))

  expect_identical(suppressWarnings(keep_units(sum, x)), sum(x))
})

test_that("keep_units sets units when `unit` argument is provide by user", {
  rate <- set_units(3, "1/min")
  x <- keep_units(rexp, 3, rate, unit=units(1/rate))
  expect_identical(units(x), units(1/rate))

})
