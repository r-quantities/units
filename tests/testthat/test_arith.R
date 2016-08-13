context("Arithmetic")

test_that("we can compare vectors with equal units", {
  x <- 1:4 * ud_units$m
  y <- 1:4 * ud_units$m
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
  ux <- x * ud_units$m
  
  expect_equal(as.numeric(10 * ux), 10 * x)
  expect_equal(as.numeric(ux / 10), x / 10)
})

test_that("we can multiply and divide units", {
  x <- 1:4 ; y <- 5:8
  m <- x * ud_units$m
  s <- y * ud_units$s
  
  expect_equal(as.numeric(m * s), x * y)
  expect_equal(as.numeric(m / s), x / y)
  
  # FIXME: There ought to be a test that the expressions get the right units
  # but I am not entirely sure how that should be wrapped. Just checking string
  # equality would give problems if units are equivalent but needs to be converted
  # first...
})

test_that("we can take powers of units", {
  x <- 1:4
  ux <- x * ud_units$m
  
  expect_equal(as.numeric(ux ** 2), x ** 2)
  expect_equal(as.numeric(ux ^ 2), x ^ 2)
  
  expect_error(ux ^ ux)
  expect_error(ux ^ x)
  
  # FIXME: There ought to be a test that the expressions get the right units
  # but I am not entirely sure how that should be wrapped. Just checking string
  # equality would give problems if units are equivalent but needs to be converted
  # first...
})

