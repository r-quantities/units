test_that("ud_are_convertible return the expected value", {
  x <- 1:10 * as_units("m")
  expect_type(ud_are_convertible("m", "km"), "logical")
  expect_true(ud_are_convertible("m", "km"))
  expect_true(ud_are_convertible(units(x), "km"))
  expect_false(ud_are_convertible("s", "kg"))

  x <- c("m", "l")
  y <- c("km", "ml", "cm", "kg")
  conv <- c(TRUE, TRUE, TRUE, FALSE)
  expect_equal(ud_are_convertible(x, y), conv)
  expect_equal(ud_are_convertible(y, x), conv)
})

test_that("ud_convert works with simple conversions", {
  x <- 1:10 * as_units("m")
  expect_equal(ud_convert(1, "m", "km"), 1/1000)
  expect_equal(ud_convert(as.numeric(x), units(x), "km"), as.numeric(x)/1000)
  expect_equal(ud_convert(1, "km", "m"), 1000)
  expect_equal(ud_convert(32, "degF", "degC"), 0)
  expect_equal(ud_convert(0, "degC", "K"), 273.15)
})

test_that("ud_convert works with vectors", {
  expect_equal(ud_convert(1:2, c("m", "mm"), "km"), 1:2/c(1e3,1e6))
  expect_equal(ud_convert(c(32, 212), "degF", "degC"), c(0, 100))
  expect_equal(ud_convert(numeric(0), "m", "km"), numeric(0))
})

test_that("ud_convert returns Error for incompatible units", {
  expect_error(ud_convert(100, "m", "kg"), "Units not convertible")
})

# element by element, as ud_convert() and ud_are_convertible() used to convert
convert_each <- function(x, from, to)
  mapply(units:::ud_convert_doubles, x, units:::ud_char(from), units:::ud_char(to))
convertible_each <- function(from, to)
  mapply(units:::ud_convertible, units:::ud_char(from), units:::ud_char(to),
         USE.NAMES = FALSE)

test_that("ud_convert() converts mixed units in order", {
  x <- c(1, 1000, 2, 1, 3000)
  from <- c("m", "mm", "m", "km", "mm")
  expect_equal(ud_convert(x, from, "m"), c(1, 1, 2, 1000, 3))
  expect_identical(ud_convert(x, from, "m"), convert_each(x, from, "m"))
  expect_identical(ud_convert(c(32, 212, 0), "degF", c("degC", "degC", "K")),
                   convert_each(c(32, 212, 0), "degF", c("degC", "degC", "K")))
})

test_that("ud_convert() and ud_are_convertible() match element by element results", {
  xs <- list(1:5, c(a = 1, b = 2), c(1, NA, NaN, Inf, -Inf, 0), 5, matrix(1:4, 2),
             TRUE, c(x = 3L))
  pairs <- list(list("m", "km"), list(c("m", "mm", "km", "cm"), "m"),
                list("degF", c("degC", "K")), list(c("m", "ft"), c("in", "mm")),
                list(c("m/s", "km/h", "m/s"), "km/h"),
                list(c("degC", "K", "degF", "degC"), "K"),
                list(units(set_units(1, m/s)), "km/h"))
  for (x in xs) for (p in pairs) # lengths that do not recycle evenly warn in both
    expect_identical(suppressWarnings(ud_convert(x, p[[1]], p[[2]])),
                     suppressWarnings(convert_each(x, p[[1]], p[[2]])))

  families <- list(c("m", "mm", "km", "cm", "in", "ft"), c("degC", "degF", "K"),
                   c("s", "min", "h", "d"))
  set.seed(1)
  for (k in 1:50) {
    n <- sample(1:30, 1)
    f <- families[[sample(length(families), 1)]]
    from <- sample(f, sample(c(1, n), 1), replace = TRUE)
    to <- sample(f, sample(c(1, n), 1), replace = TRUE)
    x <- round(rnorm(n, 100, 50), 2)
    expect_identical(ud_convert(x, from, to), convert_each(x, from, to))
    from <- sample(c(unlist(families), "foo"), n, replace = TRUE)
    expect_identical(ud_are_convertible(from, to), convertible_each(from, to))
  }
})

test_that("ud_convert() keeps mapply()'s recycling, names and edge cases", {
  expect_warning(res <- ud_convert(1:3, c("m", "mm"), "km"), "not a multiple")
  expect_equal(res, c(1e-3, 2e-6, 3e-3))
  expect_identical(names(ud_convert(c(a = 1, b = 2), c("m", "mm", "km", "cm"), "m")),
                   c("a", "b", NA, NA))
  expect_identical(ud_convert(numeric(0), "m", "km"), numeric(0))
  expect_identical(ud_convert(1, character(0), "km"), list())
  expect_identical(ud_are_convertible(character(0), character(0)), list())
  expect_identical(ud_convert(list(1, 2), "m", "km"), c(0.001, 0.002))
  expect_error(ud_convert(c(1, 2), c("m", "s"), "kg"), "Units not convertible")
})
