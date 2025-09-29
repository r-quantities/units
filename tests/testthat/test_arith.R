test_that("we can compare vectors with equal units", {
  x <- 1:4 * as_units("m")
  y <- 1:4 * as_units("m")
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

  expect_error(x == set_units(1, "kg"))
})

test_that("comparing special values gives correct results", {
  x <- c(-Inf, NaN, NA, Inf) * as_units("m")
  expect_equal(x == x, drop_units(x) == drop_units(x))
  expect_equal(x > x, drop_units(x) > drop_units(x))
  expect_equal(x < x, drop_units(x) < drop_units(x))
  expect_equal(x == rev(x), drop_units(x) == rev(drop_units(x)))
  expect_equal(x > rev(x), drop_units(x) > rev(drop_units(x)))
  expect_equal(x < rev(x), drop_units(x) < rev(drop_units(x)))
})

test_that("vectors are correctly recycled in comparisons", {
  x <- 0:3 * as_units("m")
  y <- 0:1 * as_units("m")
  res <- drop_units(x) == drop_units(y)
  expect_equal(x == y, res)
  expect_equal(y == x, res)

  y <- 0:2 * as_units("m")
  expect_warning(res <- drop_units(x) == drop_units(y))
  expect_warning(expect_equal(x == y, res))
  expect_warning(expect_equal(y == x, res))
})

test_that("aliases are correctly handled in comparisons (#339)", {
  expect_true(as_units("foot") == as_units("feet"))
  expect_true(as_units("foot") == as_units("ft"))
})

test_that("we can scale units with scalars", {
  x <- 1:4
  ux <- x * as_units("m")

  expect_equal(as.numeric(10 * ux), 10 * x)
  expect_equal(as.numeric(ux / 10), x / 10)
})

test_that("we can multiply and divide units", {
  x <- 1:4 ; y <- 5:8
  m <- x * as_units("m")
  s <- y * as_units("s")

  expect_equal(as.numeric(m * s), x * y)
  expect_equal(as.numeric(m / s), x / y)

  # FIXME: There ought to be a test that the expressions get the right units
  # but I am not entirely sure how that should be wrapped. Just checking string
  # equality would give problems if units are equivalent but needs to be converted
  # first...
})

test_that("we can take powers of units", {
  x <- 1:4
  ux <- x * as_units("m")

  expect_equal(as.numeric(ux ** 2), x ** 2)
  expect_equal(as.numeric(ux ^ 2), x ^ 2)
  expect_equal(as.character(units(ux ** 2)), "m^2")
  expect_equal(as.character(units(ux ^ 2)), "m^2")

  expect_error(ux ^ 1.3)
  expect_error(ux ^ 0.3)
  expect_error(ux ^ ux)
  # expect_error(ux ^ x) --> see below: gives mixed_units
  expect_silent(ux ^ x)

  expect_equal(as.numeric(ux ** -2), x ** -2)
  expect_equal(as.numeric(ux ^ -2), x ^ -2)
  expect_equal(as.character(units(ux ** -2)), "1/m^2")
  expect_equal(as.character(units(ux ^ -2)), "1/m^2")

  expect_equal(as.numeric(ux ** 0), x ** 0)
  expect_equal(as.numeric(ux ^ 0), x ^ 0)
  expect_identical(units(ux ** 0), units(as_units(1)))
  expect_identical(units(ux ^ 0), units(as_units(1)))
})

test_that("we support unary +/-", {
  expect_equal(-set_units(10, m), set_units(-10, m))
  expect_equal(+set_units(10, m), set_units(10, m))
})

test_that("we can convert units and simplify after multiplication", {
  x <- 1:4
  y <- 1:4
  z <- 1:4
  m <- as_units("m")
  s <- as_units("s")
  km <- as_units("km")
  ux <- x * m
  uy <- y * s
  uz <- z * km

  expect_equal(as.numeric(ux/ux), x/x)
  expect_equal(as.character(units(ux/ux)), units_options("unitless_symbol"))

  expect_equal(as.numeric(ux*uy), x*y)
  expect_equal(as.character(units(ux*uy)), "m*s")
  expect_equal(as.numeric(ux*uz), x*z)
  expect_equal(as.character(units(ux*uz)), "km*m")
  expect_equal(as.numeric(set_units(ux*uz, km * km)), (x/1000)*z)
  expect_equal(as.character(units(set_units(ux*uz, km * km))), "km^2")

  expect_equal(as.numeric(ux/ux), x/x)
  expect_equal(as.character(units(ux/ux)), units_options("unitless_symbol"))
  expect_equal(as.numeric(ux/uy), x/y)
  expect_equal(as.character(units(ux/uy)), "m/s")
  expect_equal(as.numeric(ux/uz), x/(1000*z))
  expect_equal(as.character(units(ux/uz)), units_options("unitless_symbol"))
  expect_equal(as.numeric(ux/uy/uz), x/y/z/1000)
  expect_equal(as.character(units(ux/uy/uz)), "1/s")
})

test_that("inverse units are not simplified", {
  x <- 1:4
  s <- as_units("s")
  Hz <- as_units("Hz")
  ux <- x * s
  uy <- x * Hz

  expect_equal(as.character(units(ux/uy)), "s/Hz")
  expect_equal(as.character(units(uy/ux)), "Hz/s")
})

test_that("unit one is handled correctly", {
  one <- set_units(1)
  onem <- set_units(1, m)

  expect_equal(one * one, one)
  expect_equal(onem * one, onem)
  expect_equal(one * one * one, one)
  expect_equal(onem * one * one, onem)
  expect_equal(one / one, one)
  expect_equal(onem / one, onem)
  expect_equal(one ^ 3, one)
#  expect_equal(one ^ pi, one)
#  expect_equal(one ^ -pi, one)
})

test_that("we can compute powers if the result is an integer", {
  expect_equal(set_units(1:10, m^0), set_units(1:10))
  expect_equal(as.numeric(set_units(1:10, m^2) ^ 0.5), (1:10) ^ .5)
  expect_equal(as.numeric(set_units(1:10, m^2) ^ 3/2), (1:10) ^ 3/2)
  expect_error(as.numeric(set_units(1:10, m^3) ^ 0.5), "powers not divisible")
  expect_error(as.numeric(set_units(1:10, m^3) ^ 1.5))
  expect_equal(as.numeric(set_units(1:10, m^-3) ^ (-1/3)), (1:10) ^ (-1/3))
  expect_equal(as.numeric(set_units(1:10, m^-2) ^ (-1/2)), (1:10) ^ (-1/2))
  expect_equal(as.numeric(set_units(1:10, m^-2) ^ (1/2)),  (1:10) ^ (1/2))
  expect_error(set_units(1:10, m^2) ^ (1/pi), "powers not divisible")
  expect_error(set_units(1:10, m^3) ^ -1.5, "powers not divisible")
})

test_that("we can undo logatithms", {
  x <- set_units(1:5, cm^2)
  y <- set_units(1e6*x, dam^2)

  expect_equal(exp(10^log10(log(x))), set_units(x, m^2))
  expect_equal(exp(set_units(10)^log10(log(x))), set_units(x, m^2))
  expect_equal(exp(10^log10(log(y))), set_units(y, m^2))
  expect_equal(expm1(3^log(log1p(x), base=3)), set_units(x, m^2))
  expect_equal(expm1(3^log(log1p(y), base=3)), set_units(y, m^2))
  expect_error(exp(log10(x)), "wrong base in power operation")
  expect_error(exp(x), "only allowed with logarithmic unit")
  expect_error(exp(set_units(1, 1)), "only allowed with logarithmic unit")
})

test_that("%/% and %% work", {
  x <- set_units(1:5, m^2)
  y <- set_units(1.4, foot)

  expect_true(all.equal(x, y * (x %/% y) + x %% y))

  z <- set_units(drop_units(x) %% drop_units(set_units(y, m)), m^2)
  expect_equal(x %% y, z)
  expect_equal(x %% set_units(y, m), z)
  expect_equal(x %% drop_units(set_units(y, m)), z)
  expect_equal(x %% set_units(drop_units(set_units(y, m)), 1), z)
  expect_equal(x %% set_units(drop_units(set_units(y, m)), 1), z)
  expect_error(drop_units(x) %% set_units(y, m))

  z <- set_units(drop_units(set_units(x, foot^2)) %% drop_units(y), foot^2)
  expect_equal(set_units(x, foot^2) %% y, z)
  expect_equal(set_units(x, foot^2) %% set_units(y, m), z)
  expect_equal(set_units(x, foot^2) %% drop_units(y), z)
  expect_equal(set_units(x, foot^2) %% set_units(drop_units(y), 1), z)
  expect_error(drop_units(set_units(x, foot^2)) %% y)

  expect_true(all(x %% y >= set_units(0, m^2)))
})

test_that("%*% work", {
  skip_if_not_installed("base", "4.3.0")
  mat = set_units(matrix(1:5, 1), m)
  expect_equal(mat %*% t(mat), set_units(matrix(55), m^2))
  expect_equal(t(mat) %*% mat, set_units(t(unclass(mat)) %*% unclass(mat), m^2))
})

test_that("The order of multiplication for basic units is commutative", {
  a <- set_units(1:4, m^-3)
  b <- set_units(1:4, mm)

  expect_equal(a * b, b * a)
  expect_true(units(a * b) == units(b * a))

  a <- set_units(1:4, m)
  b <- set_units(1:4, mm)
  expect_equal(a * b, b * a)
  expect_true(units(a * b) == units(b * a))
})

test_that("Division gets the right scaling and units", {
  a <- set_units(1:4, m^-3)
  b <- set_units(1:4, mm)

  expect_true(units(a / b) != units(b / a))
  expect_equal(a / b, 1/(b / a))
})

test_that("we obtain mixed units when taking powers of multiple integers", {
  a = set_units(1:4, m)
  p = 4:1
  expect_equal(a ^ p, c(set_units(1, m^4), set_units(8, m^3), set_units(9, m^2), set_units(4, m),  allow_mixed=TRUE))
})

test_that("identical units can always be divided and return unitless (#310)", {
  x1 <- log(set_units(100, "g"))
  x2 <- log(set_units(4, "g"))
  expect_equal(
    x1/x2,
    set_units(log(100)/log(4), "")
  )
  expect_equal(
    x1 %/% x2,
    set_units(log(100) %/% log(4), "")
  )
})

test_that("inverse units can always be multiplied and return unitless (related to #310)", {
  x1 <- log(set_units(100, "g"))
  x2 <- log(set_units(4, "g"))
  expect_equal(
    x1*(1/x2),
    set_units(log(100)/log(4), "")
  )
})

test_that("identical unit multiplication and division respect 'simplify' option (#355)", {
  initial_unit_simplify <- units_options("simplify")
  on.exit(units_options(simplify=initial_unit_simplify))

  gg <- structure(list(numerator="g", denominator="g"), class="symbolic_units")
  x_gg <- set_units(1, gg, mode="standard")
  x_ul <- set_units(1, unitless, mode="standard")

  units_options(simplify = FALSE)
  expect_equal(set_units(1, "g/g"), x_gg)
  expect_equal(set_units(1, "g") / set_units(1, "g"), x_gg)

  units_options(simplify = TRUE)
  expect_equal(set_units(1, "g/g"), x_ul)
  expect_equal(set_units(1, "g") / set_units(1, "g"), x_ul)

  units_options(simplify = NA)
  expect_equal(set_units(1, "g/g"), x_gg)
  expect_equal(set_units(1, "g") / set_units(1, "g"), x_ul)
})
