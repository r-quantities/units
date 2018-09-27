context("Math functions")

test_that("we can call math functions on units", {
  x <- 1:4 - 2.1
  ux <- x * as_units("m")
  
  expect_equal(as.numeric(abs(ux)), abs(x))
  expect_equal(units(abs(ux)), units(ux))
  
  expect_equal(as.numeric(sign(ux)), sign(x))
  expect_true(!inherits(sign(ux), "units"))
  
  expect_equal(as.numeric(sqrt(ux^2)), sqrt(x^2))
  expect_error(sqrt(ux), "units not divisible")
  
  expect_equal(as.numeric(floor(ux)), floor(x))
  expect_equal(units(floor(ux)), units(ux))
  expect_equal(as.numeric(ceiling(ux)), ceiling(x))
  expect_equal(units(ceiling(ux)), units(ux))
  expect_equal(as.numeric(trunc(ux)), trunc(x))
  expect_equal(units(trunc(ux)), units(ux))
  expect_equal(as.numeric(round(ux)), round(x))
  expect_equal(units(round(ux)), units(ux))
  expect_equal(as.numeric(signif(ux)), signif(x))
  expect_equal(units(signif(ux)), units(ux))
  
  expect_equal(as.numeric(cumsum(ux)), cumsum(x))
  expect_equal(units(cumsum(ux)), units(ux))
  expect_equal(as.numeric(cummax(ux)), cummax(x))
  expect_equal(units(cummax(ux)), units(ux))
  expect_equal(as.numeric(cummin(ux)), cummin(x))
  expect_equal(units(cummin(ux)), units(ux))
  
  expect_warning(y <- cos(ux))
  expect_equal(y, cos(x))
  expect_equal(class(y), "numeric")
  
  expect_equal(sin(set_units(1, rad)), set_units(sin(1)))
  expect_equal(sin(set_units(90, degree)), sin(set_units(pi/2, rad)))
  expect_equal(units(acos(set_units(-1))), units(make_units(rad)))
  expect_equal(set_units(acos(set_units(-1)), degree), set_units(180, degree))
})

test_that("we can take logarithms units", {
  x <- 1:4
  ux <- x * as_units("m")
  
  expect_equal(as.numeric(log2(ux)), log2(x))
  expect_equal(units(log2(ux)), units(as_units("lb(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log(ux, 2)), log(x, 2))
  expect_equal(units(log(ux, 2)), units(as_units("lb(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log10(ux)), log10(x))
  expect_equal(units(log10(ux)), units(as_units("lg(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log(ux, 10)), log(x, 10))
  expect_equal(units(log(ux, 10)), units(as_units("lg(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log1p(ux)), log1p(x))
  expect_equal(units(log1p(ux)), units(as_units("ln(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log(ux)), log(x))
  expect_equal(units(log(ux)), units(as_units("ln(re 1 m)", force_single_symbol=TRUE)))
  
  expect_equal(as.numeric(log(ux, exp(2))), log(x, exp(2)))
  expect_equal(units(log(ux, exp(2))), units(as_units("0.5 ln(re 1 m)", force_single_symbol=TRUE)))
})
