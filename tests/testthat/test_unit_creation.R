test_that("global options are respected", {
  # rt: round trip
  rt <- function(x) as.character(units(as_units(x)))

  expect_equal("g", rt("grams"))

  #op <- units_options(auto_convert_names_to_symbols = FALSE)
  units_options(auto_convert_names_to_symbols = FALSE)
  expect_equal("grams", rt("grams"))

  units_options(auto_convert_names_to_symbols = TRUE)
  expect_equal("g", rt("grams"))

  #units_options(op)
  expect_equal("g", rt("grams"))

  o = units_options("set_units_mode")
  g = set_units(1, g)
  units_options(set_units_mode = "standard")
  expect_equal(set_units(1, "g"), g)
  units_options(set_units_mode = o)

})


test_that("various forms of unit creation and destruction work", {

  #ox <- x <- 1L:4L # why should ints be preserved?
  ox <- x <- as.numeric(1:4)

  units(x) <- "m/s"
  expect_s3_class(x,"units")

  units(x) <- NULL
  expect_identical(x, ox)

  units(x) <- character(0)
  expect_identical(x, ox)

  ox <- x <- matrix(ox, ncol = 2)

  units(x) <- "m/s"
  expect_s3_class(x,"units")

  units(x) <- NULL
  expect_identical(x, ox)

  units(x) <- character(0)
  expect_identical(x, ox)

  ox <- y <- x <- as.numeric(1:4)
  units(x) <- "m/s"

  expect_identical(x, set_units(y, m/s))
  expect_identical(x, set_units(y, "m/s", mode = "standard"))

  expect_identical(ox, set_units(x, NULL))
  expect_identical(ox, set_units(x, NULL, mode = "standard"))


  ox <- x <- as.numeric(1:4)

  units(x) <- "m/s"
  expect_s3_class(x, "units")

  x <- drop_units(x)
  expect_identical(x, ox)
  expect_error(drop_units(x))


  ox <- x <- data.frame(x=ox, y=ox)

  units(x[[1]]) <- "m/s"
  expect_s3_class(x[[1]], "units")

  x <- drop_units(x)
  expect_identical(x, ox)


  meter <- units:::symbolic_unit("m")

  expect_identical(meter, make_units(m))
  expect_identical(meter, as_units("m"))
  expect_identical(meter, as_units(quote(m)))
  expect_identical(meter, as_units(expression(m)))

  meter_per_sec <- meter/units:::symbolic_unit("s")

  expect_identical(meter_per_sec, make_units(m/s))
  expect_identical(meter_per_sec, as_units("m/s"))
  expect_identical(meter_per_sec, as_units(" m / s "))
  expect_identical(meter_per_sec, as_units(quote(m/s)))
  expect_identical(meter_per_sec, as_units(expression(m/s)))
  expect_identical(meter_per_sec, as_units("m s-1"))

})

test_that("unitless objects can be created", {
  unit <- 1.0
  units(unit) <-  units:::unitless

  xul <- x <- as.numeric(2:5)
  units(xul) <-  units:::unitless


  expect_identical(unit, make_units())
  expect_identical(unit, make_units(1))

  expect_identical(unit, as_units(1))
  expect_identical(unit, as_units("1"))

  expect_identical(xul, set_units(x, 1))
  expect_identical(xul, set_units(x))
  expect_identical(xul, set_units(x, "1"))
  expect_identical(xul, set_units(x, "1", mode = "standard"))
  expect_identical(xul, set_units(x, mode = "standard"))
})

test_that("set_units default enforces NSE", {
  expect_error(set_units(1:3, as_units("m")))
  expect_error(set_units(1:3, as_units("m/s")))
  expect_error(set_units(1:3, make_units(m)))
  expect_error(set_units(1:3, make_units(m/s)))

  # is it bad if this works?
  # expect_error(set_units(1:3, "m/s"))
})

expect_symbolic <- function(u, n, d)
  expect_equal(units(as_units(u)), units:::.symbolic_units(n, d))
expect_symbolic_nocheck <- function(u, n, d)
  expect_equal(units(as_units(u, check_is_valid=FALSE)), units:::.symbolic_units(n, d))

test_that("exotic units work", {
  # check what udunits support
  # units:::R_ut_format(units:::R_ut_parse(some_string))

  expect_symbolic("2.2 m s", c("2.2", "m", "s"), character(0))
  expect_symbolic("2.2*m*s", c("2.2", "m", "s"), character(0))
  #expect_symbolic("2.2.m.s", c("2.2", "m", "s"), character(0))

  expect_symbolic("m2/s", c("m", "m"), "s")
  expect_symbolic("m20/s", rep("m", 20), "s")
  expect_symbolic("m^2/s", c("m", "m"), "s")
  expect_symbolic("m 2/s", c("m", "2"), "s")
  expect_symbolic("m-2/s", character(0), c("m", "m", "s"))
  expect_symbolic("m^-2/s", character(0), c("m", "m", "s"))
  expect_symbolic("m/s2", "m", c("s", "s"))
  expect_symbolic("m/s^2", "m", c("s", "s"))
  expect_symbolic("m/s 2", c("m", "2"), "s")
  expect_symbolic("m/s-2", c("m", "s", "s"), character(0))
  expect_symbolic("m/s^-2", c("m", "s", "s"), character(0))

  expect_symbolic("ml/min/(1.73m^2)", "ml", c("min", "1.73", "m", "m"))
  expect_symbolic("ml/min/1.73/m^2", "ml", c("min", "1.73", "m", "m"))
  expect_symbolic("ml/min/1.73m^2", "ml", c("min", "1.73", "m", "m"))
  expect_symbolic("ml/min/1.73m-2", c("ml", "m", "m"), c("min", "1.73"))

  old <- unlist(units_options(strict_tokenizer=TRUE))
  expect_symbolic("ml/min/1.73m^2", c("ml", "m", "m"), c("min", "1.73"))
  expect_symbolic("ml/min/1.73m-2", "ml", c("min", "1.73", "m", "m"))
  units_options(strict_tokenizer=old)

  expect_symbolic_nocheck("inH2O", "inH2O", character(0))
  expect_symbolic_nocheck("inH2O2", c("inH2O", "inH2O"), character(0))

  expect_symbolic("m/(g/(s/L))", c("m", "s"), c("g", "L"))
  expect_error(expect_warning(as_units("m/(g/s")))
  expect_error(expect_warning(as_units("m^m")))
})
