context("unit(s) creation")



test_that("parse_units() backticks strings correctly", {
  
  x <- matrix(ncol = 2, byrow = TRUE, c(
    "in",      "`in`",         
    "`in`",    "`in`",
    "kelvin",  "`kelvin`",
    "%",       "`%`",
    "T",       "`T`",
    "'/s",     "`'`/`s`",
    "'",       "`'`" ,
    '"',       '`"`' ,
    '"/s' ,    '`"`/`s`',
    "s/'" ,    "`s`/`'`"  ,
    "C" ,      "`C`" ,
    "F" ,      "`F`",
    "\u00B0C", "`\u00B0C`",
    "\u2103" , "`\u2103`",
    "\u00B0F", "`\u00B0F`",
    "\u2109",  "`\u2109`",
    "log(ug)", "`log`(`ug`)",
    "log(ug/l)", "`log`(`ug`/`l`)",
    "kg*m/s^2", "`kg`*`m`/`s`^2"
  ))
  colnames(x) <- c("input", "expected_output")

  expect_identical(units:::backtick(x[,"input"]), x[,"expected_output"])
})

test_that("explicit exponents identified correctly", {
  expect_true( are_exponents_implicit("m s") )
  expect_true( are_exponents_implicit("m2") )
  expect_true( are_exponents_implicit("m-2") )
  expect_true( are_exponents_implicit("2 m") )
  expect_true( are_exponents_implicit("m s-2") )
  expect_true( are_exponents_implicit("m s-2 kg") )
  expect_true( are_exponents_implicit("2 m s") )
  
  expect_false( are_exponents_implicit("m") )
  expect_false( are_exponents_implicit("m/s") )
  expect_false( are_exponents_implicit("m^2") )
  expect_false( are_exponents_implicit("m*s") )
})

test_that("global options are respected", {
  # rt: round trip
  rt <- function(x) as.character(units(as_units(x)))
  
  expect_equal("g", rt("grams"))
  
  op <- options(units.auto_convert_names_to_symbols = FALSE)
  expect_equal("grams", rt("grams"))
  
  options(units.auto_convert_names_to_symbols = TRUE)
  expect_equal("g", rt("grams"))
  
  options(op)
  expect_equal("g", rt("grams"))
  
})


test_that("various forms of unit creation and destruction work", {

  ox <- x <- 1L:4L
  
  units(x) <- "m/s"
  expect_s3_class(x,"units")
  
  units(x) <- NULL
  expect_identical(x, ox)
  
  
  ox <- x <- matrix(1L:4L, ncol = 2)
  
  units(x) <- "m/s"
  expect_s3_class(x,"units")
  
  units(x) <- NULL
  expect_identical(x, ox)
  
  
  ox <- y <- x <- 1L:4L
  units(x) <- "m/s"
  
  expect_identical(x, set_units(y, m/s))
  expect_identical(x, set_units(y, "m/s", mode = "standard"))
  
  expect_identical(ox, set_units(x, NULL))
  expect_identical(ox, set_units(x, NULL, mode = "standard"))
  
  
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
  unit <- 1
  units(unit) <-  units:::unitless
  
  xul <- x <- 2:5
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