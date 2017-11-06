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
  
    
  
})