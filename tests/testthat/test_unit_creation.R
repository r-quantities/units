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

test_that("parse_units() and make_units() make identical units", {

    expect_identical(make_units(`in`),   parse_units("in"))
    expect_identical(make_units(kelvin), parse_units("kelvin"))
    expect_identical(make_units(`%`),    parse_units("%"))
    expect_identical(make_units(T),      parse_units("T"))
    expect_identical(make_units(`'`/s),  parse_units("`'`/s"))
    expect_identical(make_units(`'`),    parse_units("`'`"))
    expect_identical(make_units(s/`'`),  parse_units("s/`'`"))
    expect_identical(make_units(C),      parse_units("C"))
    expect_identical(make_units(F),      parse_units("F"))
    
  
})