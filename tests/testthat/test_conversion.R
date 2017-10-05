context("Unit conversion")

test_that("we can convert numbers to unit-less units", {
  x <- as_units(1:4)
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.numeric(x), equals(1:4))
  
  y <- 1:4
  units(y) <- unitless
  expect_equal(x, y)
})

test_that("we can convert numbers to physical units", {
  m <- make_unit("m")
  x <- 1:4 * m
  expect_that(length(x), equals(4))
  expect_that(class(x), equals("units"))
  expect_that(as.character(units(x)), equals("m"))
  expect_equal(as.numeric(x), 1:4)
  
  y <- 1:4
  units(y) <- m
  expect_equal(x, y)
  
  z <- 1:4 / m
  expect_that(length(z), equals(4))
  expect_that(class(z), equals("units"))
  expect_that(as.character(units(z)), equals("1/m"))
  expect_equal(as.numeric(z), 1:4)
})

test_that("we can convert NA values to physical units", {
  m <- make_unit("m")
  x <- NA * m
  expect_equal(class(x), "units")
  expect_equal(as.character(units(x)), "m")
  expect_equal(as.numeric(x), as.numeric(NA))

  x <- set_units(NA,m/s)
  expect_equal(as.character(units(x)), "m/s")
  expect_equal(x + set_units(5,m/s), set_units(NA,m/s))
  expect_error(x + set_units(5,m))

  x <- NA
  units(x) <- m
  expect_that(as.character(units(x)), equals('m'))

  x <- rep(NA,5)
  units(x) <- with(ud_units,m/s)
  expect_equal(length(x),5)
  expect_equal(units(x),units(with(ud_units,m/s)))
  expect_equal(x,5 * x)
  expect_error(x + 1)
})

test_that("we can convert between two units that can be converted", {
  m <- make_unit("m")
  km <- make_unit("km")
  x <- y <- 1:4 * m
  units(x) <- km
  expect_equal(as.numeric(y), 1000 * as.numeric(x))
  library(magrittr)
  y %>% set_units(km) -> z
  expect_equal(x, z)
})

test_that("we can't convert between two units that can't be converted", {
  m <- make_unit("m")
  s <- make_unit("s")
  expect_error(units(m) <- s)
})

test_that("we can convert difftime objects to units", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as_units(d)
  expect_equal(as.numeric(x), as.numeric(d))
  
  week <- as.difftime(1, units = "weeks")
  units_week <- as_units(week)
  expect_equal(as.character(units(units_week)), "d")
  expect_equal(as.numeric(units_week), 7)
})

test_that("we can convert units objects to difftime objects", {
  s <- Sys.time()
  d <- s - (s + 1)
  x <- as_units(d)
  y <- as_difftime(x)
  
  expect_equal(d, y)
})

#test_that("we can convert units objects to and from hms objects", {
#  s <- Sys.time()
#  library(hms)
#  d <- as.hms(s - (s + 1))
#  x <- as_units(d)
#  y <- as.hms(x)
#  
#  expect_equal(d, y)
#})

test_that("we can subscript units", {
  x <- 1:4
  y <- x * make_unit("m")
  expect_equal(as.numeric(y[1]), x[1])
  expect_equal(class(y[1]), class(y))
})

test_that("m + m*s is an error", {
  m <- make_unit("m")
  s <- make_unit("s")
  expect_error(m + m * s)
})

test_that("we can convert between units that are not simply a scalar from each other", {
  m <- 0 * parse_unit("degC")
  units(m) <- parse_unit("degK")
  expect_equal(as.numeric(m), udunits2::ud.convert(0, "degC", "degK"))  
  expect_equal(as.character(units(m)), "degK")
  
  temp <- 75 * parse_unit('degF')
  units(temp) <- parse_unit('degK')
  result <- temp / parse_unit('degF')
  expect_equal(as.numeric(result), 75)
  expect_equal(units(result), unitless)
})

test_that("we can add units to data frames", {
  df <- data.frame(a=c(1,2,3),
                   b=c(4,5,6),
                   row.names=c('x','y','z'))

  r1 <- data.frame(a=set_units(c(1,2,3),m/s),
                   b=set_units(c(4,5,6),m/s),
                   row.names=c('x','y','z'))

  r2 <- data.frame(a=set_units(c(1,2,3),m/s),
                   b=c(4,5,6),
                   row.names=c('x','y','z'))

  r3 <- data.frame(a=c(1,2,3),
                   b=set_units(c(4,5,6),m/s),
                   row.names=c('x','y','z'))

  r4 <- data.frame(a=set_units(c(1,2,3),km),
                   b=set_units(c(4,5,6),m/s),
                   row.names=c('x','y','z'))

  expect_equal(r1, set_units(df, m/s))
  expect_equal(r2, set_units(df, list(a=m/s)))
  expect_equal(r3, set_units(df, list(b=m/s)))
  expect_equal(r1, set_units(df, list(a=m/s,b=m/s)))
  expect_equal(r1, set_units(df, list(m/s,m/s)))
  expect_equal(r4, set_units(df, list(a=km,b=m/s)))
  expect_equal(r4, set_units(df, list(b=m/s,a=km)))
  expect_equal(r4, set_units(df, list(km,m/s)))
  expect_error(set_units(df, list(m/s)))
  expect_error(set_units(df, list(m,hr,e)))
  expect_error(set_units(df, list(x=m)))
  expect_error(set_units(df, list(a=m,b=hr,c=e)))

  df <- data.frame(f=c('one','two','three'),
                   a=as.logical(c(NA,NA,NA)),
                   b=as.numeric(c(1,2,3)),
                   c=set_units(c(4,5,6),km))

  r5 <- data.frame(f=c('one','two','three'),
                   a=set_units(c(NA,NA,NA),m),
                   b=set_units(c(1,2,3),m),
                   c=set_units(c(4000,5000,6000),m))

  r6 <- data.frame(f=c('one','two','three'),
                   a=as.logical(c(NA,NA,NA)),
                   b=set_units(c(1,2,3),m/hr),
                   c=set_units(c(4,5,6),km))

  expect_equal(r5, set_units(df,m))
  expect_equal(r5, set_units(df,units(r5)))
  expect_equal(r6, set_units(df,list(b=m/hr)))
  expect_equal(r6, set_units(df,units(r6)))

  df2 <- df
  units(df2) <- with(ud_units,m)
  expect_equal(df2,r5)

  df2 <- df
  units(df2) <- with(ud_units,list(a=m,b=m,c=m))
  expect_equal(df2,r5)

  df2 <- df
  units(df2) <- with(ud_units,list(b=m/hr,c=km))
  expect_equal(df2,r6)

  df2 <- df
  units(df2) <- units(r6)
  expect_equal(df2,r6)

  df2 <- df
  units(df2[,c('b','c')]) <- with(ud_units,list(m/hr,km))
  expect_equal(df2,r6)
})
