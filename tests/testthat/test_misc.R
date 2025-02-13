test_that("We can replace parts if they have an equivalent unit", {
  x <- 1:4 * as_units("km")
  y <- c(3000, 4000) * as_units("m")
  z <- 5000 * as_units("m")
  x[3:4] <- y
  x[[5]] <- z

  expect_length(x, 5)
  expect_equal(as.numeric(x), 1:5)
  expect_equal(as.character(units(x)), "km")
})

test_that("We can't replace parts if they have different unit", {
  x <- 1:4 * as_units("km")
  y <- c(3000, 4000) * as_units("s")
  z <- 5000 * as_units("s")

  expect_error(x[3:4] <- y)
  expect_error(x[[5]] <- z)
})

test_that("We can concatenate units if they have the same unit", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("m")
  z <- c(x, y)

  expect_equal(length(z), length(x) + length(y))
  expect_equal(x, z[1:4])
  expect_equal(y, z[1:4 + 4])
})

test_that("We can't concatenate units if they have different units", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("s")
  expect_error(c(x, y))
})

test_that("We can concatenate units if their units can be converted", {
  x <- 1:4 * as_units("m")
  y <- 5:8 * as_units("km")
  z <- c(x, y, x, y)

  expect_equal(length(z), 2 * (length(x) + length(y)))
  expect_equal(as.character(units(z)), "m")
  expect_equal(x, z[1:4])
  expect_equal(x, z[1:4 + 8])
  y <- set_units(y, units(as_units("m")), mode = "standard")
  expect_equal(y, z[1:4 + 4])
  expect_equal(y, z[1:4 + 12])
})

test_that("We can use diff on a units object", {
  x = 1:10 * as_units("m")
  y = rep(1,9) * as_units("m")
  expect_equal(diff(x), y)
})

test_that("parse_unit works", {
  kg = as_units("kg")
  m = as_units("m")
  s = as_units("s")
  u0 = kg/m/m/s
  u = as_units("kg m-2 s-1", implicit_exponents = TRUE)
  expect_equal(u, u0)
  J = as_units("J")
  u0 = make_units(kg*kg*kg*m*m*J/s/s/s/s/s)
  u = as_units("kg3 m2 s-5 J", implicit_exponents = TRUE)
  expect_equal(u, u0)
})

test_that("deparse_unit works", {
  str = "kg m-2 s-1"
  u = as_units(str, implicit_exponents = TRUE)
  str0 = deparse_unit(u)
  expect_equal(str, str0)
})

test_that("set_units(x, u) is a short form for x * make_units(u)", {
  expect_equal(set_units(1:10, m/s), 1:10 * make_units(m/s))
  x = set_units(1:5, m/s)
  y = x
  units(y) = set_units(1, km/h)
  expect_identical(y, set_units(x, km/h))
})

test_that("rep.units works", {
  expect_equal(rep(set_units(1:2, m/s), 2), set_units(c(1,2,1,2), m/s))
})

test_that("set_units works with symbols in character data, and resolves names", {
  skip_on_cran()
  deg = paste0(enc2native(intToUtf8(176)), "C")
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, "degree_C", mode = "standard"))
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, "degree_Celsius", mode = "standard"))
  expect_equal(set_units(1:2, "degree_C", mode = "standard"), set_units(1:2, "degree_Celsius", mode = "standard"))
  expect_equal(set_units(1:2, degree_C),                      set_units(1:2, degree_Celsius))
  expect_equal(set_units(1:2, deg, mode = "standard"),        set_units(1:2, degree_Celsius))
  x = set_units(1:3, km)
  y <- set_units(x, "meter", mode = "standard")
  expect_equal(y, set_units(c(1000,2000,3000), m))
})

test_that("all.equal works", {
  expect_false(isTRUE(all.equal(set_units(1, m), 1)))

  expect_true(all.equal(set_units(1, m/s), set_units(3.6, km/h)))
  expect_true(all.equal(set_units(3.6, km/h), set_units(1, m/s)))

  expect_true(set_units(1, m/s) == set_units(3.6, km/h))
  expect_false(set_units(3.6, km/h) == set_units(1, m/s)) # see R FAQ 7.31
  expect_true(set_units(1e-20, g) < set_units(1e-10, g))  # see #351
})

test_that("seq works", {
  expect_equal(
    seq(to = set_units(10, m), by = set_units(1, m), length.out = 5),
    set_units(seq(to = 10, by = 1, length.out = 5), m)
  )
  expect_equal(
    seq(set_units(10, m), by = set_units(1, m), length.out = 5),
    set_units(seq(10, by = 1, length.out = 5), m)
  )
  expect_equal(
    seq(set_units(10, m), set_units(19, m)),
    set_units(seq(10, 19), m)
  )
  expect_equal(
    seq(set_units(10, m), set_units(.02, km)),
    set_units(seq(10, 20), m)
  )
})

test_that("str works", {
  expect_snapshot({
    str(set_units(1/1:3, m/s))
  })
})

test_that("subsetting keeps pillar attribute (#275)", {
  x <- set_units(1:3, m)
  attr(x, "pillar") <- "bar"

  expect_equal(attr(x[1:2], "pillar"), "bar")
  expect_equal(attr(x[[1]], "pillar"), "bar")
})

test_that("duplicated-related methods work as expected", {
  x <- set_units(1:4, m)
  expect_equal(duplicated(x), duplicated(drop_units(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_units(x)))
  expect_equal(unique(x), x)

  x <- set_units(rep(c(1, 2), 2), m)
  expect_equal(duplicated(x), duplicated(drop_units(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_units(x)))
  expect_equal(unique(x), x[1:2])

  x <- set_units(matrix(rep(c(1, 2), 2), 2, byrow=TRUE), m)
  expect_equal(duplicated(x), duplicated(drop_units(x)))
  expect_equal(anyDuplicated(x), anyDuplicated(drop_units(x)))
  expect_equal(unique(x), x[1, , drop=FALSE])
})

test_that("bind methods work properly", {
  a <- set_units(1:10, m)
  b <- set_units((1:10) * 0.001, km)

  x <- rbind(x=a, y=a)
  y <- rbind(x=a, y=b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(rownames(x), c("x", "y"))
  expect_equal(rownames(y), c("x", "y"))
  x <- rbind(rbind(a, a), a)
  y <- rbind(b, rbind(b, b))
  expect_equal(as.numeric(x), as.numeric(y) * 1000)
  expect_equal(rownames(x), c("a", "a", "a"))
  expect_equal(rownames(y), c("b", "b", "b"))

  x <- cbind(x=a, y=a)
  y <- cbind(x=a, y=b)
  expect_equal(as.numeric(x), as.numeric(y))
  expect_equal(colnames(x), c("x", "y"))
  expect_equal(colnames(y), c("x", "y"))
  x <- cbind(cbind(a, a), a)
  y <- cbind(b, cbind(b, b))
  expect_equal(as.numeric(x), as.numeric(y) * 1000)
  expect_equal(colnames(x), c("a", "a", "a"))
  expect_equal(colnames(y), c("b", "b", "b"))

  z <- cbind(
    rbind(a, b),
    rbind(x = a, y = b))
  expect_equal(dimnames(z), list(c("a", "b"), NULL))
})
