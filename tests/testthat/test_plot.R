do.call(units_options, units:::.default_options)

test_that("base plots work as expected", {
  skip_if_not_installed("vdiffr")
  fplot <- function(...) function() plot(...)

  displacement = mtcars$disp * as_units("in")^3
  units(displacement) = make_units(cm^3)
  weight = mtcars$wt * 1000 * make_units(lb)
  units(weight) = make_units(kg)
  vdiffr::expect_doppelganger("plot default", fplot(weight, displacement))

  xlab <- "some other thing"
  vdiffr::expect_doppelganger("plot lab", fplot(weight, displacement, xlab=xlab))

  units_options(group = c("(", ")") )  # parenthesis instead of square brackets
  vdiffr::expect_doppelganger("plot parentheses", fplot(weight, displacement))

  units_options(sep = c("~~~", "~"), group = c("", ""))  # no brackets; extra space
  vdiffr::expect_doppelganger("plot nothing", fplot(weight, displacement))

  units_options(sep = c("~", "~~"), group = c("[", "]"))
  gallon = as_units("gallon")
  consumption = mtcars$mpg * make_units(mi/gallon)
  units(consumption) = make_units(km/l)
  vdiffr::expect_doppelganger("plot division", fplot(displacement, consumption))

  units_options(negative_power = TRUE) # division becomes ^-1
  vdiffr::expect_doppelganger("plot npower", fplot(displacement, consumption))
  vdiffr::expect_doppelganger("plot inverse", fplot(1/displacement, 1/consumption))

  units_options(parse = FALSE)
  n = 100
  set.seed(42)
  u = rnorm(1:n) * as_units("degree_C")
  v = rnorm(1:n) * as_units("s")
  vdiffr::expect_doppelganger("plot degree_C", fplot(u, v))

  vdiffr::expect_doppelganger("plot line", fplot(u, type = 'l'))
  vdiffr::expect_doppelganger("plot hist", function() hist(u))
  vdiffr::expect_doppelganger("plot boxplot", function() boxplot(u))
})

test_that("ggplot2 plots work as expected", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2", "3.5.0")
  library(ggplot2)

  iris.u <- iris
  iris.u[1:4] <- lapply(iris.u[1:4], function(x) set_units(x, cm))

  p0 <- ggplot(iris.u) + aes(Sepal.Length, Sepal.Width, color=Species) +
    geom_point() + theme_bw() + theme(legend.position="inside",
                                      legend.position.inside=c(0.6, 0.8))

  p1 <- p0 + scale_x_units(unit="m") + scale_y_units(unit="mm")
  p2 <- p0 + xlab("some other thing")
  p3 <- p0 + xlab(NULL)

  vdiffr::expect_doppelganger("ggplot2 automatic", p0)
  vdiffr::expect_doppelganger("ggplot2 manual", p1)
  vdiffr::expect_doppelganger("ggplot2 lab", p2)
  vdiffr::expect_doppelganger("ggplot2 nolab", p3)
})

test_that("axis transformations do not affect displayed units", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2", "3.5.0")
  library(ggplot2)

  df <- data.frame(a = set_units(1:10, "m"))

  p0 <- ggplot(df, aes(y=a, x=a)) + geom_point()
  p1 <- p0 + scale_x_units(transform='log10') + scale_y_units(transform='sqrt')
  p2 <- p0 + scale_x_units(transform="log10", unit="mm")

  vdiffr::expect_doppelganger("ggplot2 default", p0)
  vdiffr::expect_doppelganger("ggplot2 transformed", p1)
  vdiffr::expect_doppelganger("ggplot2 trans + unit", p2)
})

test_that("axis limits can be changed", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2", "3.5.0")
  library(ggplot2)

  df <- data.frame(a = set_units(1:10, "m"))

  p0 <- ggplot(df, aes(y=a, x=a)) + geom_point()
  p1 <- p0 + scale_x_units(limits=c(0, 20))
  p2 <- p0 + scale_x_units(limits=set_units(c(0, 20), "m"))
  p3 <- p0 + xlim(set_units(c(0, 20), "m"))
  p4 <- p0 + xlim(set_units(c(0, 20), "km"))

  vdiffr::expect_doppelganger("ggplot2 limits via scale", p1)
  vdiffr::expect_doppelganger("ggplot2 limits via scale with units", p2)
  vdiffr::expect_doppelganger("ggplot2 limits via xlim", p3)
  vdiffr::expect_doppelganger("ggplot2 limits other units", p4)
})

do.call(units_options, units:::.default_options)
