do.call(units_options, units:::.default_options)

test_that("base plots work as expected", {
  skip_if_not_installed("vdiffr")

  displacement = mtcars$disp * as_units("in")^3
  units(displacement) = make_units(cm^3)
  weight = mtcars$wt * 1000 * make_units(lb)
  units(weight) = make_units(kg)
  vdiffr::expect_doppelganger("plot default", plot(weight, displacement))

  units_options(group = c("(", ")") )  # parenthesis instead of square brackets
  vdiffr::expect_doppelganger("plot parentheses", plot(weight, displacement))

  units_options(sep = c("~~~", "~"), group = c("", ""))  # no brackets; extra space
  vdiffr::expect_doppelganger("plot nothing", plot(weight, displacement))

  units_options(sep = c("~", "~~"), group = c("[", "]"))
  gallon = as_units("gallon")
  consumption = mtcars$mpg * make_units(mi/gallon)
  units(consumption) = make_units(km/l)
  vdiffr::expect_doppelganger("plot division", plot(displacement, consumption))

  units_options(negative_power = TRUE) # division becomes ^-1
  vdiffr::expect_doppelganger("plot npower", plot(displacement, consumption))
  vdiffr::expect_doppelganger("plot inverse", plot(1/displacement, 1/consumption))

  units_options(parse = FALSE)
  n = 100
  set.seed(42)
  u = rnorm(1:n) * as_units("degree_C")
  v = rnorm(1:n) * as_units("s")
  vdiffr::expect_doppelganger("plot degree_C", plot(u, v))

  vdiffr::expect_doppelganger("plot line", plot(u, type = 'l'))
  vdiffr::expect_doppelganger("plot hist", hist(u))
  vdiffr::expect_doppelganger("plot boxplot", boxplot(u))
})

test_that("ggplot2 plots work as expected", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("ggplot2")
  library(ggplot2)

  iris.u <- iris
  iris.u[1:4] <- lapply(iris.u[1:4], function(x) set_units(x, cm))

  p0 <- ggplot(iris.u) + aes(Sepal.Length, Sepal.Width, color=Species) +
    geom_point() + theme_bw() + theme(legend.position=c(0.6, 0.8))

  p1 <- p0 + scale_x_units(unit="m") + scale_y_units(unit="mm")

  vdiffr::expect_doppelganger("ggplot2 automatic", p0)
  vdiffr::expect_doppelganger("ggplot2 manual", p1)
})

do.call(units_options, units:::.default_options)
