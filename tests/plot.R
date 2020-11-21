library(units)
oldpar = par(mar = par("mar") + c(0, .3, 0, 0))
displacement = mtcars$disp * as_units("in")^3
units(displacement) = make_units(cm^3)
weight = mtcars$wt * 1000 * make_units(lb)
units(weight) = make_units(kg)
plot(weight, displacement)
units_options(group = c("(", ")") )  # parenthesis instead of square brackets
plot(weight, displacement)
units_options(sep = c("~~~", "~"), group = c("", ""))  # no brackets; extra space
plot(weight, displacement)
units_options(sep = c("~", "~~"), group = c("[", "]"))
gallon = as_units("gallon")
consumption = mtcars$mpg * make_units(mi/gallon)
units(consumption) = make_units(km/l)
plot(displacement, consumption) # division in consumption
units_options(negative_power = TRUE) # division becomes ^-1
plot(displacement, consumption)
plot(1/displacement, 1/consumption)
par(oldpar)

units_options(parse = FALSE)
n = 100
u = rnorm(1:n) * as_units("degree_C")
v = rnorm(1:n) * as_units("s")
plot(u, v)
plot(u, type = 'l')
hist(u)
boxplot(u)

do.call(units_options, units:::.default_options)
