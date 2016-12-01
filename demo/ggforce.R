# devtools::install_github("thomasp85/ggforce")
library(ggforce)
library(units)
gallon = make_unit("gallon")
mtcars$consumption = mtcars$mpg * with(ud_units, mi/gallon)
mtcars$power = mtcars$hp * with(ud_units, hp)
ggplot(mtcars) + geom_point(aes(power, consumption))
ggplot(mtcars) + geom_point(aes(1/power, 1/consumption))

# to generate figure 1 in vignette:

#pdf("fig1.pdf", width = 4, height = 4)
library(units)
units_options(negative_power = TRUE)
gallon = make_unit("gallon")
mtcars$consumption = mtcars$mpg * with(ud_units, mi/gallon)
mtcars$displacement = mtcars$disp * ud_units[["in"]]^3
units(mtcars$displacement) = with(ud_units, cm^3)
units(mtcars$consumption) = with(ud_units, km/l)
m = par("mar")
m[3] = 0.1
par(mar = m + c(0, .3, 0, 0))
with(mtcars, plot(1/displacement, 1/consumption))
#pdf("fig2.pdf", width = 4, height = 4)
library(ggforce)
ggplot(mtcars) + geom_point(aes(1/displacement, 1/consumption))
