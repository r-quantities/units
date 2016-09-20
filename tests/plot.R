library(units)
n = 100
u = rnorm(1:n) * make_unit("°C")
v = rnorm(1:n) * make_unit("s")
plot(u, v)
plot(u, type = 'l')
hist(u)
