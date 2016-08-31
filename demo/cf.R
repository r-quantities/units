library(rvest)
r = read_html(
 "http://cfconventions.org/Data/cf-standard-names/34/build/cf-standard-name-table.html")
tb = html_table(r, fill = TRUE)
library(units)
u = lapply(tb[[3]][2][[1]], function(x) make_unit(x))
names(u) = substr(tb[[3]][1][[1]], 1, 50)
library(udunits2)
all(sapply(u, udunits2::ud.is.parseable))
