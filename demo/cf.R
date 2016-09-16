library(rvest)
r = read_html(
 "http://cfconventions.org/Data/cf-standard-names/34/build/cf-standard-name-table.html")
tb = html_table(r, fill = TRUE)
library(units)
cf_units = tb[[3]][2][[1]]
u = lapply(cf_units, function(x) make_unit(x))
names(u) = substr(tb[[3]][1][[1]], 1, 50)
library(udunits2)
# does udunits2 understand them?
all(sapply(u, udunits2::ud.is.parseable))
# can parse_unit parse them? remove "W m-2 sr-1 (m-1)-1"
all(sapply(cf_units[cf_units != "W m-2 sr-1 (m-1)-1"],
	function(x) class(parse_unit(x))) == "units")
