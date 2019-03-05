library(testthat)
library(units)

test_check("units")
do.call(units_options, units:::.default_options)
