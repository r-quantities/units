# demo illustrating the different year lengths, as available in
# NISTunits:
library(NISTunits)
options(digits=7)
NISTyearTOsec(1) / (24 * 3600) # number of mean solar days days in 1 common Gregorian year
NISTyearTropicalTOsec(1) / (24 * 3600) # number of mean solar days days in 1 tropical year
NISTyearSiderealTOsec(1) / (24 * 3600) # number of mean solar days days in 1 sidereal year
NISTyearTropicalTOsec(1) / (24 * 3600) # number of mean solar days days in 1 tropical year
NISTyearSiderealTOsec(1) / (24 * 3600) # number of mean solar days days in 1 sidereal year

# ... and in udunits2:
library(units)
as.units(make_unit("common_year"), with(ud_units, d))
as.units(make_unit("leap_year"), with(ud_units, d)) 
as.units(make_unit("Gregorian_year"), with(ud_units, d))
as.units(make_unit("Julian_year"), with(ud_units, d))
as.units(with(ud_units, yr), with(ud_units, d)) # yr = tropical:
