# <img src="https://avatars1.githubusercontent.com/u/32303769?s=40&v=4"> Measurement Units for R

[![Build Status](https://github.com/r-quantities/units/workflows/build/badge.svg)](https://github.com/r-quantities/units/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-quantities/units/master.svg)](https://codecov.io/github/r-quantities/units?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.r-project.org/package=units) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

Support for measurement units in R vectors, matrices
and arrays: automatic propagation, conversion, derivation
and simplification of units; raising errors in case of unit
incompatibility. Compatible with the POSIXct, Date and difftime 
classes. Uses the UNIDATA udunits library and unit database for 
unit compatibility checking and conversion.

### Documentation

Documentation is provided in an R Journal publication. Cite this package as:

- Edzer Pebesma, Thomas Mailund and James Hiebert (2016). "Measurement Units in R."
  _The R Journal_, 8 (2), 486--494. 
  DOI: [10.32614/RJ-2016-061](https://doi.org/10.32614/RJ-2016-061)

The main units
[vignette](https://r-quantities.github.io/units/articles/measurement_units_in_R.html)
derives from this manuscript and is kept up to date with the package development.

- Blog posts: [first](http://r-spatial.org/r/2016/06/10/units.html),
  [second](http://r-spatial.org/r/2016/08/16/units2.html),
  [third](http://r-spatial.org/r/2016/09/29/plot_units.html).
- The [udunits2 R package](https://github.com/pacificclimate/Rudunits2) GitHub page.
- The UNIDATA [udunits2](https://github.com/Unidata/UDUNITS-2) library at GitHub.

### What it does

Package `units` provides
measurement units for R vectors: conversion, derivation, simplification and error checking:

```r
library(units)
(spd1 = set_units(1:5, m/s))
# Units: m/s
# [1] 1 2 3 4 5
(spd2 = set_units(1:5, km/h))
# Units: km/h
# [1] 1 2 3 4 5
spd1 + spd2                   # automatic conversion
# Units: m/s
# [1] 1.277778 2.555556 3.833333 5.111111 6.388889
spd1 * spd2                   # unit derivation
# Units: km*m/h/s
# [1]  1  4  9 16 25
spd1 * set_units(10, s) # unit simplification
# Units: m
# [1] 10 20 30 40 50
spd1 + set_units(10, s) # error checking
#   cannot convert s into m/s
```

### Installation

Install the release version from CRAN:

```r
install.packages("units")
```

The installation of the development version from GitHub requires, e.g., the `remotes` package:

```r
remotes::install_github("r-quantities/units")
```

If the installation fails due to a missing udunits2 system library, either install it e.g. on Ubuntu or Debian by

```
sudo apt-get install libudunits2-dev
```
on CentOS7 with
```
sudo yum install udunits2-devel
```
or on MacOS with
```
brew install udunits
```
or equivalent in your distribution. Alternatively, install it from the sources by downloading `udunits-2.2.20.tar.gz` from [ftp://ftp.unidata.ucar.edu/pub/udunits/](ftp://ftp.unidata.ucar.edu/pub/udunits/), and executing the following commands in the download directory:

```
tar zxf udunits-2.2.20.tar.gz
cd ./udunits-2.2.20/
./configure
make
sudo make install
sudo ldconfig
```

Then, retry the installation of `units`.
