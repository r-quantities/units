# Measurement Units for R

[![Build Status](https://travis-ci.org/r-quantities/units.svg?branch=master)](https://travis-ci.org/r-quantities/units) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-quantities/units?branch=master&svg=true)](https://ci.appveyor.com/project/r-quantities/units)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-quantities/units/master.svg)](https://codecov.io/github/r-quantities/units?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.r-project.org/package=units) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

### News
Cite this package as _Edzer Pebesma, Thomas Mailund
and James Hiebert, 2016.  Measurement Units in R.
The R Journal, 8 (2), 486--494._ The citation is found
[here](http://journal.r-project.org/archive/2016-2/pebesma-mailund-hiebert.pdf).
The units vignette is derived from this manuscript, and is kept up
to date with the package.

### What it does

Package `units` provides
measurement units for R vectors: conversion, derivation, simplification and error checking:
```
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

* blog posts: [first](http://r-spatial.org/r/2016/06/10/units.html), [second](http://r-spatial.org/r/2016/08/16/units2.html), [third](http://r-spatial.org/r/2016/09/29/plot_units.html)
* [package vignette](https://cran.r-project.org/web/packages/units/vignettes/units.html)
* The [R Journal publication](https://journal.r-project.org/archive/2016-2/pebesma-mailund-hiebert.pdf), which has been [updated to recent package developments](https://r-quantities.github.io/units/articles/measurement_units_in_R.html)
* the [udunits2 R package](https://github.com/pacificclimate/Rudunits2) github page
* the UNIDATA [udunits2](https://github.com/Unidata/UDUNITS-2) library at github

When installing the `udunits2` R package fails due to a missing
udunits2 system library, either install it binary e.g. by

```
sudo apt-get install libudunits2-dev
```

or install from source by downloading `udunits-2.2.20.tar.gz` from
ftp://ftp.unidata.ucar.edu/pub/udunits/, and executing the
following commands in the download directory:
```
tar zxf udunits-2.2.20.tar.gz
cd ./udunits-2.2.20/
./configure
make
sudo make install
sudo ldconfig
```
if that went well, assuming you installed by default in `/usr/local/`, add
```
export UDUNITS2_XML_PATH="/usr/local/share/udunits/udunits2.xml"
```
to your `~/.bashrc` file, start a new shell to activate this environment variable, and try installing R package `udunits2` once more. When launching rstudio from unity, you may have to load `udunits2` explicitly before using `units`, or set `UDUNITS2_XML_PATH` in a different way such that rstudio finds it.
