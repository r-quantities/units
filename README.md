# units 
Measurement Units for R Vectors: conversion, derivation, simplification and error checking

[![Build Status](https://travis-ci.org/edzer/units.svg?branch=master)](https://travis-ci.org/edzer/units) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/units?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/units)
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/units/master.svg)](https://codecov.io/github/edzer/units?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.rstudio.com/web/packages/units/index.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

```
> library(units)
> (spd1 = 1:5 * with(ud_units, m/s))
Units: m/s
[1] 1 2 3 4 5
> (spd2 = 1:5 * with(ud_units, km/h))
Units: km/h
[1] 1 2 3 4 5
> spd1 + spd2                   # automatic conversion
Units: m/s
[1] 1.277778 2.555556 3.833333 5.111111 6.388889
> spd1 * spd2                   # unit derivation
Units: km*m/h/s
[1]  1  4  9 16 25
> spd1 * 10 * with(ud_units, s) # unit simplification
Units: m
[1] 10 20 30 40 50
> spd1 + 10 * with(ud_units, s) # error checking
Error in `units<-.units`(`*tmp*`, value = list(numerator = "m", denominator = "s")) : 
  cannot convert s into m/s
```

* blog posts: [first](http://r-spatial.org/r/2016/06/10/units.html), [second](http://r-spatial.org/r/2016/08/16/units2.html)
* [package vignette](https://cran.r-project.org/web/packages/units/vignettes/units.html)
* [R Journal manuscript](https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.pdf) under review
* the [udunits2 R package](https://github.com/pacificclimate/Rudunits2) github page

When installing the `udunits2` R package fails due to a missing udunits
system library, download `udunits-2.2.20.tar.gz` from
ftp://ftp.unidata.ucar.edu/pub/udunits/, and execute the
following commands in the download directory:
```
tar zxf udunits-2.2.20.tar.gz
cd ./udunits-2.2.20/
./configure
make
sudo make install
sudo ldconfig
```

The following commands can download the development version from GitHub:

```bash
wget https://github.com/Unidata/UDUNITS-2/archive/master.zip
unzip master.zip
cd UDUNITS-2-master
autoreconf -fi
./configure
make
sudo make install
udunits2 # test it works
```
if that went well, try installing R package `udunits2` once more.
