# Measurement Units for R

[![Build Status](https://travis-ci.org/edzer/units.svg?branch=master)](https://travis-ci.org/edzer/units) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/units?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/units)
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/units/master.svg)](https://codecov.io/github/edzer/units?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.r-project.org/package=units) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

### News
The `units` vignette has been [accepted](https://journal.r-project.org/archive/accepted/) for publication in the [R Journal](https://journal.r-project.org/), where it appeared [online](https://journal.r-project.org/archive/accepted/pebesma-mailund-hiebert.pdf)

Measurement Units for R Vectors: conversion, derivation, simplification and error checking:
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

* blog posts: [first](http://r-spatial.org/r/2016/06/10/units.html), [second](http://r-spatial.org/r/2016/08/16/units2.html), [third](http://r-spatial.org/r/2016/09/29/plot_units.html)
* [package vignette](https://cran.r-project.org/web/packages/units/vignettes/units.html)
* [R Journal manuscript](https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.pdf), [accepted](https://journal.r-project.org/archive/accepted/)
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
if that went well, assuming you installed by default in `/usr/local/`, add
```
export UDUNITS2_XML_PATH="/usr/local/share/udunits/udunits2.xml"
```
to your `~/.bashrc` file, start a new shell to activate this environment variable, and try installing R package `udunits2` once more. When launching rstudio from unity, you may have to load `udunits2` explicitly before using `units`.
