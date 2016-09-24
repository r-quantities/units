# units 
Measurement Units for R Vectors

[![Build Status](https://travis-ci.org/edzer/units.svg?branch=master)](https://travis-ci.org/edzer/units) 
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/edzer/units?branch=master&svg=true)](https://ci.appveyor.com/project/edzer/units)
[![Coverage Status](https://img.shields.io/codecov/c/github/edzer/units/master.svg)](https://codecov.io/github/edzer/units?branch=master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.rstudio.com/web/packages/units/index.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

See:

* blog posts: [first](http://r-spatial.org/r/2016/06/10/units.html), [second](http://r-spatial.org/r/2016/08/16/units2.html)
* [package vignette](https://cran.r-project.org/web/packages/units/vignettes/units.html)
* [R Journal manuscript](https://cran.r-project.org/web/packages/units/vignettes/measurement_units_in_R.pdf) under review
* the [udunits2 R package](https://github.com/pacificclimate/Rudunits2) github page

To cope with bug in udunits2 0.8.1 on Win & Mac, for getting udunits2
to work you may have to set the following _before loading units or udunits2_:

```
units_file = system.file("share/udunits2.xml", package="udunits2")
Sys.setenv(UDUNITS2_XML_PATH = units_file)
```

When installing `udunits2` breaks due to a missing udunits
system library, download `udunits-2.2.20.tar.gz` from
ftp://ftp.unidata.ucar.edu/pub/udunits/, and carry out the
following commands:
```
tar zxf udunits-2.2.20.tar.gz
cd ./udunits-2.2.20/
./configure
make
sudo make install
sudo ldconfig
```
after that, try installing R package `udunits2` again.
