# units 
Measurement Units for R Vectors

[![Build Status](https://travis-ci.org/edzer/units.svg?branch=master)](https://travis-ci.org/edzer/units) 
<!-- SET UP CODECOV AND UNCOMMENT TO GET COVERAGE BADGE [![Coverage Status](https://img.shields.io/codecov/c/github/edzer/units/master.svg)](https://codecov.io/github/edzer/units?branch=master) -->
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html) [![CRAN](http://www.r-pkg.org/badges/version/units)](https://cran.rstudio.com/web/packages/units/index.html) 
[![Downloads](http://cranlogs.r-pkg.org/badges/units?color=brightgreen)](http://www.r-pkg.org/pkg/units)

See:

* [blog post](http://r-spatial.org/r/2016/06/10/units.html)
* [package vignette](https://cran.r-project.org/web/packages/units/vignettes/units.html)
* the [udunits2 R package](https://github.com/pacificclimate/Rudunits2) github page

To cope with bug in udunits2 0.8.1 on Win & Mac, for getting udunits2
to work you may have to set the following _before loading units or udunits2_:

```
units_file = system.file("share/udunits2.xml", package="udunits2")
Sys.setenv(UDUNITS2_XML_PATH = units_file)
```
