NULL
#' @import utils
#' @import stats
#' @import udunits2
NULL
#Sys.setenv(UDUNITS2_XML_PATH = "/usr/local/share/udunits/udunits2.xml")

#' Set measurement units on a numeric vector
#'
#' @param x numeric vector
#' @param value character; unit of measurement string
#'
#' @return object of class \code{units}
#' @export
#'
#' @examples
#' x = 1:3
#' class(x)
#' units(x) = "m/s" # valid
#' class(x)
#' y = 2:5
#' try(units(y) <- "xxy") # error
`units<-.numeric` = function(x, value) {
  stopifnot(is.character(value))
  if (!ud.is.parseable(value))
    stop(paste(value, "not recognized as a udunits2 unit of measurement"))
  attr(x, "units") = value
  class(x) = "units"
  x
}

#' Convert units
#' 
#' @param x object of class \code{units}
#' @param value length one character vector with target unit
#' 
#' @name units
#' @export
#' 
#' @examples
#' a = as.units(1:3, "m/s")
#' units(a) = "km/h"
#' a
`units<-.units` = function(x, value) {
  stopifnot(is.character(value))
  if (!ud.is.parseable(value))
    stop(paste(value, "not recognized as a unit of measurement"))
  if (units(x) == value) # do nothing:
    return(x)
  if (!ud.are.convertible(units(x), value))
    stop(paste("cannot convert", units(x), "into", value))
  x = ud.convert(x, units(x), value)
  attr(x, "units") = value
  x
}

#' retrieve measurement units from units object
#'
#' @param x object of class \code{units}
#'
#' @export
units.units = function(x) {
  attr(x, "units")
}

#' convert object to a units object
#'
#' @param x object of class units
#' @param value target unit, defaults to '1'
#'
#' @export
as.units = function(x, value = "1") {
  UseMethod("as.units")
}

#' @export
as.units.default = function(x, value = "1") {
  units(x) = value
  x
}

#' convert difftime objects to units
#'
#' @param x object of class units
#' @param value target unit; if omitted, taken from \code{x}
#'
#' @export
#' 
#' @examples
#' s = Sys.time()
#' d  = s - (s+1)
#' as.units(d)
as.units.difftime = function(x, value) {
  u = attr(x, "units")
  x = unclass(x)
  attr(x, "units") = NULL
  # convert from difftime to udunits2:
  if (u == "secs") # secs -> s
    units(x) = "s"
  else if (u == "mins") # mins -> min
    units(x) = "min"
  else if (u == "hours") # hours -> h
    units(x) = "h"
  else if (u == "days") # days -> d
    units(x) = "d"
  else if (u == "weeks") { # weeks -> 7 days
    x = 7 * x
    units(x) = "d"
  } else 
    stop(paste("unknown time units", u, "in difftime object"))
  if (!missing(value)) # convert optionally:
    units(x) = value
  x
}

#' @export
as.data.frame.units = as.data.frame.numeric

#' convert units object into difftime object
#'
#' @param x object of class \code{units}
#'
#' @export
#' 
#' @examples
#' 
#' t1 = Sys.time() 
#' t2 = t1 + 3600 
#' d = t2 - t1
#' as.units(d)
#' (du = as.units(d, "d"))
#' dt = as.dt(du)
#' class(dt)
#' dt
as.dt = function(x) {
  stopifnot(inherits(x, "units"))
  u = units(x)
  if (u == "s")
    as.difftime(x, units = "secs")
  else if (u == "m")
    as.difftime(x, units = "mins")
  else if (u == "h")
    as.difftime(x, units = "hours")
  else if (u == "d")
    as.difftime(x, units = "days")
  else
    stop(paste("cannot convert unit", u, "to difftime object"))
}


#' @export
`[.units` <- function(x, i, j,..., drop = TRUE) {
  ret = unclass(x)[i]
  attr(ret, "units") = units(x)
  class(ret) = "units"
  ret
}
