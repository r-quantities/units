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
#' units(x) <- with(ud_units, m/s) # valid
#' class(x)
#' y = 2:5
`units<-.numeric` <- function(x, value) {
  stopifnot(inherits(value, "units") || inherits(value, "symbolic_units"))
  
  if (inherits(value, "units"))
    value <- units(value)
  
  attr(x, "units") = value
  class(x) <- "units"
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
#' a <- with(ud_units, 1:3 * m/s)
#' units(a) <- with(ud_units, km/h)
#' a
`units<-.units` <- function(x, value) {
  stopifnot(inherits(value, "units") || inherits(value, "symbolic_units"))
  
  if (inherits(value, "units"))
    value <- units(value)
  
  if (identical(units(x), value)) # do nothing:
    return(x)
  
  str1 <- as.character(units(x))
  str2 <- as.character(value)
  if (udunits2::ud.are.convertible(str1, str2)) 
    structure(udunits2::ud.convert(x, str1, str2), units = value)
  else
    stop(paste("cannot convert", units(x), "into", value))

#  # We need to convert from one unit to another
#  conversion_constant <- .get_conversion_constant(units(x), value)
#  if (is.na(conversion_constant)) {
#    stop(paste("cannot convert", units(x), "into", value))
#  }
#  
#  structure(conversion_constant * x, units = value)
}

#' retrieve measurement units from units object
#'
#' @param x object of class \code{units}
#'
#' @export
units.units <- function(x) {
  attr(x, "units")
}

#' convert object to a units object
#'
#' @param x object of class units
#' @param value target unit, defaults to `unitless`
#'
#' @export
as.units <- function(x, value = unitless) {
  UseMethod("as.units")
}

#' @export
as.units.default <- function(x, value = unitless) {
#  value.name = as.character(substitute(value))
#  tr = try(ret <- get(value.name), silent = TRUE)
#  if (inherits(tr, "try-error"))
#  	value = with(ud_units, value)
#  else
#  	value = ret
#  if (is.null(value))
#  	stop(paste("unit", value.name, "not found: define with make_unit?"))
  units(x) <- value
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
as.units.difftime <- function(x, value) {
  u <- attr(x, "units")
  x <- unclass(x)
  attr(x, "units") <- NULL
  
  # convert from difftime to udunits2:
  if (u == "secs") # secs -> s
    x <- x * make_unit("s")
  else if (u == "mins") # mins -> min
    x <- x * make_unit("min")
  else if (u == "hours") # hours -> h
    x <- x * make_unit("h")
  else if (u == "days") # days -> d
    x <- x * make_unit("d")
  else if (u == "weeks") { # weeks -> 7 days
    x <- 7 * x
    x <- x * make_unit("d")
  } else 
    stop(paste("unknown time units", u, "in difftime object"))
  
  if (!missing(value)) # convert optionally:
    units(x) <- value
  
  x
}

#' @export
as.data.frame.units <- as.data.frame.numeric

#' convert units object into difftime object
#'
#' @param x object of class \code{units}
#'
#' @export
#' @details \link{as.difftime} is not a generic, hence this strange name.
#' @examples
#' 
#' t1 = Sys.time() 
#' t2 = t1 + 3600 
#' d = t2 - t1
#' du <- as.units(d)
#' dt = as.dt(du)
#' class(dt)
#' dt
as.dt <- function(x) {
  stopifnot(inherits(x, "units"))
  u <- as.character(units(x))
  if (u == "s")
    as.difftime(x, units = "secs")
  else if (u == "min")
    as.difftime(x, units = "mins")
  else if (u == "h")
    as.difftime(x, units = "hours")
  else if (u == "d")
    as.difftime(x, units = "days")
  else
    stop(paste("cannot convert unit", u, "to difftime object"))
}

# #' Convert units to hms
# #'
# #' Convert units to hms
# #' @param x object of class units
# #' @param ... passed on to as.hms.difftime
# #' @return object of class hms
# #' @examples
# #' if (require(hms)) {
# #'  as.hms(1:10 * with(ud_units, s))
# #'  as.hms(1:10 * with(ud_units, min))
# #'  as.hms(1:10 * with(ud_units, h))
# #'  as.hms(1:10 * with(ud_units, d))
# #' }
# #' @export
# as.hms.units = function(x, ...) {
# 	hms::as.hms(as.dt(x), ...)
# }


#' @export
`[.units` <- function(x, i, j,..., drop = TRUE)
  structure(NextMethod(), "units" = units(x), class = "units")
