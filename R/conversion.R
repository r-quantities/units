NULL
#' @import utils
#' @import stats
#' @import udunits2
NULL

# Helper functions for testing if we can convert and how using either
# user-defined conversion functions or udunits.
are_convertible <- function(from, to) {
  user_are_convertible(from, to) || udunits2::ud.are.convertible(from, to)
}

convert <- function(value, from, to) {
  value <- unclass(value)
  if (user_are_convertible(from, to)) user_convert(value, from, to)
  else if (udunits2::ud.are.convertible(from, to)) udunits2::ud.convert(value, from, to)
  else NA
}

#' Set measurement units on a numeric vector
#'
#' @param x numeric vector, or object of class \code{units}
#' @param value object of class \code{units} or \code{symbolic_units}, or in the case of \code{set_units} expression with symbols that can be resolved in \link{ud_units} (see examples).
#'
#' @return object of class \code{units}
#' @export
#' @name units
#'
#' @examples
#' x = 1:3
#' class(x)
#' units(x) <- with(ud_units, m/s) # valid
#' class(x)
#' y = 2:5
`units<-.numeric` <- function(x, value) {
  if(is.null(value))
    return(x)
 
  if(!inherits(value, "units") && !inherits(value, "symbolic_units"))
    value <- as_units(value)
  
  if (inherits(value, "units"))
    value <- units(value)
  
  attr(x, "units") = value
  class(x) <- "units"
  x
}

#' Convert units
#' 
#' @name units
#' @export
#' 
#' @examples
#' a <- with(ud_units, 1:3 * m/s)
#' units(a) <- with(ud_units, km/h)
#' a
`units<-.units` <- function(x, value) {
  
  if(is.null(value))
    return(drop_units(x))
  
  if(!inherits(value, "units") && !inherits(value, "symbolic_units"))
    value <- as_units(value)
  
  if (inherits(value, "units"))
    value <- units(value)
  
  if (identical(units(x), value)) # do nothing; possibly user-defined units:
    return(x)
  
  str1 <- as.character(units(x))
  str2 <- as.character(value)

  if (are_convertible(str1, str2)) 
    structure(convert(x, str1, str2), units = value, class = "units")
  else
    stop(paste("cannot convert", units(x), "into", value), call. = FALSE)
}

unit_ambiguous = function(value) {
  msg = paste("ambiguous argument:", value, "is interpreted by its name, not by its value")
  warning(msg, call. = FALSE)
}


#' @name units
#' @export
`units<-.logical` <- function(x, value) {
  if (!all(is.na(x))) 
    stop("x must be numeric, non-NA logical not supported")
  
  x <- as.numeric(x)
  units(x) <- value
  x
}

#' retrieve measurement units from \code{units} object
#'
#' @export
#' @name units
#' @return the units method retrieves the units attribute, which is of class \code{symbolic_units}
units.units <- function(x) {
  attr(x, "units")
}

#' @export
units.symbolic_units <- function(x) {
  x
}

#' convert object to a units object
#'
#' @param x object of class units
#' @param value an object of class units
#' @param ... passed on to other methods
#'
#' @export
as_units <- function(x, ...) {
  UseMethod("as_units")
}

#' @export
as_units.units <- function(x, value, ...) {
  if(!missing(value) && !identical(units(value), units(x)))
    warning("Use set_units() to perform unit conversion. Return unit unmodified")
  x
}
#' @export
as_units.symbolic_units <- function(x, value, ...) {
  if(!missing(value))
    warning("supplied value ignored")
  structure(1L, units = x, class = "units")
}

#' @export
#' @name as_units
as_units.default <- function(x, value = unitless, ...) {
  units(x) <- value
  x
}

#'  difftime objects to units
#'
#' @export
#' @name as_units
#' 
#' @examples
#' s = Sys.time()
#' d  = s - (s+1)
#' as_units(d)
as_units.difftime <- function(x, value, ...) {
  u <- attr(x, "units")
  x <- unclass(x)
  attr(x, "units") <- NULL
  
  # convert from difftime to udunits2:
  if (u == "secs") # secs -> s
    x <- x * symbolic_unit("s")
  else if (u == "mins") # mins -> min
    x <- x * symbolic_unit("min")
  else if (u == "hours") # hours -> h
    x <- x * symbolic_unit("h")
  else if (u == "days") # days -> d
    x <- x * symbolic_unit("d")
  else if (u == "weeks") { # weeks -> 7 days
    x <- 7 * x
    x <- x * symbolic_unit("d")
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
#' @examples
#' 
#' t1 = Sys.time() 
#' t2 = t1 + 3600 
#' d = t2 - t1
#' du <- as_units(d)
#' dt = as_difftime(du)
#' class(dt)
#' dt
as_difftime <- function(x) {
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
# 	hms::as.hms(as_difftime(x), ...)
# }


#' @export
`[.units` <- function(x, i, j,..., drop = TRUE)
  structure(NextMethod(), "units" = units(x), class = "units")

#' @export
as.POSIXct.units = function (x, tz = "UTC", ...) {
	units(x) = symbolic_unit("seconds since 1970-01-01 00:00:00 +00:00")
	as.POSIXct.numeric(as.numeric(x), tz = tz, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
}

#' @method as.Date units
#' @export
as.Date.units = function (x, ...) {
	units(x) = symbolic_unit("days since 1970-01-01")
	as.Date(as.numeric(x), origin = as.Date("1970-01-01 00:00:00"))
}

#' @export
as_units.POSIXt = function(x, value, ...) {
	u = as.numeric(as.POSIXct(x))
	units(u) = symbolic_unit("seconds since 1970-01-01 00:00:00 +00:00")
	if (! missing(value))
		units(u) = symbolic_unit(value)
	u
}

#' @export
as_units.Date = function(x, value, ...) {
	u = as.numeric(x)
	units(u) = symbolic_unit("days since 1970-01-01")
	if (!missing(value))
		units(u) = symbolic_unit(value)
	u
}
