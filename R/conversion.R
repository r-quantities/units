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
  stopifnot(inherits(value, "units") || inherits(value, "symbolic_units"))
  
  if (inherits(value, "units"))
    value <- units(value)
  
  attr(x, "units") = value
  class(x) <- "units"
  x
}

#' @export
`units<-.data.frame` <- function(x, value) {
  if(inherits(value,'units')) {
    df <- x[lapply(x,class) %in% c('logical','numeric','units')]
    value <- setNames(replicate(length(names(df)),value,simplify=FALSE),
                      names(df))
  }
  if(!is.null(names(value)) && length(value) <= length(x)) {
    if(!all(names(value) %in% names(x))) {
      stop('Names specified that are not in data frame.')
    }
    for(n in names(x)) {
      if(!is.null(value[[n]])) {
        x[[n]] <- as_units(x[[n]],value[[n]])
      }
    }
  } else if(length(value) != length(x)) {
      stop(paste(length(value),'units given but data frame has a different number of columns'))
  } else {
    for(i in 1:length(value)) {
      x[[i]] <- as_units(x[[i]],value[[i]])
    }
  }
  x
}

#' @export
`units<-.logical` <- function(x, value) {
  if(all(is.na(x))) {
    c <- match.call()
    c[1] <- call('units<-.numeric')
    c[['x']] <- as.numeric(x)
    eval(c)
  } else {
    stop("x must be numeric, non-NA logical not supported")
  }
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
  stopifnot(inherits(value, "units") || inherits(value, "symbolic_units"))
  
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

#' @name units
#' @export
#' @param ... ignored
#' @details \code{set_units} is a pipe-friendly version of \code{units<-} that evaluates \code{value} first in the environment of \link{ud_units}.
#' @examples
#' # note that these units have NOT been defined or declared before:
#' set_units(1:5, N/m^2)
#' set_units(1:5, unitless) # unit "1", unitless
#' if (require(magrittr)) {
#'  1:5 %>% set_units(N/m^2)
#'  1:10 %>% set_units(m) %>% set_units(km)
#' }
set_units = function(x, value, ...) UseMethod("set_units")

#' @export
set_units.units = function(x, value, ...) {
  e = try(u <- eval(substitute(value), ud_units, parent.frame()), silent = TRUE)
  if (inherits(e, "try-error") || ! (inherits(u, "units") 
        || inherits(u, "symbolic_units"))) {
	val_char = gsub("\"", "", deparse(substitute(value)))
	u = if (val_char %in% names(ud_units))
  	  ud_units[[ val_char ]]
	else if (ud.is.parseable(val_char)) {
      if (ud.get.symbol(val_char) != "")
        val_char = ud.get.symbol(val_char)
      make_unit(val_char)
	} else if (ud.is.parseable(eval(value)))
	  make_unit(eval(value))
  }
  units(x) = u
  x
}

#' @export
set_units.numeric = function(x, value = units::unitless, ...) {
  e = try(u <- eval(substitute(value), ud_units, parent.frame()), silent = TRUE)
  if (inherits(e, "try-error") || ! (inherits(u, "units") 
          || inherits(u, "symbolic_units"))) {
	val_char = gsub("\"", "", deparse(substitute(value)))
	u = if (val_char %in% names(ud_units))
  	  ud_units[[ val_char ]]
	else if (ud.is.parseable(val_char)) {
      if (ud.get.symbol(val_char) != "")
        val_char = ud.get.symbol(val_char)
      make_unit(val_char)
	} else if (ud.is.parseable(eval(value)))
	  make_unit(eval(value))
  }
  if (inherits(u, "units"))
    x * u
  else {
    units(x) = u
    x
  }
}

#' @export
set_units.logical <- function(x, value = units::unitless, ...) {
  if (all(is.na(x))) {
    c <- match.call()
    c[1] <- call('set_units.numeric')
    c[['x']] <- as.numeric(x)
    eval(c)
  } else {
    stop("x must be numeric, non-NA logical not supported")
  }
}

#' @export
set_units.data.frame <- function(x, value = units::unitless, ...) {
  value <- eval(substitute(value), envir=ud_units, enclos=parent.frame())
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
units.data.frame <- function(x) {
  lapply(x[lapply(x,class) == 'units'],units)
}

#' @export
units.list <- units.data.frame

#' convert object to a units object
#'
#' @param x object of class units
#' @param value target unit, defaults to `unitless`
#'
#' @export
as_units <- function(x, value = unitless) {
  UseMethod("as_units")
}

#' @export
#' @name as_units
as.units <- function(x, value = unitless) {
	.Deprecated("as_units")    # nocov
	as_units(x, value = value) # nocov
}

#' @export
#' @name as_units
as_units.default <- function(x, value = unitless) {

  unit_name <- substitute(value)
  if (is.symbol(unit_name)) {
    unit_name <- as.character(unit_name)
    if (!exists(unit_name, envir = parent.frame())) {
      value <- units:: ud_units[[unit_name]]
      if (is.null(value))
        stop(paste("unit", unit_name, "not found: define with make_unit?"))
    }
  }
  
  units(x) <- value
  x
}

#' convert difftime objects to units
#'
#' @export
#' @name as_units
#' 
#' @examples
#' s = Sys.time()
#' d  = s - (s+1)
#' as_units(d)
as_units.difftime <- function(x, value) {
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
	u1 = set_units(x, "seconds since 1970-01-01 00:00:00 +00:00")
	as.POSIXct.numeric(as.numeric(u1), tz = tz, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
}

#' @method as.Date units
#' @export
as.Date.units = function (x, ...) {
	u1 = set_units(x, "days since 1970-01-01")
	as.Date(as.numeric(u1), origin = as.Date("1970-01-01 00:00:00"))
}

#' @export
as_units.POSIXt = function(x, value) {
	u = set_units(as.numeric(as.POSIXct(x)), "seconds since 1970-01-01 00:00:00 +00:00")
	if (missing(value))
		u
	else
		set_units(u, value)
}

#' @export
as_units.Date = function(x, value) {
	u = set_units(as.numeric(x), "days since 1970-01-01")
	if (missing(value))
		u
	else
		set_units(u, value)
}
