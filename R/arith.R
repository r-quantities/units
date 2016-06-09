NULL
#' @import utils
#' @import stats
#' @import udunits2
NULL
#Sys.setenv(UDUNITS2_XML_PATH = "/usr/local/share/udunits/udunits2.xml")

#' Set measurement units on a numeric vector
#'
#' @param x numeric vector
#' @param value character; valid unit of measurement string
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
		stop(paste(value, "not recognized as a unit of measurement"))
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

.as.units = function(x, value) {
	x = unclass(x)
	class(x) = "units"
	attr(x, "units") = value
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

#' Title
#'
#' @param e1 object of class \code{units}, or something that can be coerced to it by \code{as.units(e1)}
#' @param e2 object of class \code{units}, or something that can be coerced to it by \code{as.units(e2)}
#'
#' @return object of class \code{units}
#' @export
#'
#' @examples
#' a = as.units(1:3, "m/s")
#' b = as.units(1:3, "m/s")
#' a + b
Ops.units <- function(e1, e2) {
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for \"units\" objects"))

    eq <- switch(.Generic, "+" = , "-" = , "==" = , "!=" = , 
		"<" = , ">" = , "<=" = , ">=" = TRUE, FALSE)

    prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)

	pw <- switch(.Generic, "**" = , "^" = TRUE, FALSE)

	if (! eq && ! prd && !pw)
		stop(paste("operation", .Generic, "not allowed"))

	if (eq)
		units(e2) = units(e1) 

	if (prd && inherits(e1, "units") && inherits(e2, "units")) {
		attr(e1, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
		attr(e2, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
	}

	if (pw) {
		if (inherits(e2, "units") || length(e2) > 1L)
			stop("power operation only allowed with length-one numeric power")
		attr(e1, "units") = paste0("(", units(e1), ")", .Generic, e2)
	}
    NextMethod(.Generic)
}

#' Mathematical operations for units objects
#'
#' @param x object of class units
#' @param ... parameters passed on to the Math functions
#' 
#' @export
#' 
#' @examples
#' a = sqrt(1:3)
#' units(a) = "m/s"
#' log(a)
#' cumsum(a)
#' signif(a, 2)
Math.units = function(x, ...) {
    OK <- switch(.Generic, "abs" = , "sign" = , "floor" = , "ceiling" = , 
		"trunc" = , "round" = , "signif" = , "cumsum" = , "cummax" = , "cummin" = TRUE, FALSE)
	if (!OK) {
		warning(paste("Operation", .Generic, "not meaningful for units"))
		x = unclass(x)
		attr(x, "units") = NULL
    	NextMethod(.Generic)
	} else
    	.as.units(NextMethod(.Generic), units(x))
}

#' @export
Summary.units = function(..., na.rm = FALSE) {
	OK <- switch(.Generic, "sum" = , "min" = , "max" = , "range" = TRUE, FALSE)
	if (!OK)
		stop(paste("Summary operation", .Generic, "not allowed"))
	# NextMethod(.Generic)
	args = list(...)
	u = units(args[[1]])
	if (length(args) > 1)
		for (i in 2:length(args)) {
			if (!inherits(args[[i]], "units"))
				stop(paste("argument", i, "is not of class units"))
			if (!ud.are.convertible(units(args[[i]]), u))
				stop(paste("argument", i, "has units that are not convertible to that of the first argument"))
			args[[i]] = as.units(args[[i]], u) # convert to first unit
		}
	args = lapply(args, unclass)
	as.units(do.call(.Generic, args), u)
}

#' @export
print.units <- function (x, digits = getOption("digits"), ...) 
{
    if (is.array(x) || length(x) > 1L) {
        cat("Units: ", attr(x, "units"), "\n", sep = "")
        y <- unclass(x)
        attr(y, "units") <- NULL
        print(y)
    }
    else cat(format(unclass(x), digits = digits), " ", attr(x, "units"), "\n", sep = "")
    invisible(x)
}

#' @export
`[.units` <- function(x, i, j,..., drop = TRUE) {
	ret = unclass(x)[i]
	attr(ret, "units") = units(x)
	class(ret) = "units"
	ret
}

#' @export
weighted.mean.units <- function (x, w, ...) 
	structure(weighted.mean(unclass(x), w, ...), units = attr(x, 
   		 "units"), class = "units")

#' @export
c.units <- function (..., recursive = FALSE) {
    args <- list(...)
	u = units(args[[1]])
	if (length(args) > 1)
		for (i in 2:length(args)) {
			if (!inherits(args[[i]], "units"))
				stop(paste("argument", i, "is not of class units"))
			if (!ud.are.convertible(units(args[[i]]), u))
				stop(paste("argument", i, "has units that are not convertible to that of the first argument"))
			units(args[[i]]) = u
		}
	x = unlist(args)
	as.units(x, u)
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
mean.units = function(x, ...) {
	.as.units(mean(unclass(x), ...), units(x))
}

#' @export
median.units = function(x, na.rm = FALSE) {
	.as.units(median(unclass(x), na.rm = na.rm), units(x))
}

#' quantile method for object of class units
#'
#' @param x object of class \code{units}
#' @param ... arguments passed on to quantile
#'
#' @export
quantile = function(x, ...) {
	.as.units(quantile(unclass(x), ...), units(x))
}
