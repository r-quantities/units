NULL
#' @import utils
#' @import stats
#' @importFrom methods is
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

    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)

    eq <- switch(.Generic, "+" = , "-" = , "==" = , "!=" = , 
		"<" = , ">" = , "<=" = , ">=" = TRUE, FALSE)

    prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)

	pw <- switch(.Generic, "**" = , "^" = TRUE, FALSE)

	if (! eq && ! prd && !pw)
		stop(paste("operation", .Generic, "not allowed"))

#    if (!boolean)
#        stop(gettextf("'%s' not defined for \"units\" objects", .Generic),
#             domain = NA)
	if (eq)
		units(e2) = units(e1) 

	if (prd && is(e1, "units") && is(e2, "units")) {
		attr(e1, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
		attr(e2, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
	}

	if (pw) {
		if (is(e2, "units") || length(e2) > 1L)
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
Math.units = function(x,...) {
    OK <- switch(.Generic, "abs" = , "sign" = , "floor" = , "ceiling" = , 
		"trunc" = , "round" = , "signif" = , "cumsum" = , "cummax" = , "cummin" = TRUE, FALSE)
	if (!OK) {
		warning(paste("Operation", .Generic, "not meaningful for units"))
		x = unclass(x)
		attr(x, "units") = NULL
	}
    NextMethod(.Generic)
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
