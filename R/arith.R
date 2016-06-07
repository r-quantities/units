NULL
#' @import utils
#' @import stats
#' @import udunits2
NULL
#Sys.setenv(UDUNITS2_XML_PATH = "/usr/local/share/udunits/udunits2.xml")

#' Add two vectors with identical units
#'
#' @param x first vector, of class \code{units}
#' @param y second vector, of class \code{units}
#'
#' @return numeric vector of class \code{units}
#' @export 
#'
#' @examples
#' a = 1:3
#' b = 7:9
#' plus(a,b)
plus = function(x, y) { x + y }

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

#' @export
units.units = function(x) {
	attr(x, "units")
}

#' @export
as.units = function(x, value = "1") {
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
Ops.units <- function(e1, e2)
{
#    if (nargs() == 1)
#        stop(gettextf("unary '%s' not defined for \"POSIXt\" objects",
#                      .Generic), domain = NA)
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
                      "!=" = , "<=" = , ">=" = TRUE, FALSE)

    eq <- switch(.Generic, "+" = , "-" = , "==" = , "!=" = , 
		"<" = , ">" = , "<=" = , ">=" = TRUE, FALSE)

    prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)

	if (! eq && ! prd)
		stop(paste("operation", .Generic, "not allowed"))

#    if (!boolean)
#        stop(gettextf("'%s' not defined for \"units\" objects", .Generic),
#             domain = NA)
	if (eq)
		units(e2) = units(e1) 

	if (prd) {
		attr(e1, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
		attr(e2, "units") = paste0("(", units(e1), ")", .Generic, "(", units(e2), ")")
	}

	# NOT SURE THIS IS A GOOD IDEA:
    #if(!inherits(e1, "units")) e1 <- as.units(e1)
    #if(!inherits(e2, "units")) e2 <- as.units(e2)
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

