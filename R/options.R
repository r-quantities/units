.units_options <- new.env(FALSE, parent=globalenv())

#' set one or more units global options
#' 
#' set units global options, mostly related how units are printed and plotted
#' @param ... ignored
#' @param sep character length two; default \code{c("~", "~")}; space separator between variable and units, and space separator between two different units
#' @param group character length two; start and end group, may be two empty strings, a parenthesis pair, or square brackets.
#' @param negative_power logical, default FALSE; should denominators have negative power, or follow a division symbol?
#' @param parse logical, default \code{TRUE}; should the units be made into an expression (so we get subscripts)? Setting to \code{FALSE} may be useful if \link{parse} fails, e.g. if the unit contains symbols that assume a particular encoding
#' @examples
#' units_options(sep = c("~~~", "~"), group = c("", "")) # more space, parenthesis
#' ## set defaults:
#' units_options(sep = c("~", "~"), group = c("[", "]"), negative_power = FALSE, parse = TRUE)
#' @export
units_option = function(..., sep, group, negative_power, parse) {
	if (!missing(sep)) {
	    stopifnot(is.character(sep) && length(sep) == 2)
		assign(".units.sep", sep, envir=.units_options)
	}
	if (!missing(group)) {
	    stopifnot(is.character(group) && length(group) == 2 && all(nchar(group) <= 1))
		assign(".units.group", group, envir=.units_options)
	}
	if (!missing(negative_power))
		assign(".units.negative_power", negative_power, envir=.units_options)
	if (!missing(parse))
		assign(".units.parse", parse, envir=.units_options)
}
units_option(sep = c("~", "~"), group = c("[", "]"), 
	negative_power = FALSE, parse = TRUE) # set defaults
