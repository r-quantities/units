.units_options <- new.env(FALSE, parent = globalenv())

#' set one or more units global options
#' 
#' set units global options, mostly related how units are printed and plotted
#' @param ... ignored
#' @param sep character length two; default \code{c("~", "~")}; space separator between variable and units, and space separator between two different units
#' @param group character length two; start and end group, may be two empty strings, a parenthesis pair, or square brackets; default: square brackets.
#' @param negative_power logical, default \code{FALSE}; should denominators have negative power, or follow a division symbol?
#' @param parse logical, default \code{TRUE}; should the units be made into an expression (so we get subscripts)? Setting to \code{FALSE} may be useful if \link{parse} fails, e.g. if the unit contains symbols that assume a particular encoding
#' @param set_units_mode character; either \code{"symbols"} or \code{"standard"}; see \link{set_units}; default is \code{"symbols"}
#' @param auto_convert_names_to_symbols logical, default \code{TRUE}: should names, such as \code{degree_C} be converted to their usual symbol?
#' @param simplify logical, default \code{TRUE}; simplify units in expressions?
#' @examples
#' units_options(sep = c("~~~", "~"), group = c("", "")) # more space, parenthesis
#' ## set defaults:
#' units_options(sep = c("~", "~"), group = c("[", "]"), negative_power = FALSE, parse = TRUE)
#' @export
units_options = function(..., sep, group, negative_power, parse, set_units_mode, auto_convert_names_to_symbols, simplify) {
	# op = as.list(units:::.units_options)
	if (!missing(sep)) {
	    stopifnot(is.character(sep) && length(sep) == 2)
		assign(".units.sep", sep, envir = .units_options)
	}
	if (!missing(group)) {
	    stopifnot(is.character(group) && length(group) == 2 && all(nchar(group) <= 1))
		assign(".units.group", group, envir = .units_options)
	}
	if (!missing(negative_power)) {
		stopifnot(is.logical(negative_power))
		assign(".units.negative_power", negative_power, envir=.units_options)
	}
	if (!missing(parse)) {
		stopifnot(is.logical(parse))
		assign(".units.parse", parse, envir = .units_options)
	}
	if (!missing(set_units_mode)) {
		stopifnot(is.character(set_units_mode) && length(set_units_mode) == 1)
		assign(".units.set_units_mode", set_units_mode, envir=.units_options)
	}
	if (!missing(auto_convert_names_to_symbols)) {
		stopifnot(is.logical(auto_convert_names_to_symbols))
		assign(".units.auto_convert_names_to_symbols", auto_convert_names_to_symbols, envir = .units_options)
	}
	if (!missing(simplify)) {
		stopifnot(is.logical(simplify))
		assign(".units.simplify", simplify, envir = .units_options)
	}

	dots = list(...)
	if (length(dots)) {
		if (is.list(dots[[1]]))
			do.call(units_options, dots[[1]])
		else
			get(paste0(".units.", dots[[1]]), envir = .units_options)
	} else
		invisible(NULL)
}

units_options(
	sep = c("~", "~"), 
	group = c("[", "]"), 
	negative_power = FALSE, 
	parse = TRUE,
	set_units_mode = "symbols",
	auto_convert_names_to_symbols = TRUE,
	simplify = TRUE) # set defaults
