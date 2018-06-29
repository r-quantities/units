.units_options <- new.env(FALSE, parent = globalenv())
assign(".units.sep", NA, envir = .units_options)
assign(".units.group", NA, envir = .units_options)
assign(".units.negative_power", NA, envir = .units_options)
assign(".units.parse", NA, envir = .units_options)
assign(".units.set_units_mode", NA, envir = .units_options)
assign(".units.auto_convert_names_to_symbols", NA, envir = .units_options)
assign(".units.simplify", NA, envir = .units_options)
assign(".units.allow_mixed", NA, envir = .units_options)
assign(".units.unitless_symbol", NA, envir = .units_options)


#' set one or more units global options
#' 
#' set units global options, mostly related how units are printed and plotted
#' @param ... named options (character) for which the value is queried
#' @param sep character length two; default \code{c("~", "~")}; space separator between variable and units, and space separator between two different units
#' @param group character length two; start and end group, may be two empty strings, a parenthesis pair, or square brackets; default: square brackets.
#' @param negative_power logical, default \code{FALSE}; should denominators have negative power, or follow a division symbol?
#' @param parse logical, default \code{TRUE}; should the units be made into an expression (so we get subscripts)? Setting to \code{FALSE} may be useful if \link{parse} fails, e.g. if the unit contains symbols that assume a particular encoding
#' @param set_units_mode character; either \code{"symbols"} or \code{"standard"}; see \link{set_units}; default is \code{"symbols"}
#' @param auto_convert_names_to_symbols logical, default \code{TRUE}: should names, such as \code{degree_C} be converted to their usual symbol?
#' @param simplify logical, default \code{NA}; simplify units in expressions? 
#' @param allow_mixed logical; if \code{TRUE}, combining mixed units creates a \code{mixed_units} object, if \code{FALSE} it generates an error
#' @param unitless_symbol character; set the symbol to use for unitless (1) units
#' @details This sets or gets units options. Set them by using named arguments, get them by passing the option name.
#' 
#' The default \code{NA} value for \code{simplify} means units are not simplified in \link{set_units} or \link{as_units}, but are simplified in arithmetical expressions.
#' @return in case options are set, invisibly a named list with the option values that are being set; if an option is queried, the current option value.
#' @examples
#' old = units_options(sep = c("~~~", "~"), group = c("", "")) # more space, parenthesis
#' old
#' ## set back to defaults:
#' units_options(sep = c("~", "~"), group = c("[", "]"), negative_power = FALSE, parse = TRUE)
#' units_options("group")
#' @export
units_options = function(..., sep, group, negative_power, parse, set_units_mode, auto_convert_names_to_symbols, simplify,
		allow_mixed, unitless_symbol) {
	# op = as.list(units:::.units_options)
	ret = list()
	if (!missing(sep)) {
	    stopifnot(is.character(sep) && length(sep) == 2)
		ret$sep = get(".units.sep", envir = .units_options)
		assign(".units.sep", sep, envir = .units_options)
	}
	if (!missing(group)) {
	    stopifnot(is.character(group) && length(group) == 2 && all(nchar(group) <= 1))
		ret$group = get(".units.group", envir = .units_options)
		assign(".units.group", group, envir = .units_options)
	}
	if (!missing(negative_power)) {
		stopifnot(is.logical(negative_power))
		ret$negative_power = get(".units.negative_power", envir = .units_options)
		assign(".units.negative_power", negative_power, envir=.units_options)
	}
	if (!missing(parse)) {
		stopifnot(is.logical(parse))
		ret$parse = get(".units.parse", envir = .units_options)
		assign(".units.parse", parse, envir = .units_options)
	}
	if (!missing(set_units_mode)) {
		stopifnot(is.character(set_units_mode) && length(set_units_mode) == 1)
		ret$set_units_mode = get(".units.set_units_mode", envir = .units_options)
		assign(".units.set_units_mode", set_units_mode, envir=.units_options)
	}
	if (!missing(auto_convert_names_to_symbols)) {
		stopifnot(is.logical(auto_convert_names_to_symbols))
		ret$auto_convert_names_to_symbols = get(".units.auto_convert_names_to_symbols", envir = .units_options)
		assign(".units.auto_convert_names_to_symbols", auto_convert_names_to_symbols, envir = .units_options)
	}
	if (!missing(simplify)) {
		stopifnot(is.logical(simplify))
		ret$simplify = get(".units.simplify", envir = .units_options)
		assign(".units.simplify", simplify, envir = .units_options)
	}
	if (!missing(allow_mixed)) {
		stopifnot(is.logical(allow_mixed))
		ret$allow_mixed = get(".units.allow_mixed", envir = .units_options)
		assign(".units.allow_mixed", allow_mixed, envir = .units_options)
	}
	if (!missing(unitless_symbol)) {
		stopifnot(is.character(unitless_symbol), length(unitless_symbol) == 1)
		ret$unitless_symbol = get(".units.unitless_symbol", envir = .units_options)
		assign(".units.unitless_symbol", unitless_symbol, envir = .units_options)
	}

	dots = list(...)
	if (length(dots)) {
		if (length(ret) > 0)
			stop("either set, or get units_option, but don't try to do both in one function call")
		if (is.list(dots[[1]]))
			do.call(units_options, dots[[1]])
		else
			get(paste0(".units.", dots[[1]]), envir = .units_options)
	} else
		invisible(ret)
}

units_options(
	sep = c("~", "~"), 
	group = c("[", "]"), 
	negative_power = FALSE, 
	parse = TRUE,
	set_units_mode = "symbols",
	auto_convert_names_to_symbols = TRUE,
	simplify = NA,
	allow_mixed = FALSE,
	unitless_symbol = "1") # set defaults

.units.simplify = function() {
  get(".units.simplify", envir = .units_options)
}
