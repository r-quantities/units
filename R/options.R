.units_options <- new.env(FALSE, parent = globalenv())

.default_options <- list(
  sep = c("~", "~"),
  group = c("[", "]"),
  negative_power = FALSE,
  parse = TRUE,
  set_units_mode = "symbols",
  auto_convert_names_to_symbols = TRUE,
  simplify = NA,
  allow_mixed = FALSE,
  unitless_symbol = "1",
  define_bel = TRUE
)

.setopt <- function(option) {
  name <- paste0(".units.", substitute(option))
  last <- mget(name, envir = .units_options, ifnotfound = NA)[[1]]
  assign(name, option, envir = .units_options)
  last
}

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
#' @param define_bel logical; if \code{TRUE}, define the unit \code{B} (i.e., the \emph{bel}, widely used with the \emph{deci-} prefix as \code{dB}, \emph{decibel}) as an alias of \code{lg(re 1)}. \code{TRUE} by default, unless \code{B} is already defined in the existing XML database.
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
		allow_mixed, unitless_symbol, define_bel) {
	# op = as.list(units:::.units_options)
	ret = list()
	if (!missing(sep)) {
		stopifnot(is.character(sep) && length(sep) == 2)
		ret$sep = .setopt(sep)
	}
	if (!missing(group)) {
		stopifnot(is.character(group) && length(group) == 2)
		ret$group = .setopt(group)
	}
	if (!missing(negative_power)) {
		stopifnot(is.logical(negative_power))
		ret$negative_power = .setopt(negative_power)
	}
	if (!missing(parse)) {
		stopifnot(is.logical(parse))
		ret$parse = .setopt(parse)
	}
	if (!missing(set_units_mode)) {
		stopifnot(is.character(set_units_mode) && length(set_units_mode) == 1)
		ret$set_units_mode = .setopt(set_units_mode)
	}
	if (!missing(auto_convert_names_to_symbols)) {
		stopifnot(is.logical(auto_convert_names_to_symbols))
		ret$auto_convert_names_to_symbols = .setopt(auto_convert_names_to_symbols)
	}
	if (!missing(simplify)) {
		stopifnot(is.logical(simplify))
		ret$simplify = .setopt(simplify)
	}
	if (!missing(allow_mixed)) {
		stopifnot(is.logical(allow_mixed))
		ret$allow_mixed = .setopt(allow_mixed)
	}
	if (!missing(unitless_symbol)) {
		stopifnot(is.character(unitless_symbol), length(unitless_symbol) == 1)
		ret$unitless_symbol = .setopt(unitless_symbol)
	}
	if (!missing(define_bel)) {
	  stopifnot(is.logical(define_bel))
	  ret$define_bel = .setopt(define_bel)
	  if (!identical(ret$define_bel, define_bel)) {
	    if (!is.na(ret$define_bel))
	      remove_unit("B", "bel")
	    if (define_bel)
	      install_unit("B", "lg(re 1)", "bel")
	  }
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

.units.simplify = function() {
  get(".units.simplify", envir = .units_options)
}
