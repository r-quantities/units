#' Test if two units are convertible
#'
#' Parses and checks whether units can be converted by UDUNITS-2. Units may not
#' be convertible either because they are different magnitudes or because one
#' (or both) units are not defined in the database.
#'
#' @param x character or object of class \code{symbolic_units}, for the symbol
#' of the first unit.
#' @param y character or object of class \code{symbolic_units}, for the symbol
#' of the second unit.
#'
#' @return boolean, \code{TRUE} if both units exist and are convertible.
#' @export
#'
#' @examples
#' ud_are_convertible("m", "km")
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h")
#' ud_are_convertible("s", "kg")
ud_are_convertible = function(x, y) {
	stopifnot(inherits(x, c("character", "symbolic_units")), inherits(y, c("character", "symbolic_units")))
	res <- try(R_ut_are_convertible(
	  R_ut_parse(ud_char(x)), R_ut_parse(ud_char(y))), silent = TRUE)
	! inherits(res, "try-error") && res
}

#' Convert between units
#'
#' Convert numeric vector from one unit to another compatible unit
#'
#' @param x numeric vector
#' @param from character; unit of \code{x}
#' @param to character; unit to convert \code{x} to
#'
#' @return numeric vector with \code{x} converted to new unit
#' @export
#'
#' @examples
#' ud_convert(100, "m", "km")
#' ud_convert(32, "degF", "degC")
ud_convert <- function(x, from, to) {
  .Call("_units_ud_convert", PACKAGE = "units", x, from, to)
}

ud_char <- function(x) {
  if (is.character(x)) return(x)
  res <- if (length(x$numerator))
    paste(x$numerator, collapse=" ") else "1"
  if (length(x$denominator))
    res <- paste0(res, " (", paste(x$denominator, collapse=" "), ")-1")
  res
}

ud_are_same <- function(x, y) {
  get_base <- function(x)
    tail(strsplit(R_ut_format(R_ut_parse(x), definition=TRUE), " ")[[1]], 1)
  identical(get_base(x), get_base(y))
}

ud_get_symbol = function(u) {
  u <- R_ut_parse(u)
	sym = R_ut_get_symbol(u)
	if (!length(sym))
		sym = R_ut_get_name(u)
	sym
}

ud_is_parseable = function(u) {
	res <- try(R_ut_parse(u), silent = TRUE)
	! inherits(res, "try-error")
}
