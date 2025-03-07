#' \pkg{udunits2} utilities
#'
#' Some \pkg{udunits2} utilities are exposed to the user. These functions are
#' useful for checking whether units are convertible or converting between units
#' without having to create \pkg{units} objects.
#'
#' @param from character or object of class \code{symbolic_units},
#' for the symbol of the original unit.
#' @param to   character or object of class \code{symbolic_units},
#' for the symbol of the unit to convert.
#'
#' @return \code{ud_are_convertible}
#' returns \code{TRUE} if both units exist and are convertible,
#' \code{FALSE} otherwise.
#'
#' @name udunits2
#' @export
ud_are_convertible <- function(from, to) {
  ud_convertible(ud_char(from), ud_char(to))
}

#' @param x numeric vector
#'
#' @return \code{ud_convert}
#' returns a numeric vector with \code{x} converted to new unit.
#'
#' @name udunits2
#' @export
#'
#' @examples
#' ud_are_convertible("m", "km")
#' ud_convert(100, "m", "km")
#'
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h")
#' ud_convert(1:3, units(a), "km/h")
#'
#' ud_are_convertible("degF", "degC")
#' ud_convert(32, "degF", "degC")
ud_convert <- function(x, from, to) {
  ud_convert_doubles(x, ud_char(from), ud_char(to))
}

ud_char <- function(x) {
  if (is.character(x)) return(x)
  if (!inherits(x, "symbolic_units")) stop("not a unit")

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
