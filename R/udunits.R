#' \pkg{udunits2} utilities
#'
#' Some \pkg{udunits2} utilities are exposed to the user. These functions are
#' useful for checking whether units are convertible or converting between units
#' without having to create \pkg{units} objects.
#' Arguments are recycled if necessary.
#'
#' @param from,to character vector or object of class \code{symbolic_units},
#' for the symbol(s) of the original unit(s) and the unit to convert to respectively.
#' @param ...  unused.
#'
#' @return \code{ud_are_convertible}
#' returns \code{TRUE} if both units exist and are convertible,
#' \code{FALSE} otherwise.
#'
#' @name udunits2
#' @export
ud_are_convertible <- function(from, to, ...) {
  if (length(dots <- list(...))) {
    if (exists("x", dots)) from <- dots$x
    if (exists("y", dots)) to   <- dots$y
    warning("variables `x` and `y` were unfortunate names, and are deprecated",
            "; please use `from` and `to` instead")
  }
  mapply(ud_convertible, ud_char(from), ud_char(to), USE.NAMES=FALSE)
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
#' ud_are_convertible(c("m", "mm"), "km")
#' ud_convert(c(100, 100000), c("m", "mm"), "km")
#'
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h")
#' ud_convert(1:3, units(a), "km/h")
#'
#' ud_are_convertible("degF", "degC")
#' ud_convert(32, "degF", "degC")
ud_convert <- function(x, from, to) {
  mapply(ud_convert_doubles, x, ud_char(from), ud_char(to))
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

ud_parse <- function(u, names=FALSE, definition=FALSE, ascii=FALSE) {
  R_ut_format(R_ut_parse(u), names, definition, ascii)
}
