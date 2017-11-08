
#' Deprecated functions
#' 
#' The following functions are deprecated and will be removed in a future release.
#' 
#' @param chr length 1 character string
#' 
#' @export
#' @rdname deprecated
make_unit <- function(chr) {
  warning("make_unit() is deprecated. Please use as_units()")
  as_units.character(chr, force_single_symbol = TRUE)
}

#' @export
#' @rdname deprecated
parse_unit <- function(chr) {
  warning("parse_unit() is deprecated. Please use as_units()")
  as_units.character(chr, implicit_exponents = TRUE)
}

