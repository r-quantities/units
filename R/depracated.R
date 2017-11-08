
#' Deprecated functions
#' 
#' The following functions are deprecated and will be removed in a future release.
#' 
#' @export
#' @rdname deprecated
make_unit <- function(chr, ...) {
  warning("make_unit() is deprecated. Please use as_units()")
  as_units(chr, ...)
}

#' @export
#' @rdname deprecated
parse_unit <- function(chr, ...) {
  warning("parse_unit() is deprecated. Please use as_units()")
  as_units(chr, ...)
}

