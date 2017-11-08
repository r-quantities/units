
#' deprecated functions
#' 
#' The following functions are deprecated and will be removed in a future release.
#' 
#' @param x see \link{symbolic_unit}
#' @param ... see \link{symbolic_unit}
#' @export
#' @rdname deprecated
make_unit <- function(x, ...) {
  warning("make_unit() is deprecated. Please use symbolic_unit()")
  symbolic_unit(x, ...)
}

