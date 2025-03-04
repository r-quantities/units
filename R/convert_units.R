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
#' convert_units(100, "m", "km")
#' convert_units(32, "degF", "degC")
convert_units <- function(x, from, to) {
  units:::ud_convert(x, from, to)
}
