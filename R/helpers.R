#' Apply a function keeping units
#'
#' Helper function to apply a function to a \code{units} object and then restore
#' the original units.
#'
#' Provided for incompatible functions that do not preserve units. The user is
#' responsible for ensuring the correctness of the output.
#'
#' @param FUN the function to be applied.
#' @param x first argument of \code{FUN}, of class \code{units}.
#' @param ... optional arguments to \code{FUN}.
#' @param unit symbolic unit to restore after \code{FUN}.
#'
#' @return An object of class \code{units}.
#'
#' @examples
#' x <- set_units(1:5, m)
#' keep_units(drop_units, x)
#'
#' @export
keep_units <- function(FUN, x, ..., unit=units(x)) {
  set_units(do.call(FUN, list(x, ...)), unit, mode="standard")
}
