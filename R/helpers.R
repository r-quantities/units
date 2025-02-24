#' Apply a function keeping units
#'
#' Helper function to apply a function to a \code{units} object and then restore
#' the original units.
#'
#' Provided for incompatible functions that do not preserve units. The user is
#' responsible for ensuring the correctness of the output.
#'
#' If \code{x} is not a \code{units} object
#' and \code{unit} is not provided by the user,
#' a warning is issued, and the output will also have no units
#' (see examples).
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
#' # An example use case is with random number generating functions:
#' mu <- as_units(10, "years")
#' keep_units(rnorm, n = 1, x = mu)
#'
#' # units can be directly specified if needed:
#' rate <- as_units(3, "1/year")
#' keep_units(rexp, n = 1, x = rate, unit = units(1/rate))
#'
#' # if `x` does not actually have units, a warning is issued,
#' # and the output has no units:
#' rate2 <- 3
#' keep_units(rexp, n = 1, x = rate2)
#'
#' @export
keep_units <- function(FUN, x, ..., unit=units(x)) {
  if (inherits(try(unit, silent = TRUE), "symbolic_units")) {
    set_units(do.call(FUN, list(x, ...)), unit, mode = "standard")
  } else {
    warning("wrong `unit` specification.")
    do.call(FUN, list(x, ...))
  }
}

dfapply <- function(X, FUN, ...) {
  attrs <- attributes(X)
  X <- lapply(X, FUN, ...)
  attributes(X) <- attrs
  X
}
