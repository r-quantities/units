# I don't know how to test the plotting code in unit tests, so I am turning coverage off 
# until I figure out how to.

# nocov start 

#' @export
plot.units <- function(x, y, xlab = NULL, ylab = NULL, ...) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  if (is.null(xlab)) {
    xlab <- paste0(deparse(substitute(x)), " [", as.character(units(x)), "]")
  }
  if (is.null(ylab) && inherits(y, "units")) {
    ylab <- paste0(deparse(substitute(y)), " [", as.character(units(y)), "]")
  }
  NextMethod("plot", xlab = xlab, ylab = ylab)
}

#' @export
hist.units <- function(x, xlab = NULL, ...) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  if (is.null(xlab)) {
    xlab <- paste0(deparse(substitute(x)), " [", as.character(units(x)), "]")
  }
  NextMethod("hist", xlab = xlab)
}

# nocov end