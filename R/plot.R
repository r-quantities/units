mk_unit_label = function(lab, u, sep) paste0(lab, sep[1], as.character(units(u)), sep[2])

#' plot unit objects
#' 
#' plot unit objects
#' @param x object of class units, to plot along the x axis, or, if y is missing, along the y axis
#' @param y object to plot along the y axis, or missing
#' @param xlab character; x axis label
#' @param ylab character; y axis label
#' @param ... other parameters, passed on to \link{plot.default}
#' @param sep length two character vector; symbols to put before and after the measurement unit
#' @export
#' @examples
#' u = rnorm(1:10) * make_unit("°C")
#' v = rnorm(1:10) * make_unit("s")
#' plot(u,v)
#' plot(u, type = 'l')
plot.units <- function(x, y, xlab = NULL, ylab = NULL, ..., sep = c(" [", "]")) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  if (is.null(xlab)) {
    xlab <- mk_unit_label(deparse(substitute(x)), x, sep)
  }
  if (missing(y)) { # from xy.coords:
    ylab = xlab
	xlab = "Index"
	y = x
	x = seq_along(x)
    return(NextMethod("plot", x, y, xlab = xlab, ylab = ylab))
  } else if (is.null(ylab) && inherits(y, "units")) {
    ylab <- mk_unit_label(deparse(substitute(y)), y, sep)
  }
  x = unclass(x)
  y = unclass(y)
  NextMethod("plot", xlab = xlab, ylab = ylab)
}

#' histogram for unit objects
#' 
#' histogram for unit objects
#' @param x object of class units, for which we want to plot the histogram
#' @param xlab x axis label
#' @param ... parameters passed on to \link{hist.default}
#' @param sep length two character vector; symbols to put before and after the measurement unit
#' @export
#' @examples
#' u = rnorm(100) * make_unit("°C")
#' hist(u)
hist.units <- function(x, xlab = NULL, ..., sep = c(" [", "]")) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  if (is.null(xlab)) {
    xlab <- mk_unit_label(deparse(substitute(x)), x, sep)
  }
  NextMethod("hist", xlab = xlab)
}
