#' create axis label with appropriate labels
#' 
#' create axis label with appropriate labels
#' @param lab length one character; name of the variable to plot
#' @param u vector of class \code{units}
#' @param sep length two character vector, defaulting to \code{c("~","~")}, with
#'   the white space between unit name and unit symbols, and between subsequent
#'   symbols.
#' @param group length two character vector with grouping symbols, e.g.
#'   \code{c("(",")")} for parenthesis, or \code{c("","")} for no group symbols
#' @param parse logical; indicates whether a parseable expression should be
#'   returned (typically needed for super scripts), or a simple character string
#'   without special formatting.
#' @export
#' @name plot.units
#' @details \link{units_options} can be used to set and change the defaults for
#'   \code{sep}, \code{group} and \code{doParse}.
make_unit_label = function(lab, u,
  		sep = units_options("sep"),
  		group = units_options("group"),
  		parse = units_options("parse")) {

  if (parse) {
    str = paste0("group('", group[1], "',", 
                 as.character(units(u), escape_units = TRUE, plot_sep = sep[2]),
                 ",'", group[2], "')")
    
	  if (length(grep("[^\t ]", lab)) > 0) {
	    str = paste0(lab, "*", sep[1], str)
	  }
    
    parse(text = str)
    
  } else {
    paste0(lab, " ", group[1], as.character(units(u)), group[2])
  }
}

#' plot unit objects
#' 
#' plot unit objects
#' @param x object of class units, to plot along the x axis, or, if y is missing, along the y axis
#' @param y object to plot along the y axis, or missing
#' @param xlab character; x axis label
#' @param ylab character; y axis label
#' @param ... other parameters, passed on to \link{plot.default}
#' @export
#' @examples
#' oldpar = par(mar = par("mar") + c(0, .3, 0, 0))
#' displacement = mtcars$disp * ud_units[["in"]]^3
#' # an example that would break if parse were (default) TRUE, since 'in' is a reserved word:
#' units_options(parse=FALSE)
#' make_unit_label("displacement", displacement)
#' units_options(parse=TRUE)
#' units(displacement) = with(ud_units, cm^3)
#' weight = mtcars$wt * 1000 * with(ud_units, lb)
#' units(weight) = with(ud_units, kg)
#' plot(weight, displacement)
#' units_options(group = c("(", ")") )  # parenthesis instead of square brackets
#' plot(weight, displacement)
#' units_options(sep = c("~~~", "~"), group = c("", ""))  # no brackets; extra space
#' plot(weight, displacement)
#' units_options(sep = c("~", "~~"), group = c("[", "]"))
#' gallon = as_units("gallon")
#' consumption = mtcars$mpg * with(ud_units, mi/gallon)
#' units(consumption) = with(ud_units, km/l)
#' plot(displacement, consumption) # division in consumption
#' units_options(negative_power = TRUE) # division becomes ^-1
#' plot(displacement, consumption)
#' plot(1/displacement, 1/consumption)
#' par(oldpar)
plot.units <- function(x, y, xlab = NULL, ylab = NULL, ...) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  xlab0 = paste(deparse(substitute(x), 500), collapse = "\\n")
  if (missing(y)) { # from xy.coords:
    if (is.null(ylab))
      ylab <- make_unit_label(deparse(substitute(x)), x)
	  if (is.null(xlab))
      xlab <- "Index"
  	y <- x
  	x <- seq_along(x)
    return(NextMethod("plot", x, y, xlab=xlab, ylab=ylab))
  } 
  if (is.null(xlab)) {
    xlab <- make_unit_label(xlab0, x)
  }
  if (is.null(ylab) && inherits(y, "units")) {
    ylab <- make_unit_label(deparse(substitute(y)), y)
  }
  NextMethod("plot", xlab=xlab, ylab=ylab)
}

#' histogram for unit objects
#' 
#' histogram for unit objects
#' @param x object of class units, for which we want to plot the histogram
#' @param xlab character; x axis label
#' @param main character; title of histogram 
#' @param ... parameters passed on to \link{hist.default}
#' @export
#' @examples
#' units_options(parse = FALSE) # otherwise we break on the funny symbol!
#' u = set_units(rnorm(100), degree_C)
#' hist(u)
hist.units <- function(x, xlab = NULL, main = paste("Histogram of", xname), ...) {
  # We define the axis labels if they are not already provided and then let
  # the default plotting function take over...
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  if (is.null(xlab)) {
    xlab <- make_unit_label(xname, x)
  }
  NextMethod("hist", xlab=xlab, main=main)
}

#' boxplot for unit objects
#' 
#' boxplot for unit objects
#' @param x object of class units, for which we want to plot the boxplot
#' @param ... parameters passed on to \link{boxplot.default}
#' @param horizontal logical indicating if the boxplots should be horizontal; 
#' default FALSE means vertical boxes.
#' @export
#' @examples
#' units_options(parse = FALSE) # otherwise we break on the funny symbol!
#' u = set_units(rnorm(100), degree_C)
#' boxplot(u)
boxplot.units <- function(x, ..., horizontal = FALSE) {
  xlab <- ylab <- NULL
  lab <- make_unit_label(deparse(substitute(x)), x)
  if (horizontal)
    xlab <- lab
  else ylab <- lab
  x <- drop_units(x)
  NextMethod("boxplot", xlab=xlab, ylab=ylab)
}
