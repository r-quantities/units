#' Position scales for units data
#'
#' These are the default scales for the \code{units} class. These will usually
#' be added automatically. To override manually, use \code{scale_*_units}.
#'
#' @param ... arguments passed on to \code{\link[ggplot2]{continuous_scale}}.
#' @inheritParams ggplot2::continuous_scale
#'
#' @param unit A unit specification to use for the axis. If given, the values
#' will be converted to this unit before plotting. An error will be thrown if
#' the specified unit is incompatible with the unit of the data.
#'
#' @name scale_units
#' @aliases NULL
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly=TRUE)) {
#'
#' library(ggplot2)
#'
#' mtcars$consumption <- set_units(mtcars$mpg, mi / gallon)
#' mtcars$power <- set_units(mtcars$hp, hp)
#'
#' # Use units encoded into the data
#' ggplot(mtcars) +
#'   geom_point(aes(power, consumption))
#'
#' # Convert units on the fly during plotting
#' ggplot(mtcars) +
#'   geom_point(aes(power, consumption)) +
#'   scale_x_units(unit = "W") +
#'   scale_y_units(unit = "km/l")
#'
#' # Resolve units when transforming data
#' ggplot(mtcars) +
#'   geom_point(aes(power, 1 / consumption))
#'
#' }
NULL

#' @rdname scale_units
#' @export
scale_x_units <- function(..., position = "bottom", unit = NULL) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  sc <- ggplot2::continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final",
      "xlower", "xmiddle", "xupper"),
    "position_c", identity, ...,
    position = position,
    guide = ggplot2::waiver(),
    super = MakeScaleContinuousPositionUnits()
  )
  sc$units <- as_units(unit)
  sc
}

#' @rdname scale_units
#' @export
scale_y_units <- function(..., unit = NULL) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  sc <- ggplot2::continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final",
      "lower", "middle", "upper"),
    "position_c", identity, ...,
    guide = ggplot2::waiver(),
    super = MakeScaleContinuousPositionUnits()
  )
  sc$units <- as_units(unit)
  sc
}

MakeScaleContinuousPositionUnits <- function() {
  ggplot2::ggproto(
    "ScaleContinuousPositionUnits",
    ggplot2::ScaleContinuousPosition,
    units = NULL,

    train = function(self, x) {
      if (length(x) == 0) return()
      if (!is.null(self$units))
        units(x) <- as_units(1, self$units)
      self$range$train(x)
    },

    map = function(self, x, limits = self$get_limits()) {
      if (inherits(x, "units")) {
        if (is.null(self$units))
          self$units <- units(x)
        else units(x) <- as_units(1, self$units)
        x <- drop_units(x)
      }
      ggplot2::ggproto_parent(
        ggplot2::ScaleContinuousPosition, self)$map(x, limits)
    },

    make_title = function(self, title) {
      if (!is.null(title))
        title <- make_unit_label(title, as_units(1, self$units))
      title
    }
  )
}

# registered in .onLoad()
scale_type.units <- function(x) {
  if (!"units" %in% .packages())
    stop("Variable of class 'units' found, but 'units' package is not attached.\n",
         "  Please, attach it using 'library(units)' to properly show scales with units.")
  c("units", "continuous")
}
