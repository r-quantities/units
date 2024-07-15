#' Position scales for units data
#'
#' These are the default scales for the \code{units} class. These will usually
#' be added automatically. To override manually, use \code{scale_*_units}.
#'
#' @param ... arguments passed on to \code{\link[ggplot2]{continuous_scale}}
#' (e.g. scale transformations via the \code{trans} argument; see examples).
#' @inheritParams ggplot2::scale_x_continuous
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
#' # Reverse the y axis
#' ggplot(mtcars) +
#'   geom_point(aes(power, consumption)) +
#'   scale_y_units(trans="reverse")
#'
#' }
NULL

#' @rdname scale_units
#' @export
scale_x_units <- function(..., guide = ggplot2::waiver(), position = "bottom",
                          sec.axis = ggplot2::waiver(), unit = NULL) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  sc <- ggplot2::continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final",
      "xlower", "xmiddle", "xupper"),
    palette = identity, ...,
    guide = guide,
    position = position,
    super = make_scale_units()
  )
  sc$units <- as_units(unit)
  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_units
#' @export
scale_y_units <- function(..., guide = ggplot2::waiver(), position = "left",
                          sec.axis = ggplot2::waiver(), unit = NULL) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  sc <- ggplot2::continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final",
      "lower", "middle", "upper"),
    palette = identity, ...,
    guide = guide,
    position = position,
    super = make_scale_units()
  )
  sc$units <- as_units(unit)
  set_sec_axis(sec.axis, sc)
}

make_scale_units <- function(parent=ggplot2::ScaleContinuousPosition) {
  ggplot2::ggproto(
    "ScaleContinuousPositionUnits",
    parent,
    units = NULL,

    map = function(self, x, limits = self$get_limits()) {
      if (inherits(x, "units")) {
        if (is.null(self$units))
          self$units <- units(x)
        else units(x) <- as_units(1, self$units)
        x <- drop_units(x)
      }
      ggplot2::ggproto_parent(parent, self)$map(x, limits)
    },

    transform = function(self, x) {
      if (!is.null(self$units))
        units(x) <- as_units(1, self$units)

      if (inherits(self$limits, "units")) {
        units(self$limits) <- units(x)
        self$limits <- drop_units(self$limits)
      }

      new_x <- ggplot2::ggproto_parent(parent, self)$transform(drop_units(x))
      as_units(new_x, units(x))
    },

    make_title = function(self, title) {
      if (!is.null(title))
        title <- make_unit_label(title, as_units(1, self$units))
      title
    }
  )
}

set_sec_axis <- function(sec.axis, scale) {
  if (!inherits(sec.axis, "waiver")) {
    if (inherits(sec.axis, "formula"))
      sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!inherits(sec.axis, "AxisSecondary"))
      stop("Secondary axes must be specified using \"sec_axis()\"", call.=FALSE)
    sec.axis$units <- scale$units
    scale$secondary.axis <- sec.axis
  }
  scale
}

# registered in .onLoad()
scale_type.units <- function(x) {
  if (!"units" %in% .packages())
    stop("Variable of class 'units' found, but 'units' package is not attached.\n",
         "  Please, attach it using 'library(units)' to properly show scales with units.")
  c("units", "continuous")
}

utils::globalVariables("caller_env")

# registered in .onLoad()
limits.units <- function(lims, var, call = caller_env())  {
  if (length(lims) != 2)
    stop("`", var, "` must be a two-element vector", call.=FALSE)

  trans <- if (!any(is.na(lims)) && lims[1] > lims[2])
    "reverse" else "identity"
  name <- paste0("scale_", var, "_units")
  sc <- match.fun(name)(limits=lims, transform=trans)
  sc$call <- if (!is.null(call)) call else str2lang(paste0(name, "()"))
  sc
}
