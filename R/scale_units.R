#' Continuous scales for units data
#'
#' These are the default scales for the \code{units} class. These will usually
#' be added automatically. To override manually, use \code{scale_{type}_units}.
#'
#' @param ... arguments passed on to the corresponding continuous scale
#' (see the manual page for each \code{scale_{type}} for details).
#' @inheritParams ggplot2::scale_x_continuous
#' @param unit A unit specification to use for the guide. If given, the values
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
#'   scale_y_units(transform="reverse")
#'
#' }
NULL

#' @rdname scale_units
#' @export
scale_x_units <- function(..., sec.axis = ggplot2::waiver(), unit = NULL) {
  sc <- make_scale_units(ggplot2::scale_x_continuous(...), unit)
  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_units
#' @export
scale_y_units <- function(..., sec.axis = ggplot2::waiver(), unit = NULL) {
  sc <- make_scale_units(ggplot2::scale_y_continuous(...), unit)
  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_units
#' @export
scale_colour_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_colour_continuous(...), unit)
}

#' @rdname scale_units
#' @export
scale_color_units <- scale_colour_units

#' @rdname scale_units
#' @export
scale_fill_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_fill_continuous(...), unit)
}

#' @rdname scale_units
#' @export
scale_alpha_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_alpha(...), unit)
}

#' @rdname scale_units
#' @export
scale_size_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_size(...), unit)
}

#' @rdname scale_units
#' @export
scale_size_area_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_size_area(...), unit)
}

#' @rdname scale_units
#' @export
scale_radius_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_radius(...), unit)
}

#' @rdname scale_units
#' @export
scale_linewidth_units <- function(..., unit = NULL) {
  make_scale_units(ggplot2::scale_linewidth(...), unit)
}

make_scale_units <- function(parent, unit) {
  if (!requireNamespace("ggplot2", quietly=TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)

  ggplot2::ggproto(
    paste0(class(parent)[1], "Units"),
    parent,
    units = as_units(unit),

    set_convert = function(self, x) {
      if (!inherits(x, "units"))
        return(x)

      if (is.null(self$units))
        self$units <- units(x)
      else units(x) <- as_units(1, self$units)
      drop_units(x)
    },

    map = function(self, x, limits = self$get_limits()) {
      x <- self$set_convert(x)
      ggplot2::ggproto_parent(parent, self)$map(x, limits)
    },

    train = function(self, x) {
      x <- self$set_convert(x)
      ggplot2::ggproto_parent(parent, self)$train(x)
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

    make_title = function(self, guide_title, scale_title, label_title) {
      if (missing(label_title)) { # ggplot2 <= 3.5.1
        title <- guide_title
        if (!is.null(title))
          title <- make_unit_label(title, as_units(1, self$units))
        return(title)
      }

      if (!is.null(label_title))
        label_title <- make_unit_label(label_title, as_units(1, self$units))
      ggplot2::ggproto_parent(parent, self)$make_title(
        guide_title, scale_title, label_title)
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
