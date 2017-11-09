

#' set_units
#'
#' A pipe friendly version of \code{units<-}
#'
#' @param x a numeric to be assigned units, or a units object to have units
#'   converted
#'
#'
#' @param value a \code{units} object, or something coercible to one with
#'   \code{as_units}. Depending on \code{mode}, the value is either treated as
#'   an bare expression or via standard evaluation.
#'
#' @param ... passed on to \code{as_units}
#'
#' @param mode if \code{"symbols"} (the default), then the bare expression
#'   supplied for \code{un} is treated as the unit. Otherwise, if\code{mode =
#'   "standard"}, the value of supplied to \code{un} is used. This argument can
#'   be set via a global option \code{options(units.set_units_mode =
#'   "standard")}
#'
#' @note for objects of class difftime, \code{set_units} is a simple alias for
#'   \code{`units<-`}, and calls \code{units(x) <- value} directly without
#'   calling \code{as_units(value)}
#'
#' @export
#' @rdname set_units
set_units <- function(x, ...) UseMethod("set_units")

#' @rdname set_units
#' @export
set_units.default <- function(x, value, ...,
  mode = getOption("units.set_units_mode", c("symbols", "standard"))) {
  
  if (missing(value))
    value <- unitless
  else if (match.arg(mode) == "symbols")
    value <- substitute(value)
  
  if (is.null(value))
    return(drop_units(x))
  
  units(x) <- as_units(value, ...)
  x
}

#' @rdname set_units
#' @export
set_units.difftime <- function(x, value, ...) {
  units(x) <- value
  x
}

