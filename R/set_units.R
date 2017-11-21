

#' set_units
#'
#' A pipe friendly version of \code{units<-}
#'
#' @param x a numeric to be assigned units, or a units object to have units
#'   converted
#'
#' @param value a \code{units} object, or something coercible to one with
#'   \code{as_units}. Depending on \code{mode}, the unit is constructed from the
#'   supplied bare expression or from the supplied value via standard evaluation.
#'
#' @param ... passed on to \code{as_units}
#'
#' @param mode if \code{"symbols"} (the default), then unit is constructed from
#'   the bare expression supplied. Otherwise, if\code{mode = "standard"},
#'   standard evaluation is used for the supplied value This argument can be set
#'   via a global option \code{options(units.set_units_mode = "standard")}
#'
#' @export
#' @rdname set_units
set_units <- function(x, value, ...,
  mode = getOption("units.set_units_mode", c("symbols", "standard"))) {
  
  if (missing(value))
    value <- unitless
  else if (match.arg(mode) == "symbols") {
    value <- substitute(value)
    
    if(is.numeric(value) && !identical(value, 1) && !identical(value, 1L))
      stop("The only valid number defining a unit is '1', signifying a unitless unit")
  }
  
  if (is.null(value))
    return(drop_units(x))
  
  units(x) <- as_units(value, ...)
  x
}
