# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' Mathematical operations for units objects
#'
#' @param x object of class units
#' @param ... parameters passed on to the Math functions
#'
#' @details Logarithms receive a special treatment by the underlying \pkg{udunits2}
#' library. If a natural logarithm is applied to some \code{unit}, the result is
#' \code{ln(re 1 unit)}, which means \emph{natural logarithm referenced to
#' \code{1 unit}}. For base 2 and base 10 logarithms, the output \code{lb(...)}
#' and \code{lg(...)} respectively instead of \code{ln(...)}.
#'
#' This is particularly important for some units that are typically expressed in
#' a logarithmic scale (i.e., \emph{bels}, or, more commonly, \emph{decibels}),
#' such as Watts or Volts. For some of these units, the default \pkg{udunits2}
#' database contains aliases: e.g., \code{BW} (bel-Watts) is an alias of
#' \code{lg(re 1 W)}; \code{Bm} (bel-milliWatts) is an alias of
#' \code{lg(re 0.001 W)}; \code{BV} is an alias of \code{lg(re 1 V)} (bel-Volts),
#' and so on and so forth (see the output of \code{valid_udunits()} for further
#' reference).
#'
#' Additionally, the \pkg{units} package defines \code{B}, the \emph{bel}, by
#' default (because it is not defined by \pkg{udunits2}) as an alias of
#' \code{lg(re 1)}, unless a user-provided XML database already contains a
#' definition of \code{B}, or the \code{define_bel} option is set to \code{FALSE}
#' (see \code{help(units_options)}).
#'
#' @export
#'
#' @examples
#' # roundings, cummulative functions
#' x <- set_units(sqrt(1:10), m/s)
#' signif(x, 2)
#' cumsum(x)
#'
#' # trigonometry
#' sin(x) # not meaningful
#' x <- set_units(sqrt(1:10), rad)
#' sin(x)
#' cos(x)
#' x <- set_units(seq(0, 1, 0.1), 1)
#' asin(x)
#' acos(x)
#'
#' # logarithms
#' x <- set_units(sqrt(1:10), W)
#' log(x) # base exp(1)
#' log(x, base = 3)
#' log2(x)
#' log10(x)
#' set_units(x, dBW) # decibel-watts
#' set_units(x, dBm) # decibel-milliwatts
Math.units = function(x, ...) {
  if (.Generic == "sqrt")
    return(x^0.5)

  if (.Generic == "exp")
    return(exp(1)^x)

  if (.Generic == "expm1") {
    out <- exp(1)^x
    mult <- drop_units(out) / exp(1)^drop_units(x)
    return(out - set_units(mult, units(out), mode="standard"))
  }

  if (.Generic == "sign")
    return(as.numeric(NextMethod()))

  OK <- switch(.Generic, "abs" = , "sign" = , "floor" = , "ceiling" = , "log" = ,
               "log1p" =, "trunc" = , "round" = , "signif" = , "cumsum" = ,
               "cummax" = , "cummin" = TRUE, FALSE)

  rad = units(as_units("rad"))
  deg = units(as_units("degree"))
  if (!OK && (units(x) == rad || units(x) == deg)) {
    OK <- switch(.Generic, sin =, cos =, tan =, sinpi =, cospi =, tanpi = TRUE, FALSE)
    if (OK) {
      units(x) <- "rad" # convert deg -> rad
      x <- set_units(x) # result has unit 1
    }
  }
  if (!OK && units(x) == unitless) {
    OK <- switch(.Generic, asin =, acos =, atan = TRUE, FALSE)
    if (OK)
      units(x) <- "rad" # unit of the answer (also unitless)
  }

  if (!OK) {
    warning(paste("Operation", .Generic, "not meaningful for units"))
    x <- drop_units(x)
    NextMethod()
  } else {
    if (.Generic %in% c("log", "log1p")) {
      base <- if (missing(...)) exp(1) else c(...)[1]
      uptr <- R_ut_parse(as.character(units(x)))
      u <- R_ut_format(R_ut_log(uptr, base), ascii=TRUE)
      .as.units(NextMethod(), units(symbolic_unit(u)))
    } else
      .as.units(NextMethod(), units(x))
  }
}

#' @export
#' @method log10 units
log10.units <- function(x) log(x, 10)

#' @export
#' @method log2 units
log2.units <- function(x) log(x, 2)
