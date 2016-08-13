# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' Mathematical operations for units objects
#'
#' @param x object of class units
#' @param ... parameters passed on to the Math functions
#' 
#' @export
#' 
#' @examples
#' a = sqrt(1:10)
#' units(a) = "m/s"
#' log(a)
#' log(a, base = 10)
#' cumsum(a)
#' signif(a, 2)
Math.units = function(x, ...) {
  OK <- switch(.Generic, "abs" = , "sign" = , "floor" = , "ceiling" = , "log" = ,
               "trunc" = , "round" = , "signif" = , "cumsum" = , 
               "cummax" = , "cummin" = TRUE, FALSE)
  if (!OK) {
    warning(paste("Operation", .Generic, "not meaningful for units"))
    x = unclass(x)
    attr(x, "units") = NULL
    NextMethod(.Generic)
  } else {
    if (.Generic == "log") {
      dts = list(...)
      if (is.null(dts$base) || dts$base == exp(1)) # missing or equal to default:
        u = paste0("ln(",units(x),")")
      else if (dts$base == 10)
        u = paste0("lg(",units(x),")")
      else if (dts$base == 2)
        u = paste0("lb(",units(x),")")
      else
        stop(paste("log with base", dts$base, "not supported"))
      .as.units(NextMethod(.Generic), u)
    } else
      .as.units(NextMethod(.Generic), units(x))
  }
}

