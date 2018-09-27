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
#' a <- sqrt(1:10)
#' a <- set_units(a, m/s)
#' log(a)
#' log(a, base = 10)
#' cumsum(a)
#' signif(a, 2)
Math.units = function(x, ...) {
  if (.Generic == "sqrt")
    return(x^0.5)
  
  if (.Generic == "sign")	
    return(as.numeric(NextMethod()))
  
  OK <- switch(.Generic, "abs" = , "sign" = , "floor" = , "ceiling" = , "log" = ,
               "trunc" = , "round" = , "signif" = , "cumsum" = , 
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
    # nocov start
    # I'm disabling coverage testing for this part because I am not sure
    # how it should even be implemented...
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
      .as.units(NextMethod(), units(symbolic_unit(u, check_is_valid = FALSE)))
      # nocov end
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
