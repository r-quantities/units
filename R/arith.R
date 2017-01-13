
# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' S3 Ops Group Generic Functions for units objects
#'
#' Ops functions for units objects, including comparison, product and divide, add, subtract
#'
#' @param e1 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as.units(e1)}
#' @param e2 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as.units(e2)}
#'
#' @return object of class \code{units}
#' @export
#'
#' @examples
#' a <- with(ud_units, 1:3 * m/s)
#' b <- with(ud_units, 1:3 * m/s)
#' a + b
#' a * b
#' a / b
#' a <- make_unit("kg m-3") # not understood by R as a division, but understood by udunits2
#' b <- with(ud_units, 1 * kg/m/m/m)
#' a + b
Ops.units <- function(e1, e2) {
  if (nargs() == 1)
    stop(paste("unary", .Generic, "not defined for \"units\" objects"))
  
  eq <- switch(.Generic, "+" = , "-" = , "==" = , "!=" = , 
               "<" = , ">" = , "<=" = , ">=" = TRUE, FALSE)
  
  prd <- switch(.Generic, "*" = , "/" = TRUE, FALSE)
  
  pw <- switch(.Generic, "**" = , "^" = TRUE, FALSE)
  
  if (!eq && !prd && !pw)
    stop(paste("operation", .Generic, "not allowed"))
  
  if (eq) {
  	if (!(inherits(e1, "units") && inherits(e2, "units")))
      stop("both operands of the expression should be \"units\" objects") # nocov
    units(e2) <- units(e1) # convert before we can compare
  }
  
  if (prd) {
    if (inherits(e1, "units") && inherits(e2, "units")) {
      # both vectors have units
      if (.Generic == "*") {
        e1 <- unclass(e1) * (units(e1) * units(e2))
        e2 <- unclass(e2) * (units(e1) * units(e2))
      } else if (.Generic == "/") {
        e1 <- unclass(e1) * (units(e1) / units(e2))
        e2 <- unclass(e2) * (units(e1) / units(e2))
      } else {
        stop(paste("Unexpected operator", .Generic)) # nocov
      }
      
    } else if (inherits(e1, "units")) {
      # only e1 has units
      attr(e2, "units") <- units(e1)
      class(e2) <- "units"
      
    } else if (inherits(e2, "units")) {
      # only e2 has units
      if (.Generic == "*")      attr(e1, "units") <- units(e2)
      else if (.Generic == "/") { 
	    if (length(e1) == 1) # I wish I understood why this is needed
		  attr(e2, "units") <- .invert_symbolic_units(units(e2))
		else
		  attr(e1, "units") <- .invert_symbolic_units(units(e2))
      } else stop(paste("Unexpected operator", .Generic)) # nocov
      class(e1) <- "units"
    }
  } 
  
  if (pw) { # FIXME: I am not sure how to take powers of non-integers yet
    if (inherits(e2, "units") || length(e2) > 1L)
      stop("power operation only allowed with length-one numeric power")
    if (round(e2) != e2)
      stop("currently you can only take integer powers of units")
    
    # we repeat each unit the number of times given by e2. They are already
    # sorted so they will remain sorted. We need to flip numerator and denominator
    # when the power is negative and we have a special case when it is zero where
    # units should be removed.
    if (e2 == 0) {
      attr(e1, "units") <- unitless
    } else if (e2 > 0) {
      attr(e1, "units") <- .symbolic_units(rep(units(e1)$numerator, e2),
                                           rep(units(e1)$denominator, e2))
    } else {
      attr(e1, "units") <- .symbolic_units(rep(units(e1)$denominator, abs(e2)),
                                           rep(units(e1)$numerator, abs(e2)))
    }
        
  }
  
  NextMethod(.Generic)
}

