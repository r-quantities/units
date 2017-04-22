
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
  
  eq  <- .Generic %in% c("+", "-", "==", "!=", "<", ">", "<=", ">=") # pm/equality-type
  prd <- .Generic %in% c("*", "/")                                   # product-type
  pw  <- .Generic %in% c( "**", "^")                                 # power-type
  pm  <- .Generic %in% c("+", "-")                                   # addition-type
  
  if (! any(eq, prd, pw))
    stop(paste("operation", .Generic, "not allowed"))
  
  if (eq) {
    if (!(inherits(e1, "units") && inherits(e2, "units")))
      stop("both operands of the expression should be \"units\" objects") # nocov
    units(e2) <- units(e1) # convert before we can compare; errors if unconvertible
  }
  
  if (prd) {
    if (! inherits(e1, "units"))
      units(e1) = unitless

    if (! inherits(e2, "units"))
      units(e2) = unitless

    if (.Generic == "*") {
      e1 <- .multiply_symbolic_units(unclass(e1), units(e1), units(e2))
      e2 <- .multiply_symbolic_units(unclass(e2), units(e1), units(e2))
    } else {
      e1 <- .divide_symbolic_units(unclass(e1), units(e1), units(e2))
      e2 <- .divide_symbolic_units(unclass(e2), units(e1), units(e2))
    }
    u <- units(e1)

  } else if (pw) { # FIXME: I am not sure how to take powers of non-integers yet
    if (inherits(e2, "units") || length(e2) > 1L)
      stop("power operation only allowed with length-one numeric power")
    if (round(e2) != e2)
      stop("currently you can only take integer powers of units")
    
    # we repeat each unit the number of times given by e2. They are already
    # sorted so they will remain sorted. We need to flip numerator and denominator
    # when the power is negative and we have a special case when it is zero where
    # units should be removed.
    if (e2 == 0)
      u <- unitless
    else if (e2 > 0)
      u <- .symbolic_units(rep(units(e1)$numerator, e2),
                                           rep(units(e1)$denominator, e2))
    else
      u <- .symbolic_units(rep(units(e1)$denominator, abs(e2)),
                                           rep(units(e1)$numerator, abs(e2)))
  } else # eq, plus/minus:
    u <- units(e1)

  if (eq && !pm)
    as.logical(NextMethod())
  else
    .as.units(NextMethod(), u)
}
