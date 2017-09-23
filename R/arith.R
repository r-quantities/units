
# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' S3 Ops Group Generic Functions for units objects
#'
#' Ops functions for units objects, including comparison, product and divide, add, subtract
#'
#' @param e1 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as_units(e1)}
#' @param e2 object of class \code{units}, 
#'        or something that can be coerced to it by \code{as_units(e2)},
#'        or in case of power a number (integer n or 1/n)
#'
#' @return object of class \code{units}
#' @export
#'
#' @examples
#' a <- set_units(1:3, m/s)
#' b <- set_units(1:3, m/s)
#' a + b
#' a * b
#' a / b
#' a <- make_unit("kg m-3") # not understood by R as a division, but understood by udunits2
#' b <- set_units(1, kg/m/m/m)
#' a + b
Ops.units <- function(e1, e2) {

  unary = nargs() == 1
  eq  <- .Generic %in% c("+", "-", "==", "!=", "<", ">", "<=", ">=") # pm/equality-type
  prd <- .Generic %in% c("*", "/")                                   # product-type
  pw  <- .Generic %in% c( "**", "^")                                 # power-type
  pm  <- .Generic %in% c("+", "-")                                   # addition-type

  if (unary) {
    if (! pm)
      stop("only unary + and - supported")
    if (.Generic == "-")
      return(e1 * set_units(-1.0, 1))
	else
      return(e1)
  }
  
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

    if (identical(units(e1), unitless))
      return(set_units(unclass(e1) ^ e2, 1))

    if (inherits(e2, "units") || length(e2) > 1L)
      stop("power operation only allowed with length-one numeric power")
    # if (round(e2) != e2)
    #   stop("currently you can only take integer powers of units")
    
    # we repeat each unit the number of times given by e2. They are already
    # sorted so they will remain sorted. We need to flip numerator and denominator
    # when the power is negative and we have a special case when it is zero where
    # units should be removed.
    if (e2 == 0)
      u <- unitless
    # else if (e2 > 0)
    #   u <- .symbolic_units(rep(units(e1)$numerator, e2),
    #                                        rep(units(e1)$denominator, e2))
    # else
    #   u <- .symbolic_units(rep(units(e1)$denominator, abs(e2)),
    #                                        rep(units(e1)$numerator, abs(e2)))
    else if (e2 >= 1) {
      if (round(e2) != e2) {
        stop("currently you can only take integer powers of units above 1")}
      u <- .symbolic_units(rep(units(e1)$numerator, e2),
                                           rep(units(e1)$denominator, e2))
    } else if (e2 <= -1) {
      if (round(e2) != e2) {
        stop("currently you can only take integer powers of units below -1")}
      u <- .symbolic_units(rep(units(e1)$denominator, abs(e2)),
                                           rep(units(e1)$numerator, abs(e2)))
    } else { # -1 < e2 < 0 || 0 < e2 < 1
      if ((1/e2) %% 1 != 0) {
        stop("not a integer divisor")} # work on wording
      #if ((length(units(e1)$denominator) && any((table(units(e1)$denominator)*e2) %% 1 != 0)) ||
      #    (!all(units(e1)$numerator == "1") && any((table(units(e1)$numerator)*e2) %% 1 != 0))) {
      if (any((table(units(e1)$denominator)*e2) %% 1 != 0) ||
          any((table(units(e1)$numerator)*e2)   %% 1 != 0)) {
            stop("units not divisible")} # work on wording
      if (e2 > 0) # 0 < e2 < 1
        u <- .symbolic_units(
          rep(unique(units(e1)$numerator),table(units(e1)$numerator)*e2),
          rep(unique(units(e1)$denominator),table(units(e1)$denominator)*e2))
      else # -1 < e2 < 0
        u <- .symbolic_units(
          rep(unique(units(e1)$denominator),table(units(e1)$denominator)*abs(e2)),
          rep(unique(units(e1)$numerator),table(units(e1)$numerator)*abs(e2)))
    }
  } else # eq, plus/minus:
    u <- units(e1)

  if (eq && !pm) {
    dimension = dim(structure(as.numeric(e1), dim = dim(e1)) == structure(as.numeric(e2), dim = dim(e2)))
    structure(as.logical(NextMethod()), dim = dimension)
  } else
    .as.units(NextMethod(), u)
}

#' matrix multiplication
#' @param x numeric matrix or vector
#' @param y numeric matrix or vector
#' @export
`%*%` = function(x, y) UseMethod("%*%")

#' @export
`%*%.default` = function(x, y) {
	if (inherits(y, "units"))
		`%*%.units`(x, y)
	else
		base::`%*%`(x, y)
}

#' @export
#' @examples
#' a = set_units(1:5, m)
#' a %*% a
#' a %*% t(a)
#' a %*% 1:5
#' 1:5 %*% a
`%*%.units` = function(x, y) {
	# warning("%*%.units...")
	set_units(`%*%.default`(unclass(x), unclass(y)), units(x[1] * y[1]))
}
