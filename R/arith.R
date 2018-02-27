
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
#' a <- as_units("kg m-3")
#' b <- set_units(1, kg/m/m/m)
#' a + b
#' a = set_units(1:5, m)
#' a %/% a
#' a %/% set_units(2)
#' set_units(1:5, m^2) %/% set_units(2, m)
#' a %% a
#' a %% set_units(2 )
Ops.units <- function(e1, e2) {
  if (missing(e2))
    return(NextMethod())

  eq  <- .Generic %in% c("+", "-", "==", "!=", "<", ">", "<=", ">=") # requiring identical units
  prd <- .Generic %in% c("*", "/", "%/%")                            # product-type
  pw  <- .Generic %in% c( "**", "^")                                 # power-type
  mod <- .Generic == "%%"                                            # modulo
  pm  <- .Generic %in% c("+", "-")                                   # addition-type

  if (! any(eq, prd, pw, mod))
    stop(paste("operation", .Generic, "not allowed"))
  
  if (eq) {
    if (!(inherits(e1, "units") && inherits(e2, "units")))
      stop("both operands of the expression should be \"units\" objects") # nocov
    units(e2) <- units(e1) # convert before we can compare; errors if unconvertible
  }
  
  if (prd) {
    if (!inherits(e1, "units"))
      e1 <- set_units(e1, 1) # TODO: or warn?

    if (!inherits(e2, "units"))
      e2 <- set_units(e2, 1) # TODO: or warn?

    ve1 <- as.numeric(e1) ; ue1 <- units(e1)
    ve2 <- as.numeric(e2) ; ue2 <- units(e2)
    
    if (.Generic == "*") {
      numerator <- sort(c(ue1$numerator, ue2$numerator))
      denominator <- sort(c(ue1$denominator, ue2$denominator))
    } else {
      numerator <- sort(c(ue1$numerator, ue2$denominator))
      denominator <- sort(c(ue1$denominator, ue2$numerator))
    }
    value <- as.numeric(NextMethod())
    return(.simplify_units(value, .symbolic_units(numerator, denominator)))

  } else if (pw) { # FIXME: I am not sure how to take powers of non-integers yet

    if (identical(units(e1), set_units(1)))
      return(set_units(unclass(e1) ^ e2, set_units(1)))

    if (inherits(e2, "units") || length(e2) > 1L)
      stop("power operation only allowed with length-one numeric power")
    # if (round(e2) != e2)
    #   stop("currently you can only take integer powers of units")
    
    # we repeat each unit the number of times given by e2. They are already
    # sorted so they will remain sorted. We need to flip numerator and denominator
    # when the power is negative and we have a special case when it is zero where
    # units should be removed.
    if (e2 == 0)
      u <- set_units(1)
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
  } else # eq, plus/minus, mod:
    u <- units(e1)

  if (eq && !pm) {
    dimension = dim(structure(as.numeric(e1), dim = dim(e1)) == structure(as.numeric(e2), dim = dim(e2)))
    structure(as.logical(NextMethod()), dim = dimension)
  } else
    .as.units(NextMethod(), u)
}

#' matrix multiplication
#' @name matmult
#' @param x numeric matrix or vector
#' @param y numeric matrix or vector
#' @export
#' @details see \code{"\link[base]{\%*\%}"} for the base function, reimplemented
#'   as default method
`%*%` = function(x, y) UseMethod("%*%")

#' @name matmult
#' @export
`%*%.default` = function(x, y) {
	if (inherits(y, "units"))
		`%*%.units`(x, y)
	else
		base::`%*%`(x, y)
}

#' @name matmult
#' @export
#' @examples
#' a = set_units(1:5, m)
#' a %*% a
#' a %*% t(a)
#' a %*% set_units(1:5, 1)
#' set_units(1:5, 1) %*% a
`%*%.units` = function(x, y) {
	ret = `%*%.default`(unclass(x), unclass(y))
	units(ret) = .multiply_symbolic_units(1, units(x), units(y))
	ret
}
