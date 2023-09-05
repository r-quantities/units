
# Inside the group generic functions we do have .Generic even if the diagnostics
# think we do not.
# !diagnostics suppress=.Generic

#' S3 Ops Group Generic Functions for units objects
#'
#' Ops functions for units objects, including comparison, product and divide,
#' add, subtract.
#'
#' Users are advised against performing arithmetical operations with
#' temperatures in different units. The \pkg{units} package ensure that results
#' 1) are arithmetically correct, and 2) satisfy dimensional analysis, but could
#' never ensure that results are physically meaningful. Temperature units are
#' special because there is an absolute unit, Kelvin, and relative ones, Celsius
#' and Fahrenheit degrees. Arithmetic operations between them are meaningless
#' from the physical standpoint. Users are thus advised to convert all
#' temperatures to Kelvin before operating.
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
#' a %% set_units(2)
Ops.units <- function(e1, e2) {
  if (missing(e2))
    return(NextMethod())

  cmp <- .Generic %in% c("==", "!=", "<", ">", "<=", ">=") # comparison-type
  div <- .Generic %in% c("/", "%/%")                       # division-type
  mul <- .Generic %in% "*"                                 # multiplication-only
  prd <- .Generic %in% c("*", "/", "%/%", "%%")            # product-type
  pw  <- .Generic %in% c("**", "^")                        # power-type
  mod <- .Generic %in% c("%/%", "%%")                      # modulo-type
  pm  <- .Generic %in% c("+", "-")                         # addition-type

  if (! any(cmp, pm, prd, pw, mod))
    stop(paste("operation", .Generic, "not allowed"))

  e1_inherits_units <- inherits(e1, "units")
  e2_inherits_units <- inherits(e2, "units")
  both_inherit_units <- e1_inherits_units && e2_inherits_units

  if ((pm || cmp) && !both_inherit_units)
    stop("both operands of the expression should be \"units\" objects") # nocov

  if (cmp) {
    if (!ud_are_convertible(units(e1), units(e2)))
      stop("cannot compare non-convertible units")

    if (length(e1) >= length(e2)) {
      e1 <- ud_compare(e1, e2, as.character(units(e1)), as.character(units(e2)))
      attr <- attributes(e2)
      e2 <- rep(0, length(e2))
      attributes(e2) <- attr
    } else {
      e2 <- ud_compare(e2, e1, as.character(units(e2)), as.character(units(e1)))
      attr <- attributes(e1)
      e1 <- rep(0, length(e1))
      attributes(e1) <- attr
    }
    return(NextMethod())
  }

  identical_units <-
    both_inherit_units &&
    identical(units(e1), units(e2))

  inverse_units <-
    both_inherit_units &&
    identical(units(e1)$numerator, units(e2)$denominator) &&
    identical(units(e1)$denominator, units(e2)$numerator)

  if (((div && identical_units) || (mul && inverse_units)) & !isFALSE(.units.simplify())) {
    # Special cases for identical unit division and inverse unit multiplication
    # which may not be otherwise divisible by udunits (see #310)

    # This block only runs with `!isFALSE(.units.simplify())` due to #355

    e1 <- drop_units(e1)
    e2 <- drop_units(e2)
    return(set_units(NextMethod(), 1))
  } else if (mod) {
    div <- e1 / e2
    int <- round(div)
    if (.Generic == "%/%") {
      return(int)
    } else {
      return(e1 - int*e2)
    }
  } else if (prd) {
    if (!e1_inherits_units)
      e1 <- set_units(e1, 1) # TODO: or warn?

    if (!e2_inherits_units)
      e2 <- set_units(e2, 1) # TODO: or warn?

    ve1 <- unclass(e1) ; ue1 <- units(e1)
    ve2 <- unclass(e2) ; ue2 <- units(e2)

    if (.Generic == "*") {
      numerator <- sort(c(ue1$numerator, ue2$numerator))
      denominator <- sort(c(ue1$denominator, ue2$denominator))
    } else {
      numerator <- sort(c(ue1$numerator, ue2$denominator))
      denominator <- sort(c(ue1$denominator, ue2$numerator))
    }
    return(.simplify_units(NextMethod(), .symbolic_units(numerator, denominator)))

  } else if (pw) {
    if (e2_inherits_units) {
      if (e1_inherits_units && identical(units(e1), units(as_units(1)))) {
        e1 <- drop_units(e1)
      } else if (e1_inherits_units) {
        stop("power operation only allowed with numeric power")
      }

      # code to manage things like exp(log(...)) and 10^log10(...) follows
      # this is not supported in udunits2, so we are on our own
      u <- units(e2)
      invalid_lengths <- length(u$numerator) > 1 || length(u$denominator)
      if (u == unitless || invalid_lengths || !grepl("\\(re", u$numerator))
        stop("power operation only allowed with logarithmic unit")

      sp <- strsplit(u$numerator, "\\(re ")[[1]]

      # only works for the same base
      type <- strsplit(sp[1], " ")[[1]]
      fc <- if (length(type) == 1) 1 else as.numeric(type[1])
      base <- switch(type[length(type)], lb=2, lg=10, ln=exp(1/fc))
      if (!isTRUE(all.equal(e1, base)))
        stop("wrong base in power operation")

      # this is the next unit after "undoing" the outer logarithm
      nxt_u <- paste(sp[-1], collapse="(re ")   # next unit
      nxt_u <- substr(nxt_u, 1, nchar(nxt_u)-1) # remove last parenthesis

      if (length(sp) > 2) { # another logarithm!
        mult <- 1
        attr(e2, "units")$numerator <- sub("^1 ", "", nxt_u)
      } else {
        nxt_u <- strsplit(nxt_u, " ")[[1]]
        mult <- as.numeric(nxt_u[1])
        nxt_u <- as_units(nxt_u[2])
        attr(e2, "units") <- units(nxt_u)
      }
      return(mult * NextMethod())
    }

    if (length(e2) > 1L) {
      if (length(unique(e2)) == 1) # all identical
        e2 = e2[1]
      else # return mixed units:
        return(.as.mixed_units(mapply("^", e1, e2, SIMPLIFY=FALSE)))
    }

    # we repeat each unit the number of times given by e2. They are already
    # sorted so they will remain sorted. We need to flip numerator and denominator
    # when the power is negative and we have a special case when it is zero where
    # units should be removed.
    if (e2 == 0) {
      u <- units(as_units(1))
    } else {
      if (any((table(units(e1)$denominator)*e2) %% 1 != 0) ||
          any((table(units(e1)$numerator)*e2)   %% 1 != 0))
            stop("powers not divisible") # work on wording
      if (e2 > 0)
        u <- .symbolic_units(
          rep(unique(units(e1)$numerator),table(units(e1)$numerator)*e2),
          rep(unique(units(e1)$denominator),table(units(e1)$denominator)*e2))
      else
        u <- .symbolic_units(
          rep(unique(units(e1)$denominator),table(units(e1)$denominator)*abs(e2)),
          rep(unique(units(e1)$numerator),table(units(e1)$numerator)*abs(e2)))
    }
  } else { # pm:
    units(e2) <- units(e1)
    u <- units(e1)
  }

  .as.units(NextMethod(), u)
}

#' #' matrix multiplication
#' #' @name matmult
#' #' @param x numeric matrix or vector
#' #' @param y numeric matrix or vector
#' #' @export
#' #' @details see \code{"\link[base]{\%*\%}"} for the base function, reimplemented
#' #'   as default method
#' `%*%` = function(x, y) UseMethod("%*%")
#'
#' #' @name matmult
#' #' @export
#' `%*%.default` = function(x, y) {
#' 	if (inherits(y, "units"))
#' 		`%*%.units`(x, y)
#' 	else
#' 		base::`%*%`(x, y)
#' }
#'
#' #' @name matmult
#' #' @export
#' #' @examples
#' #' a = set_units(1:5, m)
#' #' a %*% a
#' #' a %*% t(a)
#' #' a %*% set_units(1:5, 1)
#' #' set_units(1:5, 1) %*% a
#' `%*%.units` = function(x, y) {
#' 	ret = `%*%.default`(unclass(x), unclass(y))
#' 	units(ret) = .multiply_symbolic_units(1, units(x), units(y))
#' 	ret
#' }
