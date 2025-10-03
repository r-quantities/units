#' Handle measurement units
#'
#' A number of functions are provided for handling unit objects.
#' \itemize{
#'     \item \code{`units<-`} and \code{units} are the basic functions to set
#'     and retrieve units.
#'     \item \code{as_units}, a generic with methods for a
#'     character string and for quoted language. Note, direct usage of this function
#'     by users is typically not necessary, as coercion via \code{as_units} is
#'     automatically done with \code{`units<-`} and \code{set_units}.
#'     \item \code{make_units}, constructs units from bare expressions.
#'     \code{make_units(m/s)} is equivalent to \code{as_units(quote(m/s))}.
#'     \item \code{set_units}, a pipe-friendly version of \code{`units<-`}. By
#'     default it operates with bare expressions, but this
#'     behavior can be disabled by a specifying \code{mode = "standard"} or setting
#'     \code{units_options(set_units_mode = "standard")}.
#'     If \code{value} is missing or set to \code{1}, the object becomes unitless.
#' }
#'
#' @param x numeric vector, or object of class \code{units}.
#' @param value object of class \code{units} or \code{symbolic_units}, or in the
#' case of \code{set_units} expression with symbols (see examples).
#'
#' @return An object of class \code{units}.
#'
#' @details
#' If \code{value} is of class \code{units} and has a value unequal to 1, this
#' value is ignored unless \code{units_options("simplifiy")} is \code{TRUE}. If
#' \code{simplify} is \code{TRUE}, \code{x} is multiplied by this value.
#'
#' @export
#' @name units
#'
#' @examples
#' x = 1:3
#' class(x)
#' units(x) <- as_units("m/s")
#' class(x)
#' y = 2:5
`units<-.numeric` <- function(x, value) {
  if(!length(value))
    return(x)

  storage.mode(x) <- "double" # issues/324

  if(!inherits(value, "units") && !inherits(value, "symbolic_units"))
    value <- as_units(value)

  if (inherits(value, "units")) {
	if (any(is.na(value)))
	  stop("a missing value for units is not allowed")
  if (isTRUE(.units.simplify()))
    x <- x * unclass(value)
	else if (any(unclass(value) != 1.0))
	  warning(paste("numeric value", unclass(value), "is ignored in unit assignment"))
    value <- units(value)
  }

  attr(x, "units") = value
  class(x) <- "units"
  x
}

#' @name units
#' @export
#'
#' @examples
#' a <- set_units(1:3, m/s)
#' units(a) <- make_units(km/h)
#' a
#' # convert to a mixed_units object:
#' units(a) <- c("m/s", "km/h", "km/h")
#' a
`units<-.units` <- function(x, value) {
  if(!length(value))
    return(drop_units(x))

  if(!inherits(value, "units") && !inherits(value, "symbolic_units")) {
    if ((is.character(value) && length(value) > 1))
      return(set_units(mixed_units(x), value))
    value <- as_units(value)
  }

  dimx = dim(x)
  if (inherits(value, "units")) {
    if (!identical(as.numeric(value), 1))
      x <- .as.units(unclass(x) * unclass(value), units(x))
    value <- units(value)
  }

  if (identical(units(x), value)) # do nothing; possibly user-defined units:
    return(x)

  str1 <- ud_char(units(x))
  str2 <- ud_char(value)

  if (ud_are_convertible(str1, str2))
    .as.units(ud_convert(unclass(x), str1, str2), value, dim = dimx)
  else
    stop(paste("cannot convert", units(x), "into", value), call. = FALSE)
}

#unit_ambiguous = function(value) {
#  msg = paste("ambiguous argument:", value, "is interpreted by its name, not by its value")
#  warning(msg, call. = FALSE)
#}


#' @name units
#' @export
`units<-.logical` <- function(x, value) {
  if (!all(is.na(x)))
    stop("x must be numeric, non-NA logical not supported")

  # x <- as.numeric(x) ## see https://github.com/r-quantities/units/issues/413
  storage.mode(x) = "numeric"
  units(x) <- value
  x
}

#' @return The \code{units} method retrieves the units attribute, which is of
#' class \code{symbolic_units}.
#'
#' @name units
#' @export
units.units <- function(x) {
  attr(x, "units")
}

#' @name units
#' @export
units.symbolic_units <- function(x) {
  x
}

#' @export
as.data.frame.units <- function(x, row.names = NULL, optional = FALSE, ...) {
	df = as.data.frame(unclass(x), row.names, optional, ...)
	if (!optional && ncol(df) == 1)
	  colnames(df) <- deparse(substitute(x))
	dfapply(df, as_units, units(x))
}

#' @export
as.list.units <- function(x, ...)
  lapply(NextMethod(), set_units, units(x), mode="standard")

#' convert units object into difftime object
#'
#' @param x object of class \code{units}
#'
#' @export
#' @examples
#'
#' t1 = Sys.time()
#' t2 = t1 + 3600
#' d = t2 - t1
#' du <- as_units(d)
#' dt = as_difftime(du)
#' class(dt)
#' dt
as_difftime <- function(x) {
  stopifnot(inherits(x, "units"))
  u <- as.character(units(x))
  if (u == "s")
    as.difftime(x, units = "secs")
  else if (u == "min")
    as.difftime(x, units = "mins")
  else if (u == "h")
    as.difftime(x, units = "hours")
  else if (u == "d")
    as.difftime(x, units = "days")
  else
    stop(paste("cannot convert unit", u, "to difftime object"))
}

# #' Convert units to hms
# #'
# #' Convert units to hms
# #' @param x object of class units
# #' @param ... passed on to as.hms.difftime
# #' @return object of class hms
# #' @examples
# #' if (require(hms)) {
# #'  as.hms(1:10 * make_units(s))
# #'  as.hms(1:10 * make_units(min))
# #'  as.hms(1:10 * make_units(h))
# #'  as.hms(1:10 * make_units(d))
# #' }
# #' @export
# as.hms.units = function(x, ...) {
# 	hms::as.hms(as_difftime(x), ...)
# }


#' @export
`[.units` <- function(x, i, j, ..., drop = TRUE) {
  restore_units(NextMethod(), x)
}

#' @export
`[[.units` <- function(x, i, j, ...) {
  restore_units(NextMethod(), x)
}

#' @export
as.POSIXct.units = function (x, tz = "UTC", ...) {
	units(x) = symbolic_unit("seconds since 1970-01-01 00:00:00 +00:00")
	as.POSIXct.numeric(as.numeric(x), tz = tz, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
}

#' @method as.Date units
#' @export
as.Date.units = function (x, ...) {
	units(x) = symbolic_unit("days since 1970-01-01")
	as.Date(as.numeric(x), origin = as.Date("1970-01-01 00:00:00"))
}

#' @param ... passed on to other methods.
#' @param mode if \code{"symbols"} (the default), then unit is constructed from
#'   the expression supplied. Otherwise, if\code{mode = "standard"},
#'   standard evaluation is used for the supplied value This argument can be set
#'   via a global option \code{units_options(set_units_mode = "standard")}
#'
#' @name units
#' @export
set_units <- function(x, value, ..., mode = units_options("set_units_mode"))
  UseMethod("set_units")

#' @export
set_units.numeric <- function(x, value, ..., mode = units_options("set_units_mode")) {
  if (missing(value))
    value <- unitless
  else if (mode == "symbols") {
    value <- substitute(value)
    if(is.numeric(value) && !identical(value, 1) && !identical(value, 1L))
      stop("The only valid number defining a unit is '1', signifying a unitless unit")
    if (is.name(value) || is.call(value))
      value <- format(value)
  }

  units(x) <- as_units(value, ...)
  x
}

#' @export
set_units.logical <- set_units.numeric

#' @export
set_units.units <- set_units.numeric

#' Drop Units
#'
#' Drop units attribute and class.
#'
#' @param x an object with units metadata.
#'
#' @return the numeric without any units attributes, while preserving other
#' attributes like dimensions or other classes.
#'
#' @details Equivalent to \code{units(x) <- NULL}, or the pipe-friendly version
#' \code{set_units(x, NULL)}, but \code{drop_units} will fail if the object has
#' no units metadata. Use the alternatives if you want this operation to succeed
#' regardless of the object type.
#'
#' A \code{data.frame} method is also provided, which checks every column and
#' drops units if any.
#'
#' @export
#' @examples
#' x <- 1
#' y <- set_units(x, m/s)
#'
#' # this succeeds
#' drop_units(y)
#' set_units(y, NULL)
#' set_units(x, NULL)
#'
#' \dontrun{
#' # this fails
#' drop_units(x)
#' }
#'
#' df <- data.frame(x=x, y=y)
#' df
#' drop_units(df)
#'
drop_units <- function(x) UseMethod("drop_units")

#' @name drop_units
#' @export
drop_units.units <- function(x) {
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}

#' @name drop_units
#' @export
drop_units.data.frame <- function(x) {
  dfapply(x, function(i) if (inherits(i, "units")) drop_units(i) else i)
}
