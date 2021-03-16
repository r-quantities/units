# helper function:
.as.units = function(x, value, dim) {
  if (missing(dim))
    structure(x, units = value, class = "units")
  else
    structure(x, units = value, dim = dim, class = "units")
}

restore_units <- function(x, to) {
  attrs <- attributes(to)
  attrs$names <- NULL
  attrs$dim <- NULL
  attrs$dimnams <- NULL

  attributes(x)[names(attrs)] <- attrs
  x
}

#' @name units
#' @export
#'
#' @param bare_expression a bare R expression describing units. Must be valid R
#'   syntax (reserved R syntax words like \code{in} must be backticked)
#'
#' @examples
#' # The easiest way to assign units to a numeric vector is like this:
#' x <- y <- 1:4
#' units(x) <- "m/s"  # meters / second
#'
#' # Alternatively, the easiest pipe-friendly way to set units:
#' if(requireNamespace("magrittr", quietly = TRUE)) {
#'   library(magrittr)
#'   y %>% set_units(m/s)
#' }
#'
#' # these are different ways of creating the same unit:
#' # meters per second squared, i.e, acceleration
#' x1 <- make_units(m/s^2)
#' x2 <- as_units(quote(m/s^2))
#' x2 <- as_units("m/s^2")
#' x3 <- as_units("m s-2") # in product power form, i.e., implicit exponents = T
#' x4 <- set_units(1,  m/s^2) # by default, mode = "symbols"
#' x5 <- set_units(1, "m/s^2",   mode = "standard")
#' x6 <- set_units(1, x1,        mode = "standard")
#' x7 <- set_units(1, units(x1), mode = "standard")
#' x8 <- as_units("m") / as_units("s")^2
#'
#' all_identical <- function(...) {
#'   l <- list(...)
#'   for(i in seq_along(l)[-1])
#'     if(!identical(l[[1]], l[[i]]))
#'       return(FALSE)
#'   TRUE
#' }
#' all_identical(x1, x2, x3, x4, x5, x6, x7, x8)
#'
#' # Note, direct usage of these unit creation functions is typically not
#' # necessary, since coercion is automatically done via as_units(). Again,
#' # these are all equivalent ways to generate the same result.
#'
#' x1 <- x2 <- x3 <- x4 <- x5 <- x6 <- x7 <- x8 <- 1:4
#' units(x1) <- "m/s^2"
#' units(x2) <- "m s-2"
#' units(x3) <- quote(m/s^2)
#' units(x4) <- make_units(m/s^2)
#' units(x5) <- as_units(quote(m/s^2))
#' x6 <- set_units(x6, m/s^2)
#' x7 <- set_units(x7, "m/s^2", mode = "standard")
#' x8 <- set_units(x8, units(x1), mode = "standard")
#'
#' all_identical(x1, x2, x3, x4, x5, x6, x7, x8)
#'
#'
#' # Both unit names or symbols can be used. By default, unit names are
#' # automatically converted to unit symbols.
#' make_units(degree_C)
#' make_units(kilogram)
#' make_units(ohm)
#' # Note, if the printing of non-ascii characters is garbled, then you may
#' # need to specify the encoding on your system manually like this:
#' # ud_set_encoding("latin1")
#' # not all unit names get converted to symbols under different encodings
#'
#' ## Arithmetic operations and units
#' # conversion between unit objects that were defined as symbols and names will
#' # work correctly, although unit simplification in printing may not always occur.
#' x <- 500 * make_units(micrograms/liter)
#' y <- set_units(200, ug/l)
#' x + y
#' x * y # numeric result is correct, but units not simplified completely
#'
#' # note, plural form of unit name accepted too ('liters' vs 'liter'), and
#' # denominator simplification can be performed correctly
#' x * set_units(5, liters)
#'
#' # unit conversion works too
#' set_units(x, grams/gallon)
#'
#' ## Creating custom, user defined units
#' # For example, a microbiologist might work with counts of bacterial cells
#' # make_units(cells/ml) # by default, throws an ERROR
#' # First define the unit, then the newly defined unit is accepted.
#' install_unit("cells")
#' make_units(cells/ml)
#'
#' # Note that install_unit() adds support for defining relationships between
#' # the newly created symbols or names and existing units.
#'
#' ## set_units()
#' # set_units is a pipe friendly version of `units<-`.
#' if(requireNamespace("magrittr", quietly = TRUE)) {
#'   library(magrittr)
#'   1:5 %>% set_units(N/m^2)
#'   # first sets to m, then converts to km
#'   1:5 %>% set_units(m) %>% set_units(km)
#' }
#'
#' # set_units has two modes of operation. By default, it operates with
#' # bare symbols to define the units.
#' set_units(1:5, m/s)
#'
#' # use `mode = "standard"` to use the value of supplied argument, rather than
#' # the bare symbols of the expression. In this mode, set_units() can be
#' # thought of as a simple alias for `units<-` that is pipe friendly.
#' set_units(1:5, "m/s", mode = "standard")
#' set_units(1:5, make_units(m/s), mode = "standard")
#'
#' # the mode of set_units() can be controlled via a global option
#' # units_options(set_units_mode = "standard")
#'
#' # To remove units use
#' units(x) <- NULL
#' # or
#' set_units(x, NULL)
#' # or
#' drop_units(y)
make_units <- function(bare_expression, check_is_valid = TRUE) {
  as_units.call(substitute(bare_expression), check_is_valid = check_is_valid)
}

#' @name units
#' @export
as_units <- function(x, ...) {
  UseMethod("as_units")
}

#' @name units
#' @export
as_units.default <- function(x, value = unitless, ...) {
  if (is.null(x)) return(x)
  units(x) <- value
  x
}

#' @name units
#' @export
as_units.units <- function(x, value, ...) {
  if(!missing(value) && !identical(units(value), units(x)))
    warning("Use set_units() to perform unit conversion. Return unit unmodified")
  x
}

#' @name units
#' @export
as_units.symbolic_units <- function(x, value, ...) {
  if(!missing(value))
    warning("supplied value ignored")
  .as.units(1L, x)
}

#' @examples
#' s = Sys.time()
#' d  = s - (s+1)
#' as_units(d)
#'
#' @name units
#' @export
as_units.difftime <- function(x, value, ...) {
  u <- attr(x, "units")
  x <- unclass(x)
  attr(x, "units") <- NULL

  # convert from difftime to udunits2:
  if (u == "secs") # secs -> s
    x <- x * symbolic_unit("s")
  else if (u == "mins") # mins -> min
    x <- x * symbolic_unit("min")
  else if (u == "hours") # hours -> h
    x <- x * symbolic_unit("h")
  else if (u == "days") # days -> d
    x <- x * symbolic_unit("d")
  else if (u == "weeks") { # weeks -> 7 days
    x <- 7 * x
    x <- x * symbolic_unit("d")
  } else
    stop(paste("unknown time units", u, "in difftime object"))

  if (!missing(value)) # convert optionally:
    units(x) <- value

  x
}

#  ----- as_units.character helpers ------

backtick <- function(x) {
  # backtick all character runs uninterupted by one of ^()*^/`- or a space
  # don't double up backticks
  x <- gsub("`?([^() \\*^/`-]+)`?", "`\\1`", x)
  gsub("`([0-9]*\\.?[0-9]+)`", "\\1", x) # unbacktick bare numbers
}

are_exponents_implicit <- function(s) {
  s <- trimws(s)
  has <- function(chr, regex = FALSE)
    grepl(chr, s, fixed = !regex, perl = regex)
  !has("^") && !has("*") && !has("/") && has("\\s|\\D.*\\d$", regex = TRUE)
}

is_udunits_time <- function(s) {
  ud_is_parseable(s) && ud_are_convertible(s, "seconds since 1970-01-01")
}

#' @name units
#' @export
#'
#' @param force_single_symbol Whether to perform no string parsing and force
#'   treatment of the string as a single symbol.
#'
#' @param implicit_exponents If the unit string is in product power form (e.g.
#'   \code{"km m-2 s-1"}). Defaults to \code{NULL}, in which case a guess is made
#'   based on the supplied string. Set to \code{TRUE} or \code{FALSE} if the guess is
#'   incorrect.
#'
#' @section Character strings:
#'
#'   Generally speaking, there are 3 types of unit strings are accepted in
#'   \code{as_units} (and by extension, \code{`units<-`}).
#'
#'   The first, and likely most common, is a "standard" format unit
#'   specification where the relationship between unit symbols or names is
#'   specified explicitly with arithmetic symbols for division \code{/},
#'   multiplication \code{*} and power exponents \code{^}, or other mathematical
#'   functions like \code{log()}. In this case, the string is parsed as an R
#'   expression via \code{parse(text = )} after backticking all unit symbols and
#'   names, and then passed on to \code{as_units.call()}. A heuristic is used to
#'   perform backticking, such that any continuous set of characters
#'   uninterrupted by one of \code{()\\*^-} are backticked (unless the character
#'   sequence consists solely of numbers \code{0-9}), with some care to not
#'   double up on pre-existing backticks. This heuristic appears to be quite
#'   robust, and works for units would otherwise not be valid R syntax. For
#'   example, percent (\code{"\%"}), feet (\code{"'"}), inches (\code{"in"}),
#'   and Tesla (\code{"T"}) are all backticked and parsed correctly.
#'
#'   Nevertheless, for certain complex unit expressions, this backticking heuristic
#'   may give incorrect results.  If the string supplied fails to parse as an R
#'   expression, then the string is treated as a single symbolic unit and
#'   \code{symbolic_unit(chr)} is used as a fallback with a warning. In that
#'   case, automatic unit simplification may not work properly when performing
#'   operations on unit objects, but unit conversion and other Math operations
#'   should still give correct results so long as the unit string supplied
#'   returns \code{TRUE} for \code{ud_is_parsable()}.
#'
#'   The second type of unit string accepted is one with implicit exponents. In
#'   this format, \code{/}, \code{*}, and \code{^}, may not be present in the
#'   string, and unit symbol or names must be separated by a space. Each unit
#'   symbol may optionally be followed by a single number, specifying the power.
#'   For example \code{"m2 s-2"} is equivalent to \code{"(m^2)*(s^-2)"}.
#'
#'   It must be noted that prepended numbers are supported too, but their
#'   interpretation slightly varies depending on whether they are separated from
#'   the unit string or not. E.g., \code{"1000 m"} is interpreted as magnitude
#'   and unit, but \code{"1000m"} is interpreted as a prefixed unit, and it is
#'   equivalent to \code{"km"} to all effects.
#'
#'   The third type of unit string format accepted is the special case of
#'   udunits time duration with a reference origin, for example \code{"hours
#'   since 1970-01-01 00:00:00"}. Note, that the handling of time and calendar
#'   operations via the udunits library is subtly different from the way R
#'   handles date and time operations. This functionality is mostly exported for
#'   users that work with udunits time data, e.g., with NetCDF files. Users are
#'   otherwise encouraged to use \code{R}'s date and time functionality provided
#'   by \code{Date} and \code{POSIXt} classes.
#'
as_units.character <- function(x,
                               check_is_valid = TRUE,
                               implicit_exponents = NULL,
                               force_single_symbol = FALSE, ...) {

  stopifnot(is.character(x), length(x) == 1)

  if (isTRUE(x == "")) return(unitless)

  if(force_single_symbol || is_udunits_time(x))
    return(symbolic_unit(x, check_is_valid = check_is_valid))

  if(is.null(implicit_exponents))
    implicit_exponents <- are_exponents_implicit(x)

  if(implicit_exponents)
    x <- convert_implicit_to_explicit_exponents(x)

  x <- backtick(x)
  o <- try(expr <- parse(text = x)[[1]], silent = TRUE)

  if(inherits(o, "try-error")) {
    warning("Could not parse expression: ", sQuote(x),          # nocov
      ". Returning as a single symbolic unit()", call. = FALSE) # nocov
    return(symbolic_unit(x, check_is_valid = check_is_valid))   # nocov
  }

  as_units.call(expr, check_is_valid = check_is_valid)
}


convert_implicit_to_explicit_exponents <- function(x) {
  if (length(grep(c("[*/]"), x)) > 0)
    stop("If 'implicit_exponents = TRUE', strings cannot contain `*' or `/'")
  x <- gsub("\\b([^\\d-]+)([-]?\\d+)\\b", "\\1^(\\2)", x, perl =TRUE)
  x <- gsub("\\s+", " * ", trimws(x), perl = TRUE)
  x
}

#  ----- as_units.call helpers ------

# from package:yasp, paste collapse with serial (oxford) comma
pc_and <- function(..., sep = "") {
  x <- paste(..., sep = sep, collapse = NULL)
  lx <- length(x)
  if(lx == 0L)
    ""
  else if (lx == 1L)
    x
  else if (lx == 2L)
    paste0(x, collapse = " and ")
  else
    paste0( paste0(x[-lx], collapse = ", "), ", and ", x[lx])
}

#`%not_in%` <- function(x, table) match(x, table, nomatch = 0L) == 0L

.msg_units_not_recognized <- function(unrecognized_symbols, full_expr) {

  if (is.language(full_expr))
    full_expr <- deparse(full_expr)

  is_are <- if (length(unrecognized_symbols) > 1L) "are" else "is"

  paste0("In ", sQuote(full_expr), ", ",
    pc_and(sQuote(unrecognized_symbols)), " ", is_are, " not recognized by udunits.\n\n",
    "See a table of valid unit symbols and names with valid_udunits().\n",
    "Custom user-defined units can be added with install_unit().\n\n",
    "See a table of valid unit prefixes with valid_udunits_prefixes().\n",
    "Prefixes will automatically work with any user-defined unit.")
}

units_eval_env <- new.env(parent = baseenv())
units_eval_env$ln <- function(x) base::log(x)
units_eval_env$lg <- function(x) base::log(x, base = 10)
units_eval_env$lb <- function(x) base::log(x, base = 2)


#' @name units
#' @export
#'
#' @param check_is_valid throw an error if all the unit symbols are not either
#'   recognized by udunits2 via \code{ud_is_parseable()}, or a custom
#'   user defined via \code{install_unit()}. If \code{FALSE}, no check
#'   for validity is performed.
#'
#' @note By default, unit names are automatically substituted with unit names
#'   (e.g., kilogram --> kg). To turn off this behavior, set
#'   \code{units_options(auto_convert_names_to_symbols = FALSE)}
#'
#' @section Expressions:
#'
#'   In \code{as_units()}, each of the symbols in the unit expression is treated
#'   individually, such that each symbol must be recognized by the udunits
#'   database (checked by \code{ud_is_parseable()}, \emph{or} be a custom,
#'   user-defined unit symbol that was defined by \code{install_unit()}. To
#'   see which symbols and names are currently recognized by the udunits
#'   database, see \code{valid_udunits()}.
#'
#' @seealso \code{\link{install_unit}}, \code{\link{valid_udunits}}
as_units.call <- function(x, check_is_valid = TRUE, ...) {

  if(missing(x) || identical(x, quote(expr =)) ||
     identical(x, 1) || identical(x, 1L))
    return(.as.units(1, unitless))

  if (is.vector(x) && !is.expression(x) && any(is.na(x)))
  	stop("a missing value for units is not allowed")

  stopifnot(is.language(x))

  vars <- all.vars(x)
  if(!length(vars))
    stop(call. = FALSE,
"No symbols found. Please supply bare expressions with this approach.
See ?as_units for usage examples.")

  if (check_is_valid) {
    valid <- vapply(vars, ud_is_parseable, logical(1L))
    if (!all(valid))
      stop(.msg_units_not_recognized(vars[!valid], x), call. = FALSE)
  }

  names(vars) <- vars
  tmp_env <- lapply(vars, symbolic_unit, check_is_valid = FALSE)

  if (dont_simplify_here <- is.na(.units.simplify())) {
  	units_options(simplify = FALSE)
  	on.exit(units_options(simplify = NA))
  }

  unit <- tryCatch( eval(x, tmp_env, units_eval_env),
    error = function(e) stop( paste0( conditionMessage(e), "\n",
          "Did you try to supply a value in a context where a bare expression was expected?"
        ), call. = FALSE ))

#  if(as.numeric(unit) %not_in% c(1, 0)) # 0 if log() used.
#    stop(call. = FALSE,
#"In ", sQuote(deparse(x)), " the numeric multiplier ", sQuote(as.numeric(unit)), " is invalid.
#Use `install_unit()` to define a new unit that is a multiple of another unit.")

  .as.units(as.numeric(unit), units(unit))
}

#' @name units
#' @export
as_units.expression <- as_units.call

#' @name units
#' @export
as_units.name       <- as_units.call

#' @name units
#' @export
as_units.POSIXt = function(x, value, ...) {
  u = as.numeric(as.POSIXct(x))
  units(u) = symbolic_unit("seconds since 1970-01-01 00:00:00 +00:00")
  if (! missing(value))
    units(u) = symbolic_unit(value)
  u
}

#' @name units
#' @export
as_units.Date = function(x, value, ...) {
  u = as.numeric(x)
  units(u) = symbolic_unit("days since 1970-01-01")
  if (!missing(value))
    units(u) = symbolic_unit(value)
  u
}


symbolic_unit <- function(chr, check_is_valid = TRUE) {

  stopifnot(is.character(chr), length(chr) == 1)

  if (check_is_valid && !ud_is_parseable(chr)) {
    msg <- paste(sQuote(chr), "is not a unit recognized by udunits or a user-defined unit")
    stop(msg, call. = FALSE)
  }

  auto_convert <- units_options("auto_convert_names_to_symbols")
  if (auto_convert && ud_is_parseable(chr)) {
    sym <- ud_get_symbol(chr)
    if (length(sym))
      chr <- sym
  }

  .as.units(1, .symbolic_units(chr))
}
