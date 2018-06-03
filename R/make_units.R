#' Unit creation
#'
#' A number of functions are provided for creating unit objects. 
#' \itemize{
#'     \item \code{as_units}, a generic with methods for a
#'     character string and for quoted language. Note, direct usage of this function
#'     by users is typically not necessary, as coercion via \code{as_units} is
#'     automatically done with \code{`units<-`} and \code{set_units()}.
#'    
#'     \item \code{make_units()}, constructs units from bare expressions.
#'     \code{make_units(m/s)} is equivalent to \code{as_units(quote(m/s))}
#'    
#'     \item \code{set_units()}, a pipe_friendly version of \code{`units<-`}. By
#'     default it operates with bare expressions like \code{make_unit}, but this
#'     behavior can be disabled by a specifying \code{mode = "standard"} or setting 
#'     \code{units_options(set_units_mode = "standard")}.
#' }
#' 
#' @export
#' @rdname as_units
#' 
#' @param bare_expression a bare R expression describing units. Must be valid R
#'   syntax (reserved R syntax words like \code{in} must be backticked)
#'
#' @noMd
#' @examples
#' # The easiest way to assign units to a numeric vector is like this: 
#' x <- y <- 1:4
#' units(x) <- "m/s"  # meters / second
#' 
#' # Alternatively, the easiest pipe-friendly way to set units:
#' if(require(magrittr)) 
#'   y %>% set_units(m/s)
#' 
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
#' install_symbolic_unit("cells")
#' make_units(cells/ml) 
#' 
#' # Note, install_symbolic_unit() does not add any support for unit
#' # conversion, or arithmetic operations that require unit conversion. See
#' # ?install_conversion_constant for defining relationships between user 
#' # defined units.
#'
#' ## set_units()
#' # set_units is a pipe friendly version of `units<-`. 
#' if (require(magrittr)) {
#'  1:5 %>% set_units(N/m^2)
#'  # first sets to m, then converts to km
#'  1:5 %>% set_units(m) %>% set_units(km)
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
#' drop_units(y)
make_units <- function(bare_expression, check_is_valid = TRUE) {
  as_units.call(substitute(bare_expression), check_is_valid = check_is_valid)
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


#' @rdname as_units
#' @export
#' @noMd
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
#'   The third type of unit string format accepted is the special case of
#'   udunits time duration with a reference origin, for example \code{"hours
#'   since 1970-01-01 00:00:00"}. Note, that the handling of time and calendar
#'   operations via the udunits library is subtly different from the way R
#'   handles date and time operations. This functionality is mostly exported for
#'   users that work with udunits time data, e.g., with NetCDF files. Users are
#'   otherwise encouraged to use \code{R}'s date and time functionality provided
#'   by \code{Date} and \code{POSIXt} classes.
as_units.character <- function(x, 
                               check_is_valid = TRUE,
                               implicit_exponents = NULL, 
                               force_single_symbol = FALSE, ...) {

  stopifnot(is.character(x), length(x) == 1)
  
  if(force_single_symbol || is_udunits_time(x))
    return(symbolic_unit(x, check_is_valid = check_is_valid))
  
  if(is.null(implicit_exponents))
    implicit_exponents <- are_exponents_implicit(x)
  
  if(implicit_exponents)
    return(.parse_unit_with_implicit_exponents(x))
  
  x <- backtick(x)
  o <- try(expr <- parse(text = x)[[1]], silent = TRUE)
  
  if(inherits(o, "try-error")) {
    warning("Could not parse expression: ", sQuote(x), 
      ". Returning as a single symbolic unit()", call. = FALSE)
    return(symbolic_unit(x, check_is_valid = check_is_valid))
  }

  as_units.call(expr, check_is_valid = check_is_valid)
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

`%not_in%` <- function(x, table) match(x, table, nomatch = 0L) == 0L

.msg_units_not_recognized <- function(unrecognized_symbols, full_expr) {
    
  if (is.language(full_expr))
    full_expr <- deparse(full_expr)
  
  is_are <- if (length(unrecognized_symbols) > 1L) "are" else "is" 
  
  paste0("In ", sQuote(full_expr), ", ", 
    pc_and(sQuote(unrecognized_symbols)), " ", is_are, " not recognized by udunits.\n",
    "See a table of valid unit symbols and names with valid_udunits().\n", 
    "Add custom user-defined units with install_symbolic_unit().")
}

is_valid_unit_symbol <- function(chr) {
  ud_is_parseable(chr)
}

units_eval_env <- new.env(parent = baseenv())
units_eval_env$ln <- function(x) base::log(x)
units_eval_env$lg <- function(x) base::log(x, base = 10)
units_eval_env$lb <- function(x) base::log(x, base = 2)


#' @export
#' @rdname as_units
#'
#' @param check_is_valid throw an error if all the unit symbols are not either
#'   recognized by udunits2 via \code{ud_is_parseable()}, or a custom
#'   user defined via \code{install_symbolic_unit()}. If \code{FALSE}, no check
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
#'   user-defined unit symbol that was defined either by
#'   \code{install_symbolic_unit()} or \code{install_conversion_constant()}. To
#'   see which symbols and names are currently recognized by the udunits
#'   database, see \code{udunits_symbols()}.
#'
#' @return A new unit object that can be used in arithmetic, unit conversion or
#'   unit assignment.
#'
#' @seealso \code{\link{valid_udunits}}
as_units.call <- function(x, check_is_valid = TRUE, ...) {
  
  if(missing(x) || identical(x, quote(expr =)) || 
     identical(x, 1) || identical(x, 1L))
    return(structure(1, units = unitless, class = "units"))
  
  stopifnot(is.language(x))
  
  vars <- all.vars(x)
  if(!length(vars))
    stop(call. = FALSE,
"No symbols found. Please supply bare expressions with this approach.
See ?as_units for usage examples.")
  
  if (check_is_valid) {
    valid <- vapply(vars, is_valid_unit_symbol, logical(1L))
    if (!all(valid))
      stop(.msg_units_not_recognized(vars[!valid], x), call. = FALSE)
  }
  
  names(vars) <- vars
  tmp_env <- lapply(vars, symbolic_unit, check_is_valid = FALSE)
 
  if (dont_simplify_here <- is.na(.units.simplify()))
  	units_options(simplify = FALSE)
  
  unit <- tryCatch( eval(x, tmp_env, units_eval_env),
    error = function(e) stop( paste0( conditionMessage(e), "\n",
          "Did you try to supply a value in a context where a bare expression was expected?"
        ), call. = FALSE ))

  if (dont_simplify_here)
  	units_options(simplify = NA)
  
#  if(as.numeric(unit) %not_in% c(1, 0)) # 0 if log() used. 
#    stop(call. = FALSE,
#"In ", sQuote(deparse(x)), " the numeric multiplier ", sQuote(as.numeric(unit)), " is invalid. 
#Use `install_conversion_constant()` to define a new unit that is a multiple of another unit.")
  
  structure(as.numeric(unit), units = units(unit), class = "units")
}


#' @export
as_units.expression <- as_units.call

#' @export
as_units.name       <- as_units.call


symbolic_unit <- function(chr, check_is_valid = TRUE) {
  
  stopifnot(is.character(chr), length(chr) == 1)
  
  if (check_is_valid && !is_valid_unit_symbol(chr)) {
    msg <- paste(sQuote(chr), "is not a unit recognized by udunits or a user-defined unit")
    stop(msg, call. = FALSE)
  }
  
  auto_convert <- units_options("auto_convert_names_to_symbols")
  if (auto_convert && ud_is_parseable(chr)) {
    sym <- ud_get_symbol(chr)
    if (nzchar(sym)) 
      chr <- sym
  }
  
  structure(1, units = .symbolic_units(chr), class = "units")
}


#' drop units
#' 
#' @param x a units object
#' 
#' @return the numeric without any units attributes, while preserving other
#'   attributes like dimensions or other classes.
#'   
#' @note Equivalent to \code{units(x) <- NULL}
#' 
#' @export
drop_units <- function(x) UseMethod("drop_units")

#' @export
drop_units.units <- function(x) {
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}

