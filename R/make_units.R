#' Unit creation
#'
#' A number of functions are provided for creating unit objects. \itemize{
#'
#' \item \code{as_units}, a generic \code{as_units}, with methods for a
#' character string and for quoted language. Note, direct usage of this funciton
#' by users is typically not necessary, as coersion via \code{as_units} is
#' automatically done with \code{`units<-`} and \code{set_units()}.
#'
#' \item \code{make_units()}, which only accepts only bare expressions.
#' \code{make_unit{m/s}} is equivelant to \code{as_units(quote(m/s))}
#'
#' \item \code{symbolic_unit()} for creation of a single symbolic unit
#' \code{symbolic_units("kg")}.
#'
#' \item \code{set_units()}, a pipe_friendly version of \code{`units<-`}. By
#' default it operates with bare expressions like \code{make_unit}, but this
#' behavior can be disabled by a specifying \code{mode = "standard"} or setting 
#' \code{options(units.set_units_mode = "standard")}.
#'
#' }
#' 
#' @rdname make_units
#' 
#' @param x a bare R expression describing units. Must be valid R syntax
#'   (reserved R syntax words like \code{in} must be backticked)
#' @param allow_user_defined If FALSE (the default), an error is thrown if any
#'   of the symbols that makeup the units expression are not recognized by the
#'   udunits database. See details.
#' @param auto_convert_names_to_symbols Automatically attempt conversion of
#'   names to symbols in the supplied unit expression.
#'
#' @details In \code{make_unit()}, \code{set_units()}, and
#'   \code{as_units()}, each of the symbols in the expression is treated as a
#'   symbolic unit, as returned by \code{symbolic_unit}. By default, each of the
#'   symbols must be recognized by the udunits database. To see which symbols
#'   and names are currently recognized by the database, see
#'   \code{udunits_symbols()}. If \code{allow_user_defined = TRUE} and an
#'   unrecognized unit is found in the expression, a valid \code{units} object
#'   is still returned, with a warning. To avoid the warning with user defined
#'   units, use \code{symbolic_unit(...,user_defined = TRUE)}
#'   
#' @return A new unit object that can be used in arithmetics or unit conversion or unit assignment. 
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
#' # these different ways of creating the same unit:
#' # meters per second squared, i.e, acceleration
#' x1 <- make_units(m/s^2)
#' x2 <- as_units(quote(m/s^2))
#' x2 <- as_units("m/s^2")
#' x3 <- as_units("m s-2") # in product power form, i.e., implicit exponents = T
#' x4 <- set_units(1,  m/s^2) # by default, mode = "symbols"
#' x5 <- set_units(1, "m/s^2",   mode = "standard")
#' x6 <- set_units(1, x1,        mode = "standard")
#' x7 <- set_units(1, units(x1), mode = "standard")
#' x8 <- symbolic_unit("m") / symbolic_unit("s")^2
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
#' # necessary, since coersion is automatically done via as_units(). Again, 
#' # these are all equivelant ways to generate the same result.
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
#' # udunits2::ud.set.encoding("latin1")
#' # not all unit names get converted to symbols under different encodings
#'
#' ## Arithmetic operations and units
#' # conversion between unit objects that were defined as symbols and names will
#' # work correctly, although unit simplification in printing may not always occur.
#' x <- 500 * make_units(micrograms/liter)
#' y <- set_units(200, ug/l)
#' x + y
#' x * y # numberic result is correct, but units not simplified completely
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
#' define_new_symbolic_unit("cells")
#' make_units(cells/ml) 
#' # Note, define_new_symbolic_unit() does not add any support for unit
#' converstion, or arathmetic operations that require unit conversion. See
#' ?install_conversion_function for how to define relationships for user defined
#' units.
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
#' set_units(1:5, make_unit(m/s), mode = "standard")
#' 
#' # the mode of set_units() can be controlled via a global option
#' # options(units.set_units_mode = "standard")
NULL






backtick <- function(x) {
  # backtick all character runs uninterupted by one of ^()*^/`- or a space
  # don't double up backticks
  x <- gsub("`?([^() \\*^/`-]+)`?", "`\\1`", x)
  
  gsub("`([0-9]*\\.?[0-9]+)`", "\\1", x) # unbacktick bare numbers
}


are_exponents_implicit <- function(s) {
  s <- trimws(s)
  has <- function(chr, fixed = TRUE) grepl(chr, s, fixed = fixed)
  !has("^") & !has("*") & !has("/") & has("\\s", fixed = FALSE)
}

is_udunits_time <- function(s) {
  ud.is.parseable(s) && 
    ud.are.convertible(s, "seconds since 1970-01-01")
}

#' @rdname make_unit
#' @param implicit_exponents If the unit string is in product power form (e.g.
#'   \code{"km m-2 s-1"}). Defaults to \code{NA}, in which case a guess is made
#'   based on the supplied string. Set to \code{TRUE} or {FALSE} if the guess is
#'   incorrect.
#'   
#' @details Generally speaking, there are 3 types of unit strings are accepted
#'   in \code{as_units} (and by extension, \code{`units<-`}).
#'
#'   The first, and likely most common, is a "standard" format unit
#'   specificaiton where the relationship between unit symbols or names is
#'   specified with arathmetic operators for division \code{/}, multiplication
#'   \code{*} and power exponents \code{^}, or other mathematical functions like
#'   \code{log()}. In this case, the string is parsed as an R expression via
#'   \code{parse(text = )} after backticking all symbols. A heuristic is used to
#'   perform backticking, such that any continuous set of characters
#'   uninterrupted by one of \code{()\*^-} are backticked (unless the character
#'   sequence consists solely of numbers \code{0-9}), with some care to not
#'   doubleup on pre-existing backticks. This heurestic appears to be quite
#'   robust, and works for units would otherwise not be valid R syntax. For
#'   example, percent (\code{"%"}), feet (\code{"'"}), inches (\code{"in"}), and
#'   Tesla (\code{"T"}) are all backticked and parsed correctly.
#'
#'   Nevertheless, for certain complex expressions, this backticking heurestic
#'   may give incorrect results.  If the string supplied failes to parse as an R
#'   expression, then the string is treated as a single symbolic unit and
#'   \code{symbolic_unit(chr)} is used as a fallback with a warning. In that
#'   case, automatic unit simplification may not work properly when performing
#'   operations on unit objects, but unit conversion and other Math operations
#'   should still give correct results so long as the unit string supplied
#'   returns \code{TRUE} for \code{udunits2::ud.is.parsable()}.
#'
#'   The second type of unit string accepted is one with implicit exponents. In
#'   this format, \code{/}, \code{`*`}, and \code{^}, may not be present in the
#'   string, and unit symbol or names must be separated by a space. Each unit
#'   symbol may optionally be followed by a single number, specifying the power.
#'   For example \code{"m2 s-2"} is equivelant to \code{"(m^2)*(s^-2)"}.
#'
#'   The third type of unit string format accepted is the special case of
#'   udunits time duration with a reference origin, for example \code{"hours
#'   since 1970-01-01 00:00:00"}. Note, that the handeling of time and calendar
#'   operations via the udunits library is subtly different from the way R
#'   handles date and time operations. This functionality is mostly exported for
#'   users that work with udunits units data, e.g., with NetCDF files. Users are
#'   otherwise encouraged to use \code{R}'s date and time functionality provided
#'   by \code{Date} and \code{POSIXt} classes.
#'
#' @export
#' @noMd
as_units.character <- function(chr,
                        implicit_exponents = NA,
                        allow_user_defined = FALSE,
                        auto_convert_names_to_symbols = 
                          getOption("units.auto_convert_names_to_symbols", TRUE),
                        auto_backtick = TRUE) {


  stopifnot(is.character(chr), length(chr) == 1)
  
  if(is_udunits_time(chr))
    return(symbolic_unit(chr))
  
  if(is.na(implicit_exponents))
    implicit_exponents <- are_exponents_implicit(chr)
  
  if(implicit_exponents)
    return(.parse_unit_with_implicit_exponents(chr)) 
  
  if(auto_backtick)
    chr <- backtick(chr)

  o <- try(expr <- parse(text = chr)[[1]], silent = TRUE)
  
  if(inherits(o, "try-error")) {
    warning("Could not parse expression: ", sQuote(chr), 
      ". Returning as a single symbolic_unit()", call. = FALSE)
    return(symbolic_unit(chr, user_defined = allow_user_defined, check_is_parsable = TRUE))
  }

  as_units.call(expr, allow_user_defined = allow_user_defined, 
              auto_convert_names_to_symbols = auto_convert_names_to_symbols)
}

#' @export
make_unit <- as_units.character



#' @param n a numeric to be assigned units, or a units object to have units
#'   converted.
#'   
#' @param un a \code{units} object, or something coercable to one with \code{as_units}
#'
#' @param ... see parameter \code{mode}
#' @param mode if \code{"symbols"} (the default), then \code{...} are passed on
#'   to \code{make_unit()}. If \code{"character"}, then \code{...} are passed on to
#'   \code{parse_unit()}. If \code{"units"}, then \code{...} must be a single
#'   object of class \code{units} or \code{.symbolic_units} and the value is
#'   directly assigned to \code{n} via \code{`units<-`}
#'
#' @export
#' @rdname make_unit
set_units <- function(n, ...) UseMethod("set_units")

#' @export
set_units.default <- function(n, un, ...,
  mode = getOption("units.set_units_mode", c("standard", "bare_symbols"))) {
  
  if (match.arg(mode) == "bare_symbols")
    un <- substitute(un)
  
  units(n) <- as_units(un, ...)
  n
}


#' @export
set_units.difftime <- function(n, value) {
  units(n) <- value
  n
}

#' @export
#' @param chr a scalar character string describing a unit.
#'
#' @param check_is_parsable check if the symbolic unit is recognized by the
#'   udunits2 database
#'
#' @param user_defined Create a custom unit that is recognized by the udunits2
#'   database. This argument is ignored if `check_is_parsable = FALSE`. If
#'   `check_is_parsable = TRUE` and `user_defined = TRUE`, a warning is issued
#'   in the case of an unrecognized units, otherwise, if `user_defined = FALSE`,
#'   an error is thrown.
#'
#' @param auto_convert_name_to_symbol Automatically convert a unit name to it's
#'   symobl. E.g., \code{kilogram} becomes \code{kg}. Note, conversion is not
#'   reliable if the unit name contains a prefixe. This is a limitation of the
#'   underlying \code{udunits2} package and may change in the future.
#'
#' @rdname make_unit
symbolic_unit <- function(chr, check_is_parsable = TRUE, user_defined = TRUE, 
                          auto_convert_name_to_symbol = TRUE) {
  stopifnot(is.character(chr), length(chr) == 1)
  if(check_is_parsable && !ud.is.parseable(chr)) {
    msg <- paste(sQuote(chr), "is not a unit recognized by udunits")
    fun <- if(isTRUE(user_defined)) warning else stop
    fun(msg, call. = FALSE)
  }
  if(auto_convert_name_to_symbol && ud.is.parseable(chr)) {
    sym <- ud.get.symbol(chr)
    if(nzchar(sym))
      chr <- sym
  }
  
  structure(1L, units = .symbolic_units(chr), class = "units")
}


# from package:TKutils, paste + pretty collapse
pc <- function(x) {
  lx <- length(x)
  switch( as.character(lx),
          "0" = character(), 
          "1" = x,
          "2" = paste0(x, collapse = " and "),
          # else
          paste0(
            paste0(x[-lx], collapse = ", "), ", and ", x[lx]))
}


.msg_units_not_recognized <- function(unrecognized_symbols, full_expr) {
    
  if (is.language(full_expr))
    full_expr <- deparse(full_expr)
  
  is_are <- if (length(unrecognized_symbols) > 1L) "are" else "is" 
  
  paste0("In ", sQuote(full_expr), ", ", 
    pc(sQuote(unrecognized_symbols)), " ", is_are, " not recognized by udunits")
}



as_units.call <- function(expr, 
  allow_user_defined = FALSE, 
  auto_convert_names_to_symbols = TRUE) {
  
  stopifnot(is.language(expr))
  
  vars <- all.vars(expr)
  if(!length(vars)) 
    return(structure(1L, units = unitless, class = "units"))
  
  parsable <- vapply(vars, ud.is.parseable, logical(1L))
  if(!all(parsable)) {
    msg <- .msg_units_not_recognized(vars[!parsable], expr)
    fun <- if (allow_user_defined) warning else stop
    fun(msg, call. = FALSE)
  }
  
  names(vars) <- vars
  tmp_env <- lapply(vars, symbolic_unit, check_is_parsable = FALSE, 
                    auto_convert_name_to_symbol = auto_convert_names_to_symbols)
  
  unit <- eval(expr, tmp_env, baseenv())
  
  if(as.numeric(unit) != 1) 
    warning(call. = FALSE,
"In ", sQuote(deparse(expr)), " the numeric multiplier ", sQuote(as.numeric(unit)), " was discarded. 
The returned unit object was coerced to a value of 1.
Use `install_conversion_constant()` to define a new unit that is a multiple of another unit.")
  
  structure(1L, units = units(unit), class = "units")
}

as_units.expression <- as_units.call  #function(x, ...) as_units.call(x[[1]], ...)
as_units.name       <- as_units.call

as_units.NULL <- function(x, ...) x


#' @export
drop_units <- function(x) {
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}
