
#' deprecated functions
#' 
#' The following functions are deprecated and will be removed in a future release.
#' 
#' @export
#' @rdname deprecated
make_unit <- function(x, ...) {
  warning("make_unit() is deprecated. Please use symbolic_unit()")
  symbolic_unit(x, ...)
}


#' Unit creation
#'
#' Four functions are provided for creating \code{units} objects: 
#' \itemize{ 
#'    \item \code{make_units()} accepts a bare expression: \code{make_units(kg*m/s^2)}
#'    \item \code{parse_units()} accepts a string: \code{parse_units("kg*m/s^2")}
#'    \item \code{set_units()} a pipe friendly version of \code{`units<-`}. Accepts a unit object or a bare expression.
#'    \item \code{symbolic_unit()} for creation of a single symbolic unit: \code{symbolic_units("kg")}
#'    }
#'
#' @param x a bare R expression describing units. Must be valid R syntax (reserved R syntax words like \code{in} must be backticked)
#' @param allow_user_defined If FALSE (the default), an error is thrown if any
#'   of the symbols that makeup the units expression are not recognized by the
#'   udunits database. See details.
#'
#' @details In \code{make_units()}, \code{set_units()}, and
#'   \code{parse_units()}, each of the symbols in the expression is treated as a
#'   symbolic unit, as returned by \code{symbolic_unit}. By default, each of the
#'   symbols must be recognized by the udunits database. To see which symbols
#'   and names are currently recognized by the database, see
#'   \code{udunits_symbols()}. If \code{allow_user_defined = TRUE} and an
#'   unrecognized unit is found in the expression, a valid \code{units} object
#'   is still returned, with a warning. To avoid the warning with user defined
#'   units, use \code{symbolic_unit(...,user_defined = TRUE)}
#'   
#' @return A new unit object that can be used in arithmetics
#' @export
#' @noMd
#' @examples
#' # these calls all return identical objects
#' # meters per second squared, i.e, acceleration
#' x1 <- make_units(m/s^2)
#' x2 <- parse_units("m/s^2")
#' x3 <- set_units(1, m/s^2)
#' x4 <- set_units(1, x1)
#' x5 <- set_units(1, units(x1))
#' x6 <- symbolic_unit("m") / symbolic_unit("s")^2
#' 
#' all_identical <- function(...) {
#'   l <- list(...)
#'   for(i in seq_along(l)[-1])
#'     if(!identical(l[[1]], l[[i]]))
#'       return(FALSE)
#'   TRUE
#' }
#' all_identical(x1, x2, x3, x4, x5, x6)
#' 
#' # Both full unit names or symbols can be used. Arithmetic operations and unit
#' conversion between # unit objects that were defined as symbols and names will
#' work correctly, # although unit simplification in printing may not always occur.
#' x <- 500 * make_units(micrograms/liter)
#' y <- set_units(200, ug/l)
#' x + y
#' x * y # numberic result is correct, but units not simplified
#' 
#' # note, plural form of unit name accepted too ('liters' vs 'liter'), and 
#' # denominator simplification can is performed correctly 
#' x * set_units(5, liters)
#' 
#' # unit conversion works too
#' set_units(x, grams/gallon)
#' 
#' # Creating custom, user defined units
#' # For example, a microbiologist might work with counts of bacterial cells
#' # make_units(cells/ml) # by default, throws an ERROR
#' make_units(cells/ml, allow_user_defined = TRUE) # throws a warning instead
#' # alternatively, create the custom unit separately
#' cells <- symbolic_unit("cells", check_is_parsable = FALSE)
#' ml <- make_units(ml)
#' cells/ml
#' 
#' # set_units is just a pipe friendly version of `units<-`
#' note that these units have NOT been defined or declared before:
#' set_units(1:5, N/m^2)
#' set_units(1:5, unitless) # unit "1", unitless
#' if (require(magrittr)) {
#'  1:5 %>% set_units(N/m^2)
#'  
#'  # first sets to m, then converts to km
#'  1:10 %>% set_units(m) %>% set_units(km) 
#' }
make_units <- function(x, allow_user_defined = FALSE, 
                       auto_convert_name_to_symbol = TRUE) {
  .eval_units(substitute(x), 
              allow_user_defined = allow_user_defined, 
              auto_convert_name_to_symbol = auto_convert_name_to_symbol)
}

backtick <- function(x) {
  # backtick all character runs uninterupted by one of ^()*^/`- or a space
  # don't double up backticks
  x <- gsub("`?([^() \\*^/`-]+)`?", "`\\1`", x)
  
  gsub("`([0-9]*\\.?[0-9]+)`", "\\1", x) # unbacktick bare numbers
}

#' @rdname make_units
#' @param implicit_exponents If the unit string is in product power form (e.g.
#'   \code{"km m-2 s-1"}). Defaults to \code{FALSE}
#'
#' @param auto_backtick If \code{TRUE} (the default), symbols in the unit object
#'   are automatically backticked prior to parsing as an R expression. See
#'   details.
#'
#' @details If \code{auto_backtick = TRUE} in \code{parse_units()}, symbols are
#'   automatically backticked prior to parsing the string as an R expression. A
#'   heuristic is used to perform backticking, such that any continuous set of
#'   characters uninterrupted by one of \code{()\*^-} are backticked (unless the
#'   character sequence consists solely of numbers \code{0-9}), with some care
#'   to not doubleup on pre-existing backticks. For certain expressions, this
#'   heurestic may give incorrect results.
#'
#'   If the string supplied to \code{parse_units} failes to parse as an R
#'   expression (via \code{parse(text = chr)}), then the string is treated as a
#'   single symbolic unit and \code{symbolic_unit(chr)} is used as a fallback
#'   with a warning. Note, in that case, automatic unit simplification may not
#'   work properly when performing operations on unit objects, but unit
#'   conversion and other Math operations should still give correct results so
#'   long as the unit string supplied returns \code{TRUE} for
#'   \code{udunits2::ud.is.parsable()}
#'
#' @export
#' @noMd
parse_units <- function(chr,
                        implicit_exponents = FALSE,
                        allow_user_defined = FALSE,
                        auto_convert_name_to_symbol = TRUE,
                        auto_backtick = TRUE) {


  stopifnot(is.character(chr), length(chr) == 1)
  if(implicit_exponents)
    return(.parse_units_with_implicit_exponents(chr)) # current parse_unit()

  if(auto_backtick)
    chr <- backtick(chr)

  o <- try(expr <- parse(text = chr)[[1]], silent = TRUE)
  if(inherits(o, "try-error")) {
    warning("Could not parse expression: ", sQuote(chr), 
      ". Returning as a single symbolic_unit()", call. = FALSE)
    return(symbolic_unit(chr, user_defined = allow_user_defined, check_is_parsable = TRUE))
  }

  .eval_units(expr, allow_user_defined = allow_user_defined, 
              auto_convert_name_to_symbol = auto_convert_name_to_symbol)
}



#' @param n a numeric to be assigned units, or a units object to have units
#'   converted.
#'   
#' @details In \code{set_units()}, standard evaluation of the supplied argument
#'   \code{un} is attempted first. If the result is not a units object, then the
#'   bare expression that was typed into the function call is used to create a
#'   new units object. For this reason, using \code{set_units()} with a bare
#'   expression should be avoided in R packages and other situations where the
#'   evaluation environemnt isn't always known in advance, because the supplied
#'   expression may unexpectantly evaluate to a valid object, in which case the
#'   expression itself will not be used
#' @export
#' @rdname make_units
set_units <- function(n, ...) {
  units(n) <- make_units(...)
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
#' @rdname make_units
symbolic_unit <- function(chr, check_is_parsable = TRUE, user_defined = TRUE, 
                          auto_convert_name_to_symbol = TRUE) {
  stopifnot(is.character(chr), length(chr) == 1)
  if(check_is_parsable && !udunits2::ud.is.parseable(chr)) {
    msg <- paste(sQuote(chr), "is not a unit recognized by udunits")
    fun <- if(isTRUE(user_defined)) warning else stop
    fun(msg, call. = FALSE)
  }
  if(auto_convert_name_to_symbol && ud.is.parseable(chr)) {
    sym <- ud.get.symbol(chr)
    if(nzchar(sym))
      chr <- sym
  }
  
  as_units.default(1, .symbolic_units(chr))
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



.eval_units <- function(expr, 
  allow_user_defined = FALSE, 
  auto_convert_name_to_symbol = TRUE) {
  
  stopifnot(is.language(expr))
  
  vars <- all.vars(expr)
  if(!length(vars)) return(as_units(1, unitless))
  
  parsable <- vapply(vars, ud.is.parseable, logical(1L))
  if(!all(parsable)) {
    msg <- .msg_units_not_recognized(vars[!parsable], expr)
    fun <- if (allow_user_defined) warning else stop
    fun(msg, call. = FALSE)
  }
  
  names(vars) <- vars
  tmp_env <- lapply(vars, symbolic_unit, check_is_parsable = FALSE, 
                    auto_convert_name_to_symbol = auto_convert_name_to_symbol)
  
  eval(expr, tmp_env, baseenv())
}
