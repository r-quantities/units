



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
#'   \code{"km m-2 s-1"}). Defaults to \code{FALSE}
#'
#' @param auto_backtick If \code{TRUE} (the default), symbols in the unit object
#'   are automatically backticked prior to parsing as an R expression. See
#'   details.
#'
#' @details If \code{auto_backtick = TRUE} in \code{parse_unit()}, symbols are
#'   automatically backticked prior to parsing the string as an R expression. A
#'   heuristic is used to perform backticking, such that any continuous set of
#'   characters uninterrupted by one of \code{()\*^-} are backticked (unless the
#'   character sequence consists solely of numbers \code{0-9}), with some care
#'   to not doubleup on pre-existing backticks. For certain expressions, this
#'   heurestic may give incorrect results.
#'
#'   If the string supplied to \code{parse_unit} failes to parse as an R
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

  .eval_units(expr, allow_user_defined = allow_user_defined, 
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
set_units <- function(n, un, ...) {
    units(n) <- as_units(un, ...)
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



.eval_units <- function(expr, 
  allow_user_defined = FALSE, 
  auto_convert_names_to_symbols = TRUE) {
  
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
                    auto_convert_name_to_symbol = auto_convert_names_to_symbols)
  
  unit <- eval(expr, tmp_env, baseenv())
  
  if(as.numeric(unit) != 1) 
    warning(call. = FALSE,
"In ", sQuote(deparse(expr)), " the numeric multiplier ", sQuote(as.numeric(unit)), " was discarded. 
The returned unit object was coerced to a value of 1.
Use `install_conversion_constant()` to define a new unit that is a multiple of another unit.")
  
  structure(1L, units = units(unit), class = "units")
}

#' @export
drop_units <- function(x) {
  class(x) <- setdiff(class(x), "units")
  attr(x, "units") <- NULL
  x
}
