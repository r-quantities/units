
#' Deprecated functions
#' 
#' The following functions are deprecated and will be removed in a future release.
#' 
#' @param chr length 1 character string
#' 
#' @export
#' @rdname deprecated
make_unit <- function(chr) {
  ## .Deprecated("as_units", msg = "make_unit() is deprecated. Please use as_units()")
  #   message("make_unit will be replaced by as_unit, and then be deprecated; please use as_units")
  if(!is_valid_unit_symbol(chr))
    warning(paste(sQuote(chr), "is not a valid unit symbol recognized by udunits"), call.=FALSE)
  as_units.character(chr, force_single_symbol = TRUE, check_is_valid = FALSE)
}

#' @export
#' @rdname deprecated
parse_unit <- function(chr) {
  .Deprecated("as_units", msg = "parse_unit() is deprecated. Please use as_units()")
  as_units.character(chr, implicit_exponents = TRUE)
}

#' @export
#' @name deprecated
#' @param x a numeric
#' @param value a units object, by default, unitless
as.units <- function(x, value = unitless) {
  .Deprecated("as_units")    # nocov
  as_units(x, value = value) # nocov
}


