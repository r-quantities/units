#' Define new symbolic units
#'
#' Adding a symbolic unit allows it to be used in \code{as_units},
#' \code{make_units} and \code{set_units}. No installation is performed if the
#' unit is already known by udunits.
#'
#' @param chr a length 1 character vector that is the new unit name or symbol.
#' @param warn warns if the supplied unit symbol is already a valid unit symbol
#'   recognized by udunits.
#'
#' @export
#' @rdname install_symbolic_unit
install_symbolic_unit <- function(chr, warn = TRUE) {
  if(ud.is.parseable(chr)) {
    if (warn) 
      warning(sQuote(chr), " is already a valid unit recognized by udunits.\n",
              "Installation not necessary and is not performed")
    return(invisible(FALSE))
  }
  R_ut_new_dimensionless_unit(chr)
  #assign(chr, NULL, envir =  user_defined_units)
}

#' Install a conversion constant between user-defined units.
#' 
#' @description Tells the \code{units} package how to convert between units that
#'   have a linear relationship, i.e. can be related on the form \eqn{y = \alpha
#'   x}.
#'   
#' @param from    String for the symbol of the unit being converted from.
#' @param to      String for the symbol of the unit being converted to; must be a non-existing unit name.
#' @param const   The constant \eqn{\alpha} in the conversion.
#'   
#' @details This function handles the very common case where units are related 
#'   through a linear function, that is, you can convert from one to the other 
#'   as \eqn{y = \alpha x}. Using this function, you specify that you
#'   can go from values of type \code{from} to values of type \code{to} by 
#'   multiplying by a constant.
#'   
#' @examples 
#' 
#' # one orange is worth two apples
#' install_symbolic_unit("orange")
#' install_conversion_constant("orange", "apple", 2) # apple = 2 * orange
#' apples <- 2 * as_units("apple")
#' oranges <- 1 * as_units("orange")
#' apples + oranges
#' oranges + apples
#' 
#' @export
install_conversion_constant <- function(from, to, const) {
  stopifnot(is.finite(const), const != 0.0)
  R_ut_scale(as.character(to), as.character(from), 1.0 / as.double(const))
}
