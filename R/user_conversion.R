#' Define new symbolic units
#'
#' Adding a symbolic unit allows it to be used in \code{as_units},
#' \code{make_units} and \code{set_units}. No installation is performed if the
#' unit is already known by udunits.
#'
#' @param name a length 1 character vector that is the unit name or symbol.
#' @param warn warns if the supplied unit symbol is already a valid unit symbol
#'   recognized by udunits.
#' @param dimensionless logical; if \code{TRUE}, a new dimensionless unit is created, if \code{FALSE} a new base unit is created. Dimensionless units are convertible to other dimensionless units (such as \code{rad}), new base units are not convertible to other existing units.
#'
#' @details \code{install_symbolic_unit} installs a new dimensionless unit; these are directly compatible to any other dimensionless unit. To install a new unit that is a scaled or shifted version of an existing unit, use \code{install_conversion_constant} or \code{install_conversion_offset} directly.
#' @export
#' @rdname install_symbolic_unit
#' @seealso \code{\link{install_conversion_constant}}, \code{\link{install_conversion_offset}}
#' @examples
#' install_symbolic_unit("person")
#' set_units(1, rad) + set_units(1, person) # that is how dimensionless units work!
install_symbolic_unit <- function(name, warn = TRUE, dimensionless = TRUE) {
  if(ud_is_parseable(name)) {
    if (warn) 
      warning(sQuote(name), 
	    " is already a valid unit recognized by udunits; removing and reinstalling.")
    remove_symbolic_unit(name)
  }
  if (dimensionless)
  	R_ut_new_dimensionless_unit(name)
  else
  	R_ut_new_base_unit(name)
}

#' @export
#' @rdname install_symbolic_unit
remove_symbolic_unit <- function(name) {
	R_ut_remove_unit(name)
}

#' Install a conversion constant or offset between user-defined units.
#' 
#' @description Tells the \code{units} package how to convert between units that
#'   have a linear relationship, i.e. can be related on the form \eqn{y = \alpha
#'   x} (constant) or \eqn{y = \alpha + x} (offset).
#'   
#' @param from    String for the symbol of the unit being converted from.
#' @param to      String for the symbol of the unit being converted to. One of \code{from} and \code{to} must be an existing unit name.
#' @param const   The constant \eqn{\alpha} in the conversion.
#'   
#' @details This function handles the very common case where units are related 
#'   through a linear function, that is, you can convert from one to the other 
#'   as \eqn{y = \alpha x}. Using this function, you specify that you
#'   can go from values of type \code{from} to values of type \code{to} by 
#'   multiplying by a constant, or adding a constant.
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
#' @seealso \code{\link{install_symbolic_unit}}, \code{\link{remove_symbolic_unit}}
install_conversion_constant <- function(from, to, const) {
  stopifnot(is.finite(const), const != 0.0)
  if (! xor(ud_is_parseable(from), ud_is_parseable(to)))
    stop("exactly one of (from, to) must be a known unit")
  if (ud_is_parseable(to))
  	R_ut_scale(as.character(from), as.character(to), as.double(const))
  else
    R_ut_scale(as.character(to), as.character(from), 1.0 / as.double(const))
}

#' @export
#' @name install_conversion_constant 
#' @examples
#' install_conversion_offset("meter", "newmeter", 1)
#' m = set_units(1:3, meter)
#' n = set_units(1:3, newmeter)
#' m + n
#' n + m
install_conversion_offset <- function(from, to, const) {
  stopifnot(is.finite(const))
  if (! xor(ud_is_parseable(from), ud_is_parseable(to)))
    stop("exactly one of (from, to) must be a known unit")
  if (ud_is_parseable(to))
    R_ut_offset(as.character(from), as.character(to), -as.double(const))
  else
    R_ut_offset(as.character(to), as.character(from), as.double(const))
}
