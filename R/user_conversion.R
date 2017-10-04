
# Using environments as hash tables...
conversion_table <- new.env(parent = emptyenv())

get_conversion_function <- function(from, to) {
  if (!exists(from, conversion_table)) return(NULL)
  table <- get(from, conversion_table)
  if (!exists(to, table)) return(NULL)
  get(to, table)
}

user_are_convertible <- function(from, to) {
  f <- get_conversion_function(from, to)
  !is.null(f) && is.function(f)
}

user_convert <- function(value, from, to) {
  f <- get_conversion_function(from, to)
  f(value)
}

#' Install a function for conversion between user-defined units.
#' 
#' @description Tells the \code{units} package how to convert one-way from one 
#'   unit to another.
#'   
#' @param from  String for the symbol of the unit being converted from.
#' @param to    String for the symbol of the unit being converted to.
#' @param f     A function responsible for conversion.
#'   
#' @details This is the most general way of specifying conversion between 
#'   user-defined units. The function installes a one-way conversion from one
#'   unit to another through a general function, \code{f}, that must take one
#'   numeric argument and return one numeric argument. When the \code{units}
#'   package tries to convert between units, it will look up \code{from} and
#'   \code{to} to see if it can find a conversion function. If it can, it will
#'   call \code{f} and consider the value converted from unit \code{from} to
#'   unit \code{to}.
#'   
#'   It is the user's responsibility to install a conversion from \code{to} back
#'   to \code{from} as well. One-way conversion does not work well with the 
#'   \code{units} package, since conversion is done in several places for unit 
#'   expression simplification and if a unit can only be converted in one 
#'   direction, this simplification will not work correctly.
#'   
#'   For conversion that can be done as a linear function, \eqn{y = \alpha x +
#'   \beta}, you should instead use the
#'   \code{\link{install_conversion_constant}} function. This function will
#'   automatically install conversion functions for both directions of unit
#'   conversion.
#'   
#' @examples 
#' 
#' apples <- 2 * make_unit("apple", user_defined = TRUE)
#' oranges <- 3 * make_unit("orange", user_defined = TRUE)
#' # one orange is worth two apples
#' install_conversion_function("orange", "apple", function(x) 2 * x)
#' install_conversion_function("apple", "orange", function(x) x / 2)
#' apples + oranges
#' oranges + apples
#' 
#' @seealso \code{\link{install_conversion_constant}}
#'   
#' @export
install_conversion_function <- function(from, to, f) {
  if (!exists(from, conversion_table)) {
    assign(from, new.env(parent = emptyenv()), conversion_table)
  }
  table <- get(from, conversion_table)
  assign(to, f, table)
}

#' Install a function for conversion between user-defined units.
#' 
#' @description Tells the \code{units} package how to convert between units that
#'   have a linear relationship, i.e. can be related on the form \eqn{y = \alpha
#'   x}.
#'   
#' @param from    String for the symbol of the unit being converted from.
#' @param to      String for the symbol of the unit being converted to.
#' @param const   The constant \eqn{\alpha} in the conversion.
#'   
#' @details This function handles the very common case where units are related 
#'   through a linear function, that is, you can convert from one to the other 
#'   as \eqn{y = \alpha x}. Using this function, you specify that you
#'   can go from values of type \code{from} to values of type \code{to} by first
#'   multiplying a constant and then adding an offset. The function then
#'   automatically installs that conversion and the invers \eqn{x =
#'   y/\alpha}.
#'   
#'   For a more general conversion mechanism, see
#'   \code{\link{install_conversion_function}}.
#'   
#' @examples 
#' 
#' apples <- 2 * make_unit("apple", user_defined = TRUE)
#' oranges <- 1 * make_unit("orange", user_defined = TRUE)
#' # one orange is worth two apples
#' install_conversion_constant("orange", "apple", 2)
#' apples + oranges
#' oranges + apples
#' 
#' @seealso \code{\link{install_conversion_function}}
#'   
#' @export
install_conversion_constant <- function(from, to, const) {
  install_conversion_function(from, to, function(x) const * x)
  install_conversion_function(to, from, function(x) x / const)
}

