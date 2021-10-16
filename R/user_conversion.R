#' Define or remove units
#'
#' Installing new symbols and/or names allows them to be used in \code{as_units},
#' \code{make_units} and \code{set_units}. Optionally, a relationship can be
#' defined between such symbols/names and existing ones (see details and examples).
#'
#' At least one symbol or name is expected, but multiple symbols and/or names
#' can be installed (and thus mapped to the same unit) or removed at the same
#' time. The \code{def} argument enables arbitrary relationships with existing
#' units using UDUNITS-2 syntax:
#' \tabular{llll}{
#'   \strong{String Type} \tab \strong{Using Names} \tab \strong{Using Symbols}
#'     \tab \strong{Comment}\cr
#'   Simple \tab meter \tab m \tab \cr
#'   Raised \tab meter^2 \tab m2 \tab
#'     higher precedence than multiplying or dividing\cr
#'   Product \tab newton meter \tab N.m \tab \cr
#'   Quotient \tab meter per second \tab m/s \tab \cr
#'   Scaled \tab 60 second \tab 60 s \tab \cr
#'   Prefixed \tab kilometer \tab km \tab \cr
#'   Offset \tab kelvin from 273.15 \tab K @ 273.15 \tab
#'     lower precedence than multiplying or dividing\cr
#'   Logarithmic \tab lg(re milliwatt) \tab lg(re mW) \tab
#'     "lg" is base 10, "ln" is base e, and "lb" is base 2\cr
#'   Grouped \tab (5 meter)/(30 second) \tab (5 m)/(30 s) \tab
#' }
#' The above may be combined, e.g., \code{"0.1 lg(re m/(5 s)^2) @ 50"}.
#' You may also look at the \code{<def>} elements in the units database to see
#' examples of string unit specifications.
#'
#' @param symbol a vector of symbols to be installed/removed.
#' @param def either \itemize{
#'   \item an empty definition, which defines a new base unit;
#'   \item \code{"unitless"}, which defines a new dimensionless unit;
#'   \item a relationship with existing units (see details for the syntax).
#' }
#' @param name a vector of names to be installed/removed.
#'
#' @examples
#' # define a fortnight
#' install_unit("fn", "2 week", "fortnight")
#' year <- as_units("year")
#' set_units(year, fn)        # by symbol
#' set_units(year, fortnight) # by name
#' # clean up
#' remove_unit("fn", "fortnight")
#'
#' # working with currencies
#' install_unit("dollar")
#' install_unit("euro", "1.22 dollar")
#' install_unit("yen", "0.0079 euro")
#' set_units(as_units("dollar"), yen)
#' # clean up
#' remove_unit(c("dollar", "euro", "yen"))
#'
#' # an example from microbiology
#' cfu_symbols <- c("CFU", "cfu")
#' cfu_names <- c("colony_forming_unit", "ColonyFormingUnit")
#' install_unit("cell")
#' install_unit(cfu_symbols, "3.4 cell", cfu_names)
#' cell <- set_units(2.5e5, cell)
#' vol <- set_units(500, ul)
#' set_units(cell/vol, "cfu/ml")
#' set_units(cell/vol, "CFU/ml")
#' set_units(cell/vol, "colony_forming_unit/ml")
#' set_units(cell/vol, "ColonyFormingUnit/ml")
#' # clean up
#' remove_unit(c("cell", cfu_symbols), cfu_names)
#'
#' @export
install_unit <- function(symbol=character(0), def=character(0), name=character(0)) {
  stopifnot(is.character(def), length(def) < 2)
  stopifnot(is.character(symbol), is.character(name))
  if (!length(symbol) && !length(name))
    stop("at least one symbol or name must be specified")

  if (!length(def)) {
    ut_unit <- R_ut_new_base_unit()
  } else if (identical(def, "unitless")) {
    ut_unit <- R_ut_new_dimensionless_unit()
  } else {
    ut_unit <- R_ut_parse(def)
  }

  ud_map_symbols(symbol, ut_unit)
  ud_map_names(name, ut_unit)
}

#' @rdname install_unit
#' @export
remove_unit <- function(symbol=character(0), name=character(0)) {
  stopifnot(is.character(symbol), is.character(name))
  if (!length(symbol) && !length(name))
    stop("at least one symbol or name must be specified")

  ud_unmap_symbols(symbol)
  ud_unmap_names(name)
}

#' Define new symbolic units
#'
#' Adding a symbolic unit allows it to be used in \code{as_units},
#' \code{make_units} and \code{set_units}. No installation is performed if the
#' unit is already known by udunits.
#'
#' @param name a length 1 character vector that is the unit name or symbol.
#' @param warn warns if the supplied unit symbol is already a valid unit symbol
#'   recognized by udunits.
#' @param dimensionless logical; if \code{TRUE}, a new dimensionless unit is
#' created, if \code{FALSE} a new base unit is created. Dimensionless units are
#' convertible to other dimensionless units (such as \code{rad}), new base units
#' are not convertible to other existing units.
#'
#' @details \code{install_symbolic_unit} installs a new dimensionless unit;
#' these are directly compatible to any other dimensionless unit. To install a
#' new unit that is a scaled or shifted version of an existing unit, use
#' \code{install_conversion_constant} or \code{install_conversion_offset} directly.ç
#'
#' @export
install_symbolic_unit <- function(name, warn = TRUE, dimensionless = TRUE) {# nocov start
  .Deprecated("install_unit")
  check_unit_format(name)

  if(ud_is_parseable(name)) {
    if (warn) warning(
      sQuote(name), " is already a valid unit recognized by udunits; removing and reinstalling.")
    remove_symbolic_unit(name)
  }

  ut_unit <- if (dimensionless)
    R_ut_new_dimensionless_unit() else R_ut_new_base_unit()
  ud_map_names(name, ut_unit)

  invisible(NULL)
}# nocov end

#' @export
#' @rdname install_symbolic_unit
remove_symbolic_unit <- function(name) {# nocov start
  .Deprecated("remove_unit")
	remove_unit(name=name)
}# nocov end

#' Install a conversion constant or offset between user-defined units.
#'
#' Tells the \code{units} package how to convert between units that
#' have a linear relationship, i.e. can be related on the form \eqn{y = \alpha
#' x} (constant) or \eqn{y = \alpha + x} (offset).
#'
#' @param from    String for the symbol of the unit being converted from.
#' @param to      String for the symbol of the unit being converted to. One of
#' \code{from} and \code{to} must be an existing unit name.
#' @param const   The constant \eqn{\alpha} in the conversion.
#'
#' @details This function handles the very common case where units are related
#'   through a linear function, that is, you can convert from one to the other
#'   as \eqn{y = \alpha x}. Using this function, you specify that you
#'   can go from values of type \code{from} to values of type \code{to} by
#'   multiplying by a constant, or adding a constant.
#'
#' @export
install_conversion_constant <- function(from, to, const) {# nocov start
  .Deprecated("install_unit")
  stopifnot(is.finite(const), const != 0.0)
  if (! xor(ud_is_parseable(from), ud_is_parseable(to)))
    stop("exactly one of (from, to) must be a known unit")
  if (ud_is_parseable(to)) {
    new <- R_ut_scale(R_ut_parse(to), as.double(const))
    ud_map_names(check_unit_format(from), new)
  } else {
    new <- R_ut_scale(R_ut_parse(from), 1.0 /as.double(const))
    ud_map_names(check_unit_format(to), new)
  }
}# nocov end

#' @export
#' @name install_conversion_constant
install_conversion_offset <- function(from, to, const) {# nocov start
  .Deprecated("install_unit")
  stopifnot(is.finite(const))
  if (! xor(ud_is_parseable(from), ud_is_parseable(to)))
    stop("exactly one of (from, to) must be a known unit")
  if (ud_is_parseable(to)) {
    new <- R_ut_offset(R_ut_parse(to), -as.double(const))
    ud_map_names(check_unit_format(from), new)
  } else {
    new <- R_ut_offset(R_ut_parse(from), as.double(const))
    ud_map_names(check_unit_format(to), new)
  }
}# nocov end

check_unit_format <- function(x) {# nocov start
  cond <- c(
    # leading and trailing numbers
    grepl("^[[:space:]]*[0-9]+", x), grepl("[0-9]+[[:space:]]*$", x),
    # arithmetic operators
    grepl("\\+|\\-|\\*|\\/|\\^", x),
    # intermediate spaces
    grepl("[[:alnum:][:punct:]]+[[:space:]]+[[:alnum:][:punct:]]+", x)
  )
  if (any(cond))
    stop("the following elements are not allowed in new unit names/symbols:\n",
         "  - leading or trailing numbers\n",
         "  - arithmetic operators\n",
         "  - intermediate white spaces")
  x
}# nocov end
