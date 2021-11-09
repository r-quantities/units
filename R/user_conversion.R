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
