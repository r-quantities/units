#' Define or remove units
#'
#' Installing new symbols and/or names allows them to be used in \code{as_units},
#' \code{make_units} and \code{set_units}. Optionally, a relationship can be
#' defined between such symbols/names and existing ones (see details and examples).
#'
#' At least one symbol or name is expected, but multiple symbols and/or names
#' can be installed (and thus mapped to the same unit) or removed at the same
#' time. If \code{def} resolves to a unit that already has a symbol (resp.
#' name), the new symbols (resp. names) are installed as aliases: they can be
#' used as input, but the unit keeps its own symbol (resp. name) for output.
#' Otherwise, the first symbol and the first name become those of the unit.
#'
#' An installation either succeeds as a whole or leaves the system unchanged:
#' if any symbol or name is already read as a different unit, nothing is
#' installed. Note that UDUNITS-2 reads an identifier by name (regardless of
#' case), then by symbol, and then by removing a prefix, so a new symbol or
#' name also collides with names or symbols of the other kind, with prefixed
#' units (e.g., \code{"km"}), and, for names, with symbols that differ only in
#' case (e.g., a name \code{"KG"} would shadow \code{"kg"}). Removing a symbol
#' or name only undoes what \code{install_unit} did for it; symbols and names
#' from the units database, or from anywhere else, are left untouched with a
#' warning.
#'
#' To redefine a symbol or name from the database on purpose, use
#' \code{override = TRUE}: the database mappings that give it another meaning
#' are set aside (and reported with a message), and \code{remove_unit} puts
#' them back. The new meaning also applies to prefixed forms (e.g., redefining
#' \code{"gal"} redefines \code{"kgal"}), and to existing \code{units} objects
#' that use the identifier, so it is best done at the start of a session.
#' Identifiers installed by \code{install_unit} cannot be overridden, but must
#' be removed first, and other identifiers that a new name would shadow are
#' still refused. As names are matched regardless of case, overriding a symbol
#' also sets aside a database name spelled the same in any letter case (e.g.,
#' overriding the symbol \code{"gal"} makes \code{"Gal"} unavailable until it
#' is removed); override the name instead to redefine every spelling.
#'
#' The \code{def} argument enables arbitrary relationships with existing
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
#' @param override logical; if \code{TRUE}, symbols and names that are read as a
#' different unit in the units database are redefined (see details).
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
#' # redefine "gal" (in the database, the unit of acceleration cm/s^2)
#' install_unit("gal", "1 gallon", override = TRUE)
#' set_units(set_units(1, gal), L)
#' # clean up: "gal" is cm/s^2 again
#' remove_unit("gal")
#' set_units(set_units(1, gal), cm/s^2)
#'
#' @export
install_unit <- function(symbol=character(0), def=character(0), name=character(0),
                         override=FALSE) {
  stopifnot(is.character(def), length(def) < 2)
  stopifnot(is.logical(override), length(override) == 1, !is.na(override))
  .check_ids(symbol, name)

  # workaround for https://github.com/r-quantities/units/issues/289
  if (any(grepl("%", c(symbol, name), fixed=TRUE)))
    stop("Symbols/names with a percentage character '%' are not allowed ",
         "(see https://github.com/r-quantities/units/issues/289)")

  if (!length(def)) {
    ut_unit <- R_ut_new_base_unit()
  } else if (identical(def, "unitless")) {
    ut_unit <- R_ut_new_dimensionless_unit()
  } else {
    ut_unit <- R_ut_parse(def)
  }

  was <- ud_install(symbol, name, ut_unit, override)
  if (length(was))
    message("redefined ", paste(was, collapse = ", "))
  invisible(NULL)
}

.check_ids <- function(symbol, name) {
  stopifnot(is.character(symbol), is.character(name))
  if (!length(symbol) && !length(name))
    stop("at least one symbol or name must be specified")
  if (!all(validEnc(c(symbol, name))))
    stop("symbols and names must be valid strings in the current locale")
}

#' @rdname install_unit
#' @export
remove_unit <- function(symbol=character(0), name=character(0)) {
  .check_ids(symbol, name)

  res <- ud_remove(symbol, name)
  if (length(res$lost)) # nocov start
    warning("could not restore the database ", paste(res$lost, collapse = ", "),
            "; reload the database with load_units_xml()", call. = FALSE)
  # nocov end
  if (length(res$foreign))
    warning("not installed by install_unit(), left unchanged: ",
            paste0("'", res$foreign, "'", collapse = ", "),
            "; use install_unit(..., override = TRUE) to redefine them",
            call. = FALSE)
  invisible(NULL)
}
