#' @export
`with.units database` <- function(data, expr, ...) {
  if (all(!unlist(l10n_info()))) # ISO8859-15
    data <- subset(data, Encoding(names(data)) != "UTF-8")
  eval(substitute(expr), data, enclos = parent.frame())
}

#' @export
`print.units database` <- function(x, ...) {
  cat("Units database, containing", length(x), "units:\n")
  cat(" ", paste(names(x)[1:9], collapse=", "), "...\n")
}

#' List containing pre-defined units from the udunits2 package.
#'
#' Lazy loaded when used
#' @export
ud_units <- NULL

`%|%` <- function(x, y) ifelse(is.na(x), y, x)

`%empty%` <-  function(x, y) if(length(x)==0) y else x

pcc <- function(...) paste0(..., collapse = ", ")

#' Get information about valid units
#'
#' The returned dataframe is constructed at runtime by reading the xml database
#' that powers unit conversion in [package:udunits2]. Inspect this dataframe to
#' determine what inputs are accepted by \code{as_units} (and the other
#' functions it powers: \code{as_units} , \code{set_units} , \code{units<-}).
#'
#' Any entry listed under \code{symbol} , \code{symbol_aliases} , \code{
#' name_singular} , \code{name_singular_aliases} , \code{name_plural} , or
#' \code{name_plural_aliases} is valid. Additionally, any entry under
#' \code{symbol} or \code{symbol_aliases} may can also contain a valid prefix,
#' as specified by \code{valid_udunits_prefixes()} .
#'
#' Note, this is primarily intended for interactive use, the exact format of the
#' returned dataframe may change in the future.
#'
#' @param quiet logical, defaults \code{TRUE} to give a message about the location of
#'   the udunits database being read.
#'
#' @return a data frame with columns \code{symbol} , \code{symbol_aliases} ,
#'   \code{name_singular} , \code{name_singular_aliases} , \code{name_plural} ,
#'   or \code{name_plural_aliases} , \code{def} , \code{definition} ,
#'   \code{comment} , \code{dimensionless} and \code{source_xml}
#'
#' @export
#'
#' @name valid_udunits
#' @examples
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   valid_udunits()
#'   valid_udunits_prefixes()
#'   if(interactive())
#'     View(valid_udunits())
#' }
valid_udunits <- function(quiet = FALSE) {
  db <- .read_ud_db(type="unit")
  if (!quiet) message(.startup_msg())

  xml_nodes <- xml2::xml_children(db)

  l <- lapply(seq_len(xml2::xml_length(db)), function(i) {
    unit <- xml_nodes[[i]]

    source_xml <- xml2::xml_text(xml2::xml_find_first(unit, ".//source"))

    symbols <- xml2::xml_find_all(unit, ".//symbol")
    symbols <- xml2::xml_text(symbols) %empty% ""
    symbol         <-     symbols[ 1]
    symbol_aliases <- pcc(symbols[-1])

    unit_names <- xml2::xml_find_all(unit, ".//name")

    all_names <- unlist(lapply(unit_names, function(.x)
      xml2::xml_text(xml2::xml_children(.x))))

    singular <- xml2::xml_find_all(unit_names, ".//singular")
    singular <- xml2::xml_text(singular)

    plural   <- xml2::xml_find_all(unit_names, ".//plural")
    plural   <- xml2::xml_text(plural)

    name_singular         <-     singular[ 1]  %|% ""
    name_singular_aliases <- pcc(singular[-1]) %|% ""

    name_plural         <-     plural[ 1]  %|% ""
    name_plural_aliases <- pcc(plural[-1]) %|% ""

    def <- xml2::xml_find_all(unit, ".//def")
    def <- xml2::xml_text(def) %empty% ""

    definition <-  xml2::xml_find_all(unit, ".//definition")
    definition <- xml2::xml_text(definition) %empty% ""

    comment <- xml2::xml_find_all(unit, ".//comment")
    comment <- xml2::xml_text(comment) %empty% ""

    dimensionless <- xml2::xml_find_all(unit, ".//dimensionless")
    dimensionless <- as.logical(length(dimensionless))

    # all node names that might be in a unit node
    # db %>% xml_children() %>% map(~xml_children(.x) %>% xml_name()) %>%
    # unique() %>% unlist() %>% unique()
    # [1] "base"          "name"          "symbol"
    # [4] "aliases"       "definition"    "def"
    # [7] "comment"       "dimensionless"

    # rest_xml <- unit %>% xml_children()
    # rest <- map(rest_xml, xml_text)
    # names(rest) <- rest_xml %>% xml_name()
    # rest <- list(rest)

    data.frame(symbol, symbol_aliases,
               name_singular, name_singular_aliases,
               name_plural, name_plural_aliases,
               def, definition, comment, dimensionless, source_xml, #, rest
               stringsAsFactors = FALSE)

  })

  df <- do.call(rbind, c(l, stringsAsFactors=FALSE, make.row.names=FALSE))
  class(df) <- c( "tbl_df", "tbl", "data.frame")
  df
}

#' @name valid_udunits
#' @export
valid_udunits_prefixes <- function(quiet = FALSE) {
  db <- .read_ud_db(type="prefix")
  if (!quiet) message(.startup_msg())

  # all prefix valid names
  # db %>% xml_children() %>% map(~xml_children(.x) %>% xml_name()) %>%
  #   unlist() %>% unique()
  # "value"  "name"   "symbol"

  l <- lapply( xml2::xml_children(db), function(prefix) {

    symbols <- xml2::xml_find_all(prefix, ".//symbol")
    symbols <- xml2::xml_text(symbols)
    symbol <- symbols[1]
    symbol_aliases <- pcc(symbols[-1])

    name <- xml2::xml_find_all(prefix, ".//name")
    name <- xml2::xml_text(name)

    value <- xml2::xml_find_all(prefix, ".//value")
    value <- xml2::xml_double(value)

    data.frame(symbol, symbol_aliases, name, value)
  })

  df <- do.call(rbind.data.frame,
                c(l, stringsAsFactors = FALSE, make.row.names = FALSE))
  class(df) <- c( "tbl_df", "tbl", "data.frame")
  df
}
