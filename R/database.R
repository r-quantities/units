.startup_msg <- function(warn = FALSE) {
  paste("udunits database from" , .get_ud_xml(warn))
}

.get_ud_xml <- function(warn = FALSE) {
  paths = c(
    Sys.getenv("UDUNITS2_XML_PATH"),
    "/usr/local/share/udunits/udunits2.xml",
    "/usr/share/xml/udunits/udunits2.xml",
    "/usr/share/udunits/udunits2.xml")

  fallback = system.file("share/udunits/udunits2.xml", package="units")

  w = which(file.exists(paths))
  if (length(w) > 1 && warn)
    warning(paste("multiple udunits databases present:\n - "),
            paste(paths[w], collapse = "\n - "), call.=FALSE)
  p = if (length(w) == 0) fallback else paths[w[1]]

  if (!file.exists(p))
    stop("Failed to identify udunits database: units will not work properly.",
         "\nPlease set the UDUNITS2_XML_PATH environment variable before ",
         "attempting to read the units database")
  p
}

.read_ud_db <- function(file = .get_ud_xml(), type) {
  if (! requireNamespace("xml2", quietly = TRUE))
    stop("package 'xml2' is required for this functionality", call.=FALSE)

  db <- xml2::read_xml(file)

  # resolve imports
  imports <- xml2::xml_find_all(db, "import")
  files <- sapply(imports, xml2::xml_text)
  for (i in files) {
    import <- xml2::read_xml(file.path(dirname(file), i))
    for (child in xml2::xml_children(import)) {
      source <- gsub("udunits2-|\\.xml", "", i)
      source <- paste0("<doc><source>", source, "</source></doc>")
      source <- xml2::xml_child(xml2::read_xml(source))
      xml2::xml_add_child(child, source)
      xml2::xml_add_child(db, child)
    }
  }
  xml2::xml_remove(imports)

  # filter by type
  if (!missing(type)) {
    children <- xml2::xml_children(db)
    xml2::xml_remove(children[xml2::xml_name(children) != type])
  }

  db
}

################################################################################
# Use this to generate the data -- to avoid Travis problems the result
# is stored as package data
# ud_units <- units:::.construct_ud_units()
# usethis::use_data(ud_units, internal=TRUE, overwrite=TRUE)

.construct_ud_units <- function() {
  .get_ud_symbols <- function(db, type) {
    symbols <- xml2::xml_find_all(db, paste0(type, "/symbol"))
    sapply(symbols, xml2::xml_text)
  }

  db <- .read_ud_db()
  ud_prefixes <- .get_ud_symbols(db, "prefix")
  ud_symbols <- .get_ud_symbols(db, "unit")
  expand_with_prefixes <- function(symbol) paste(ud_prefixes, symbol, sep = "")
  symbols <- unique(c(ud_symbols,
                      unlist(Map(expand_with_prefixes, ud_symbols), use.names = FALSE)))
  ud_units <- lapply(symbols, as_units, force_single_symbol = TRUE, check_is_valid = FALSE)
  names(ud_units) <- symbols
  class(ud_units) <- "units database"
  ud_units
}
# but this gives Y' Y" etc which are NOT recognized by as_units()
################################################################################
