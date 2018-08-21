

.message_where_udunits_db <- function() {
  udunits2_dir <- .get_ud_xml_dir()
  message("udunits system database read from " , udunits2_dir)
}

.get_ud_xml_dir <- function(warn = FALSE) {
  paths = c(
    Sys.getenv("UDUNITS2_XML_PATH"),
    "/usr/local/share/udunits/udunits2.xml",
    "/usr/share/xml/udunits/udunits2.xml",
    "/usr/share/udunits/udunits2.xml")

  fallback = system.file("share/udunits/udunits2.xml", package="units")

  w = which(file.exists(paths))
  if (length(w) > 1 && warn)
  	warning(paste("multiple udunits databases present:", paste(paths[w], collapse = " ")))
  p = if (length(w) == 0)
	fallback
  else
  	paths[w[1]]

  if (!file.exists(p))
    stop(
      "Failed to identify udunits system database: units will not work properly.\nPlease set the UDUNITS2_XML_PATH environment variable before attempting to read the units database")
  else 
    dirname(p)
}

.read_ud_db_symbols <- function(dir, filename) {
  if (! requireNamespace("xml2", quietly = TRUE))
    stop("package xml2 required to create ud_units database")
  database <- xml2::read_xml(file.path(dir, filename))
  symbols <- xml2::xml_find_all(database, ".//symbol")
  unlist(Map(function(node) as.character(xml2::xml_contents(node)), symbols))
}

## EP: it seems this is used nowhere
#.read_ud_db_scales <- function(dir, filename) {
#  if (! requireNamespace("xml2", quietly = TRUE))
#    stop("package xml2 required to create ud_units database")
#  database <- xml2::read_xml(file.path(dir, filename))
#  symbols <- xml2::xml_find_all(database, ".//value")
#  symbols
#  unlist(Map(function(node) as.numeric(as.character(xml2::xml_contents(node))), symbols))
#}

.get_ud_symbols <- function() {
  udunits2_dir <- .get_ud_xml_dir()
  symbols <- c(.read_ud_db_symbols(udunits2_dir, "udunits2-base.xml"),
               .read_ud_db_symbols(udunits2_dir, "udunits2-derived.xml"),
               .read_ud_db_symbols(udunits2_dir, "udunits2-accepted.xml"),
               .read_ud_db_symbols(udunits2_dir, "udunits2-common.xml"))

  symbols
}

.get_ud_prefixes <- function() {
  udunits2_dir <- .get_ud_xml_dir()
  .read_ud_db_symbols(udunits2_dir, "udunits2-prefixes.xml")
}

.construct_ud_units <- function(){
  ud_prefixes <- .get_ud_prefixes()
  ud_symbols <- .get_ud_symbols()
  expand_with_prefixes <- function(symbol) paste(ud_prefixes, symbol, sep = "")
  symbols <- unique(c(ud_symbols,
                      unlist(Map(expand_with_prefixes, ud_symbols), use.names = FALSE)))
  #ud_units <- Map(make_units, symbols)
  ud_units <- lapply(symbols, as_units, force_single_symbol = TRUE, check_is_valid = FALSE)
  names(ud_units) <- symbols
  ud_units
}
# but this gives Y' Y" etc which are NOT recognized by as_units()

# Use this to generate the data -- to avoid Travis problems the result
# is stored as package data
#ud_units <- .construct_ud_units()
#devtools::use_data(ud_units)

#' List containing pre-defined units from the udunits2 package.
#' 
#' Lazy loaded when used
#' @export
ud_units <- NULL

`%|%` <- function(x, y) ifelse(is.na(x), y, x)

`%empty%` <-  function(x, y) if(length(x)==0) y else x

pcc <- function(...) paste0(..., collapse = ", ")

.read_ud_db <- function(dir, filename) {
  if (! requireNamespace("xml2", quietly = TRUE))
    stop("package xml2 required to create ud_units database")
  database <- xml2::read_xml(file.path(dir, filename))
  # xml2::as_list(database)
  database
}



.ud_db_xml_list_as_dataframe <- function(db) {
  
  xml_nodes <- xml2::xml_children(db)
  
  l <- lapply(seq_len(xml2::xml_length(db)), function(i) {
    unit <- xml_nodes[[i]]
    
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
           def, definition, comment, dimensionless, #, rest
           stringsAsFactors = FALSE) 
    
  })
  
  do.call(rbind.data.frame, c(l, stringsAsFactors = FALSE, make.row.names = FALSE))
  
}

.read_ud_db_as_dataframe <- function(file_name) {
  udunits2_dir <- .get_ud_xml_dir()

  xml <- .read_ud_db(udunits2_dir, file_name) 
  df <- .ud_db_xml_list_as_dataframe(xml)
  
  df$source_xml <- gsub("udunits2-|\\.xml", "", file_name)
  df
}


.get_ud_db_all <- function() {
  
  base     <- .read_ud_db_as_dataframe("udunits2-base.xml") # 7
  derived  <- .read_ud_db_as_dataframe("udunits2-derived.xml") # 23
  accepted <- .read_ud_db_as_dataframe("udunits2-accepted.xml") # 24
  common   <- .read_ud_db_as_dataframe("udunits2-common.xml") # 221
  
  rbind.data.frame( 
    base, derived, accepted, common,
    stringsAsFactors = FALSE, make.row.names = FALSE
  )
}


.get_ud_prefixes_xml <- function() {
  udunits2_dir <- .get_ud_xml_dir()
  .read_ud_db(udunits2_dir, "udunits2-prefixes.xml")
}


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
  if(!requireNamespace("xml2", quietly = TRUE))
    stop("Package 'xml2' is required.")
  
  if(!nzchar(.get_ud_xml_dir())) 
    return(invisible())
  
  if(!quiet) 
    .message_where_udunits_db()
  
  df <- .get_ud_db_all()
  class(df) <- c( "tbl_df", "tbl", "data.frame")
  df
}



#' @name valid_udunits
#' @export
valid_udunits_prefixes <- function(quiet = FALSE) {
  
  if(!nzchar(.get_ud_xml_dir())) 
    return(invisible())
  
  if(!quiet) 
    .message_where_udunits_db()
  
  pr <- .get_ud_prefixes_xml()
  
  # all prefix valid names
  # pr %>% xml_children() %>% map(~xml_children(.x) %>% xml_name()) %>%
  #   unlist() %>% unique()
  # "value"  "name"   "symbol"
  
  l <- lapply( xml2::xml_children(pr), function(prefix) {
    
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
