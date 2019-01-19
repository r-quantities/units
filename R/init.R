#' @import utils
#' @import stats
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
  udunits_init(file.path(.get_ud_xml_dir(), "udunits2.xml"))
  if (ud_is_parseable("B"))
    .default_options$define_bel <- FALSE
  do.call(units_options, .default_options)
  
  native <- if (!l10n_info()[["MBCS"]]) "ascii"
  else if (l10n_info()[["Latin-1"]]) "latin1"
  else "utf8"
  ud_set_encoding(native)
}

.onAttach <- function(libname, pkgname) {
    msg <- paste("udunits system database from", .get_ud_xml_dir(TRUE))
    packageStartupMessage(msg)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit()
}
