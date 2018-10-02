#' @import utils
#' @import stats
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
  udunits_init(file.path(.get_ud_xml_dir(), "udunits2.xml"))
  do.call(units_options, .default_options)
}

.onAttach <- function(libname, pkgname) {
    msg <- paste("udunits system database from", .get_ud_xml_dir(TRUE))
    packageStartupMessage(msg)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit()
}
