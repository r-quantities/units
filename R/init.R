#' @import utils
#' @import stats
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
  # can this be lozy loaded on first access attempt instead?
	udunits_init(file.path(.get_ud_xml_dir(), "udunits2.xml"))
}

.onAttach <- function(libname, pkgname) {
    msg <- paste("udunits system database from", .get_ud_xml_dir(TRUE))
    packageStartupMessage(msg)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit()
}
