#' @import utils
#' @import stats
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
	udunits_init(.get_ud_xml_dir())
}

.onAttach <- function(libname, pkgname) {
    msg <- paste("udunits system database from", .get_ud_xml_dir(TRUE))
    packageStartupMessage(msg)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit()
}
