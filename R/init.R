#' @import utils
#' @import stats
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
	udunits_init(character(0), TRUE)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit(logical(0))
}
