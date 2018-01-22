#' @import utils
#' @import stats
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
	udunits_init(character(0))
}

.onAttach <- function(libname, pkgname) {
    msg <- "udunits system database read"
    p0 <- Sys.getenv("UDUNITS2_XML_PATH")
    if (p0 != "") {
        msg <- paste(msg, "from", p0)
    }
    packageStartupMessage(msg)
}

.onUnLoad = function(libname, pkgname) {
	udunits_exit()
}
