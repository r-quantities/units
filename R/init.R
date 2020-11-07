#' @import utils
#' @import stats
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
  udunits_init(.get_ud_xml())
  if (ud_is_parseable("B"))
    .default_options$define_bel <- FALSE
  do.call(units_options, .default_options)

  native <- if (l10n_info()[["UTF-8"]]) "utf8"
  else if (l10n_info()[["Latin-1"]]) "latin1"
  else "ascii"
  ud_set_encoding(native)

  register_all_s3_methods()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(.startup_msg(TRUE))
}

.onUnload = function(libname, pkgname) {
  # force run weak finalizers before freeing sys
  invisible(gc())
	udunits_exit()
}
