#' @import utils
#' @import stats
#' @import graphics
#' @importFrom Rcpp evalCpp
#' @useDynLib units
NULL

.onLoad = function(libname, pkgname) {
  ## Avoid updating the RNG state
  rng <- .GlobalEnv[[".Random.seed"]]
  on.exit({
    if (is.null(rng)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      }
    } else {
      .GlobalEnv[[".Random.seed"]] <- rng
    }
  })
  
  load_units_xml()

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

.onUnload = function(libpath) {
  ud_exit()
}
