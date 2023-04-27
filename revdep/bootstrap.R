if (!nchar(n <- Sys.getenv("workers"))) {

  # https://github.com/Enchufa2/bspm/issues/67
  ns <- asNamespace("bspm")
  unlockBinding("ask_user", ns)
  assignInNamespace("ask_user", function(later, ...) later, ns)
  lockBinding("ask_user", ns)

  options(bspm.version.check=TRUE)
  rdeps <- tools::package_dependencies("units", which="all", reverse=TRUE)[[1]]
  install.packages(rdeps, dependencies=TRUE)
  install.packages("pak", type="source", repos=sprintf(
    "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
    .Platform$pkgType, R.Version()$os, R.Version()$arch))
  pkgs <- lapply(rdeps, function(i) tryCatch({
    sapply(strsplit(pak::pkg_system_requirements(i), " "), "[", -(1:3))
  }, error=function(e) NULL))
  system(paste("apt-get install -y", paste(unique(unlist(pkgs)), collapse=" ")))
  remotes::install_github("r-lib/revdepcheck")

} else {

  bspm::disable()

  setHook(packageEvent("revdepcheck", "onLoad"), function(...) {
    deps_opts <- getFromNamespace("deps_opts", "revdepcheck")
    pkg_name <- getFromNamespace("pkg_name", "revdepcheck")
    dir_find <- getFromNamespace("dir_find", "revdepcheck")
    r_process_options <- getFromNamespace("r_process_options", "callr")

    assignInNamespace(
      "deps_install_opts", ns="revdepcheck",
      function(pkgdir, pkgname, quiet = FALSE, env = character()) {

        ## just link the system library
        func <- function(libdir, packages, quiet, repos) {
          unlink(libdir[1], force=TRUE, recursive=TRUE)
          system(paste("cd", dirname(libdir[1]), " && ln -s /usr/lib/R/site-library", basename(libdir[1])))
        }

        args <- c(
          ## We don't want to install the revdep checked package again,
          ## that's in a separate library, hence the `exclude` argument
          deps_opts(pkgname, exclude = pkg_name(pkgdir)),

          list(
            libdir = dir_find(pkgdir, "pkg", pkgname),
            quiet = quiet
          )
        )

        ## CRANCACHE_REPOS makes sure that we only use cached CRAN packages,
        ## but not packages that were installed from elsewhere
        r_process_options(
          func = func,
          args = args,
          system_profile = FALSE,
          user_profile = FALSE,
          env = c(CRANCACHE_REPOS = "cran,bioc",
                  CRANCACHE_QUIET = if (quiet) "yes" else "no",
                  env)
        )
      }
    )
  })

  revdepcheck::revdep_check(
    timeout=as.difftime(2, units="hours"), num_workers=as.numeric(n))

}
