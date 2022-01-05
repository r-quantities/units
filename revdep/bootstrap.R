options(
  HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(
    getRversion(), R.version$platform, R.version$arch, R.version$os)),
  repos = c(
    CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"),
  pak.no_extra_messages = TRUE
)
install.packages("pak", repos="https://r-lib.github.io/p/pak/stable/")

deps <- pak::pkg_deps("r-lib/revdepcheck")$package
rdeps <- tools::package_dependencies("units", reverse=TRUE)[[1]]
cmds <- lapply(unique(c(deps, rdeps)), pak::pkg_system_requirements)
cmds <- unique(unlist(cmds, use.names=FALSE))

system("apt-get update -y")
for (cmd in cmds) system(cmd)
pak::pkg_install("r-lib/revdepcheck")
