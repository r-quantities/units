type_sum.units <- function(x, ...) {
  gr = units_options("group")
  # see https://github.com/r-lib/pillar/issues/73 : currently the [ and ] mess up things.
  structure(paste0(gr[1], as.character(units(x)), gr[2]),
            class = "type_sum_units")
}

type_sum.mixed_units <- function(x, ...) {
  "mixed_units" 
}

pillar_shaft.units <- function(x, ...) {
  u_char <- as.character(units(x))
  if (! requireNamespace("pillar", quietly = TRUE))
    stop("package pillar not available: install first?")
  #out <- paste(format(unclass(x), ...), pillar::style_subtle(u_char))
  out <- format(unclass(x), ...)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 8)
}

pillar_shaft.mixed_units <- function(x, ...) {
  if (! requireNamespace("pillar", quietly = TRUE))
    stop("package pillar not available: install first?")
  out <- format(x, ...)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 6)
}

format_type_sum.type_sum_units <- function(x, width, ...) {
  if (! requireNamespace("pillar", quietly = TRUE))
    stop("package pillar not available: install first?")
  pillar::style_subtle(x)
}

#nocov start
register_all_s3_methods <- function() {
  register_s3_method("pillar::type_sum", "units")
  register_s3_method("pillar::type_sum", "mixed_units")
  register_s3_method("pillar::pillar_shaft", "units")
  register_s3_method("pillar::pillar_shaft", "mixed_units")
  register_s3_method("pillar::format_type_sum", "type_sum_units")
}

register_s3_method <- function(generic, class, fun=NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(fun))
    fun <- get(paste0(generic, ".", class), envir=parent.frame())
  stopifnot(is.function(fun))

  if (package %in% loadedNamespaces())
    registerS3method(generic, class, fun, envir=asNamespace(package))

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...)
    registerS3method(generic, class, fun, envir=asNamespace(package)))
}
# nocov end
