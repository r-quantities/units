# Run by test_udunits.R in a child process. Holds the result of every native
# routine across a reload of the unit database, and across an unload of the
# namespace and of the DLL. A udunits pointer handed to R would be freed by its
# finalizer after its unit system or the DLL is gone, and crash this process.
.libPaths(strsplit(commandArgs(TRUE)[1], .Platform$path.sep, fixed = TRUE)[[1]])
suppressPackageStartupMessages(library(units))

calls <- alist(
  parse_unit         = parse_unit("kg m2 s-2"),
  ud_set_encoding    = ud_set_encoding("utf8"),
  ud_compare         = ud_compare(1, 1000, "km", "m"),
  ud_convertible     = ud_convertible("m", "km"),
  ud_convert_doubles = ud_convert_doubles(1, "km", "m"),
  ud_map_unit        = ud_map_unit("zz", "zorkmid", character(0)),
  ud_unmap_symbols   = ud_unmap_symbols("zz"),
  ud_unmap_names     = ud_unmap_names("zorkmid"),
  R_ut_get_name      = R_ut_get_name("m"),
  R_ut_get_symbol    = R_ut_get_symbol("meter"),
  R_ut_log           = R_ut_log("mW", 10),
  R_ut_parse         = R_ut_parse("1"),
  R_ut_format        = R_ut_format("1", definition = TRUE)
)
# ud_init() runs in load_units_xml() and ud_exit() in unloadNamespace() below
routines <- sub("^_units_", "", names(getDLLRegisteredRoutines("units")$.Call))
untested <- setdiff(routines, c(names(calls), "ud_init", "ud_exit"))
cat("untested routines: [", toString(untested), "]\n", sep = "")

old <- lapply(calls, eval, envir = asNamespace("units"))
load_units_xml()                          # frees the unit system of `old`
new <- lapply(calls, eval, envir = asNamespace("units"))
is_pointer <- function(x) typeof(x) == "externalptr"
cat("pointers: [", toString(names(Filter(is_pointer, c(old, new)))), "]\n", sep = "")

libpath <- find.package("units")
unloadNamespace("units")                  # frees the unit system of `new`
library.dynam.unload("units", libpath)    # unloads the DLL
rm(old); invisible(gc())                  # `old` is finalized now, `new` at exit
cat("survived\n")
