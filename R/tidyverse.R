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
  pillar::pillar_shaft(unclass(x), ...)
}

pillar_shaft.mixed_units <- function(x, ...) {
  u_char <- pillar::style_subtle(sapply(x, function(unit) format(unit[0])))
  out <- paste0(format(unclass(x), ...), u_char)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 6)
}

format_type_sum.type_sum_units <- function(x, width, ...) {
  pillar::style_subtle(x)
}


# vctrs proxying and restoration -------------------------------------

vec_proxy.units = function(x, ...) {
  x
}
vec_restore.units = function(x, to, ...) {
  restore_units(x, to)
}
restore_units <- function(x, to) {
  out <- .as.units(x, units(to))
  attr(out, "pillar") <- attr(to, "pillar")
  out
}


# vctrs coercion -----------------------------------------------------

vec_ptype2.units.units = function(x, y, ..., x_arg = "", y_arg = "") {
  x_units = units(x)
  y_units = units(y)

  if (!ud_are_convertible(x_units, y_units))
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)

  x_bare = drop_units(x)
  y_bare = drop_units(y)
  common = vctrs::vec_ptype2(x_bare, y_bare, ..., x_arg = x_arg, y_arg = y_arg)

  # Use left-hand side units
  set_units(common, x_units, mode = "standard")
}

vec_cast.units.units = function(x, to, ..., x_arg = "", to_arg = "") {
  x_units = units(x)
  to_units = units(to)

  if (!ud_are_convertible(x_units, to_units))
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)

  # Convert to target units before converting base type. Unit
  # conversion might change the type and so must happen first.
  out = set_units(x, to_units, mode = "standard")

  out_bare = drop_units(out)
  to_bare = drop_units(to)
  out = vctrs::vec_cast(out_bare, to_bare, ..., x_arg = x_arg, to_arg = to_arg)

  # Set target units again
  set_units(out, to_units, mode = "standard")
}


#nocov start
register_all_s3_methods <- function() {
  s3_register("pillar::type_sum", "units")
  s3_register("pillar::type_sum", "mixed_units")
  s3_register("pillar::pillar_shaft", "units")
  s3_register("pillar::pillar_shaft", "mixed_units")
  s3_register("pillar::format_type_sum", "type_sum_units")
  s3_register("vctrs::vec_proxy", "units")
  s3_register("vctrs::vec_restore", "units")
  s3_register("vctrs::vec_ptype2", "units.units")
  s3_register("vctrs::vec_cast", "units.units")
  s3_register("ggplot2::scale_type", "units")
  s3_register("ggplot2::limits", "units")
}
# nocov end
