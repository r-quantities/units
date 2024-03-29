#' @export
`[<-.units` <- function(x, ..., value) {
  units(value) <- units(x)
  NextMethod()
}

#' @export
`[[<-.units` <- `[<-.units`

#' @export
c.units <- function(..., recursive = FALSE, allow_mixed = units_options("allow_mixed")) {
  args <- list(...)
  args[sapply(args, is.null)] <- NULL # remove NULLs
  u <- units(args[[1]])
  if (length(args) == 1)
  	.as.units(NextMethod(), u)
  else if (.units_are_convertible(args[-1], u)) {
    args <- lapply(args, set_units, u, mode="standard")
    .as.units(do.call(c, lapply(args, drop_units)), u)
  } else if (allow_mixed)
    do.call(c, lapply(args, mixed_units))
  else
  	stop("units are not convertible, and cannot be mixed; try setting units_options(allow_mixed = TRUE)?")
}

.units_are_convertible = function(x, u) {
	for (i in seq_along(x))
		if (! ud_are_convertible(units(x[[i]]), u))
			return(FALSE)
	TRUE
}

#' @export
diff.units = function(x, ...) {
  u = units(x)
  # units(x) = u will not work here, as units(x) is NULL!
  .as.units(NextMethod(), u)
}

#' @export
rep.units = function(x, ...) {
  u = units(x)
  .as.units(NextMethod(), u)
}


#' deparse unit to string in product power form (e.g. km m-2 s-1)
#'
#' deparse unit to string in product power form (e.g. km m-2 s-1)
#' @param x object of class units
#' @return length one character vector
#' @examples
#' u = as_units("kg m-2 s-1", implicit_exponents = TRUE)
#' u
#' deparse_unit(u)
#' @export
deparse_unit = function(x) {
  stopifnot(inherits(x, "units"))
  u = units(x)
  tn = table(u$numerator)
  nm1 = names(tn)
  vals1 = as.character(tn)
  vals1[vals1 == "1"] = ""
  td = - table(u$denominator)
  nm2 = names(td)
  vals2 = as.character(td)
  paste(c(paste0(nm1, vals1), paste0(nm2, vals2)), collapse=" ")
}
# This should perhaps be an option in format.symbolic_units

#' @method all.equal units
#' @export
all.equal.units = function(target, current, ...) {
  if (inherits(current, "units")) {
    units(current) <- units(target)
    target <- drop_units(target)
    current <- drop_units(current)
  }
  NextMethod()
}

#' seq method for units objects
#' @param from see \link[base]{seq}
#' @param to see \link[base]{seq}
#' @param by see \link[base]{seq}
#' @param length.out see \link[base]{seq}
#' @param along.with see \link[base]{seq}
#' @param ... see \link[base]{seq}
#' @details arguments with units are converted to have units of the first argument (which is either \code{from} or \code{to})
#' @export
#' @examples
#' seq(to = set_units(10, m), by = set_units(1, m), length.out = 5)
#' seq(set_units(10, m), by = set_units(1, m), length.out = 5)
#' seq(set_units(10, m), set_units(19, m))
#' seq(set_units(10, m), set_units(.1, km), set_units(10000, mm))
seq.units = function(from, to, by = ((to - from)/(length.out - 1)),
         length.out = NULL, along.with = NULL, ...) {
  mf = missing(from)
  mt = missing(to)
  uuu = if (mf)
      units(to)
  	else
      units(from)
  if (! mf)
    from = as.numeric(from)
  if (! mt)
  	to = as.numeric(set_units(to, uuu, mode = "standard"))
  if (! missing(by))
    by = as.numeric(set_units(by, uuu, mode = "standard"))
  set_units(NextMethod(), uuu, mode = "standard")
}

#' @export
str.units = function(object, ...) {
  gr <- units_options("group")
  unit_string <- paste0(gr[1], as.character(attr(object, "units")), gr[2])
  cat(paste0(" Units: ", unit_string))
  str(drop_units(object), ...)
}

#' @export
duplicated.units <- function(x, incomparables=FALSE, ...) {
  if (is.null(dim(x)))
    NextMethod() else duplicated.array(x, incomparables, ...)
}

#' @export
anyDuplicated.units <- function(x, incomparables=FALSE, ...) {
  if (is.null(dim(x)))
    NextMethod() else anyDuplicated.array(x, incomparables, ...)
}

#' @export
unique.units <- function(x, incomparables = FALSE, ...) {
  xx <- if (is.null(dim(x)))
    NextMethod() else unique.array(x, incomparables, ...)
  .as.units(xx, units(x))
}
