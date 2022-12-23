.as.mixed_units = function(x) {
	stopifnot(is.list(unclass(x)))
	structure(x, class = c("mixed_units", "list"))
}

# constructor function:
#' Create or convert to a mixed units list-column
#' @param x numeric, or vector of class \code{units}
#' @param values character vector with units encodings, or list with symbolic units of class \code{mixed_symbolic_units}
#' @param value see values
#' @param ... ignored
#' @details if \code{x} is of class \code{units}, \code{values} should be missing or of class \code{mixed_symbolic_units}; if \code{x} is numeric, \code{values} should be a character vector the length of \code{x}.
#' @examples
#' a <- 1:4
#' u <- c("m/s", "km/h", "mg/L", "g")
#' mixed_units(a, u)
#' units(a) = as_units("m/s")
#' mixed_units(a) # converts to mixed representation
#' @export
mixed_units <- function(x, values, ...) UseMethod("mixed_units")


#' @export
mixed_units.units = function(x, values, ...) {
	stopifnot(missing(values))
	u = as.character(units(x))
	mixed_units(unclass(x), rep(u, length(x)))
}


#' @export
mixed_units.numeric = function(x, values, ...) {
	#stopifnot(length(x) == length(values), is.character(values), is.numeric(x))
	stopifnot(is.character(values), is.numeric(x))
	.as.mixed_units(mapply(set_units, x, values, mode = "standard", SIMPLIFY = FALSE))
}

#' @export
#' @name mixed_units
`units<-.mixed_units` = function(x, value) {
	set_units(x, value)
}


#' @export
format.mixed_units = function(x, ...) {
	sapply(x, format, ...)
}

#' @export
`[.mixed_units` = function(x, i, ...) {
	.as.mixed_units(unclass(x)[i])
}

#' @export
c.mixed_units = function(...) {
	args = list(...)
	.as.mixed_units(do.call(c, lapply(args, unclass)))
}

#' @export
set_units.mixed_units = function(x, value, ..., mode = "standard") {
	if (! is.character(value))
		stop("use character string to denote target unit") # FIXME: rlang::quo stuff needed here?

  # conversion data frame and split
  cv <- data.frame(
    val = as.numeric(x), from = I(units(x)), to = value, idx = seq_along(x),
    stringsAsFactors = FALSE)
  sp <- paste(cv$from, cv$to, sep=".")
  sp <- factor(sp, levels=unique(sp))

  # grouped conversion
  cv <- do.call(rbind, unname(by(cv, sp, function(x) {
    x$val <- set_units(x$val, x$from[[1]], mode="standard")
    x$val <- mixed_units(set_units(x$val, x$to[1], mode="standard"))
    x
  }, simplify=FALSE)))

  # reordering
  cv[order(cv$idx), ]$val
}

#' @export
as_units.mixed_units = function(x, ...) {
	set_units(do.call(c, x), value = units(x[[1]]), mode = "standard")
}


#' @export
units.mixed_units = function(x) {
  u = lapply(x, function(i) if (inherits(i, "units")) units(i) else NULL)
	structure(u, class = "mixed_symbolic_units")
}

#' @export
as.character.mixed_symbolic_units = function(x, ...) {
	sapply(x, function(i) if (!is.null(i)) as.character(i) else "NULL")
}

.cat_units_table <- function(x) {
  cat("Mixed units: ")
  if (!length(x)) return()

  tbl = table(as.character(units(x)))
  tbl = paste(names(tbl), " (", as.numeric(tbl), ")", sep = "")
  cat(paste(tbl, collapse = ", "), "\n")
}

#' @export
print.mixed_units = function(x, ...) {
  .cat_units_table(x)
	cat(paste(format(x, ...), collapse = ", "), "\n")
}

#' @export
str.mixed_units = function(object, ...) {
  .cat_units_table(object)
  cat(capture.output(str(unclass(object), ...))[-1], sep="\n")
}

#' @name drop_units
#' @export
drop_units.mixed_units = function(x) {
	sapply(x, drop_units)
}

#' @export
Ops.mixed_units = function(e1, e2) {
	if (inherits(e2, "units"))
		e2 = mixed_units(e2)
    ret = switch(.Generic,
			"==" = mapply(function(x, y) { x == y }, e1, e2, SIMPLIFY = TRUE),
			"!=" = mapply(function(x, y) { x != y }, e1, e2, SIMPLIFY = TRUE),
			"*"  = mapply(function(x, y) { x * y  }, e1, e2, SIMPLIFY = FALSE),
			"/"  = mapply(function(x, y) { x / y  }, e1, e2, SIMPLIFY = FALSE),
			"+"  = mapply(function(x, y) { x + y  }, e1, e2, SIMPLIFY = FALSE),
			"-"  = mapply(function(x, y) { x - y  }, e1, e2, SIMPLIFY = FALSE),
			stop(paste("operation", .Generic, "not supported"))
		)
	if (is.list(ret))
		ret = .as.mixed_units(ret)
	ret
}

#' @export
unique.mixed_units <- function(x, incomparables = FALSE, ...) {
  .as.mixed_units(NextMethod())
}

#' @export
as.data.frame.mixed_units <- as.data.frame.AsIs
