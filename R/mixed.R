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
	mixed_units(as.numeric(x), rep(u, length(x)))
}


#' @export
mixed_units.numeric = function(x, values, ...) { 
	#stopifnot(length(x) == length(values), is.character(values), is.numeric(x))
	stopifnot(is.character(values), is.numeric(x))
	structure(mapply(set_units, x, values, mode = "standard", SIMPLIFY = FALSE), 
		class = "mixed_units")
}

#' @export
#' @name mixed_units
`mixed_units<-` = function(x, value) {
	mixed_units(x, value)
}


#' @export
format.mixed_units = function(x, ...) {
	sapply(x, format, ...)
}

#' @export
`[.mixed_units` = function(x, i, ...) {
	structure(unclass(x)[i], class = "mixed_units")
}
c.mixed_units = function(...) {
	args = list(...)
	structure(do.call(c, lapply(args, unclass)), class = "mixed_units")
}

#' @export
set_units.mixed_units = function(x, value, ..., mode = "standard") {
	if (! is.character(value))
		stop("use character string to denote target unit") # FIXME: rlang::quo stuff needed here?
	do.call(c, lapply(x, set_units, value = value, mode = mode, ...))
}

#' @export
as_units.mixed_units = function(x, ...) {
	set_units(do.call(c, x), value = units(x[[1]]), mode = "standard")
}


#' @export
units.mixed_units = function(x) {
	structure(lapply(x, units), class = "mixed_symbolic_units")
}

#' @export
as.character.mixed_symbolic_units = function(x, ...) {
	sapply(x, as.character)
}

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
			"*"  = mapply(function(x, y) { x * y  }, e1, e2, SIMPLIFY = FALSE)
			# etc.
		)
	if (is.list(ret))
		ret = structure(ret, class = "mixed_units")
	ret
}


