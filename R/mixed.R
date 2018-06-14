# constructor function:
#' @export
mixed_units = function(values, units) { # FIXME: make generic
	if (missing(units) && inherits(values, "units"))
		mixed_units(as.numeric(values), units(units))
	else {
		stopifnot(length(values) == length(units), is.character(units), is.numeric(values))
		m = mapply(set_units, values, units, mode = "standard", SIMPLIFY = FALSE)
		structure(m, class = "mixed_units")
	}
}

#' @export
format.mixed_units = function(x, ...) {
	sapply(x, format, ...)
}

#' @export
`[.mixed_units` = function(x, i, ...) {
	structure(unclass(x)[i], class = "mixed_units")
}

#' @export
`mixed_units<-` = function(x, value) {
	mixed_units(x, value)
}

#' @export
set_units.mixed_units = function(x, value, ..., mode = "standard") {
	if (! is.character(value))
		stop("use character string to denote target unit") # FIXME: rlang::quo stuff needed here?
	#stopifnot(length(value) == 1)
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


