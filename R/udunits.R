#' Test if two units are convertible
#'
#' Parses and checks whether units can be converted by UDUNITS-2. Units may not
#' be convertible either because they are different magnitudes or because one
#' (or both) units are not defined in the database.
#'
#' @param x character or object of class \code{symbolic_units}, for the symbol
#' of the first unit.
#' @param y character or object of class \code{symbolic_units}, for the symbol
#' of the second unit.
#'
#' @return boolean, \code{TRUE} if both units exist and are convertible.
#' @export
#'
#' @examples
#' ud_are_convertible("m", "km")
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h")
#' ud_are_convertible("s", "kg")
ud_are_convertible = function(x, y) {
	res <- try(R_ut_are_convertible(
	  R_ut_parse(as.character(x)), R_ut_parse(as.character(y))), silent = TRUE)
	! inherits(res, "try-error") && res
}

ud_get_symbol = function(u) {
	sy = R_ut_get_symbol(u)
	if (sy == "")
		R_ut_get_name(u)
	else
		sy
}

ud_is_parseable = function(u) {
	res <- try(R_ut_parse(u), silent = TRUE)
	! inherits(res, "try-error")
}

ud_convert = function(value, from, to) {
	R_convert_doubles(R_ut_parse(from), R_ut_parse(to), value)
}

ud_set_encoding = function(enc) {
	R_ut_set_encoding(as.character(enc))
}
