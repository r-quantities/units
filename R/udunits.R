#' Test if two units are convertible
#'
#' @param u1 string, the first unit
#' @param u2 string, the second unit
#'
#' @return boolean, \code{TRUE} if both units are convertible.
#' @export
#'
#' @examples
#' ud_are_convertible("m", "km")
#' a <- set_units(1:3, m/s)
#' ud_are_convertible(units(a), "km/h)
#' ud_are_convertible("s", "kg")
ud_are_convertible = function(u1, u2) {
	res <- try(R_ut_are_convertible(R_ut_parse(as.character(u1)), R_ut_parse(as.character(u2))),
		silent = TRUE)
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
