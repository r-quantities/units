ud.are.convertible = function(u1, u2) {
	R_ut_are_convertible(R_ut_parse(as.character(u1)), R_ut_parse(as.character(u2)))
}

ud.get.symbol = function(u) {
	sy = R_ut_get_symbol(u)
	if (sy == "")
		R_ut_get_name(u)
	else
		sy
}

ud.is.parseable = function(u) {
	res <- try(R_ut_parse(u), silent = TRUE)
	! inherits(res, "try-error")
}

ud.convert = function(value, from, to) {
	R_convert_doubles(R_ut_parse(from), R_ut_parse(to), value)
}

ud.set.encoding = function(enc) {
	R_ut_set_encoding(as.character(enc))
	return()
}
