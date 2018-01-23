ud_are_convertible = function(u1, u2) {
	R_ut_are_convertible(as.character(u1), as.character(u2))
}

ud_get_symbol = function(u) {
	sy = R_ut_get_symbol(u)
	if (sy == "")
		R_ut_get_name(u)
	else
		sy
}

ud_is_parseable = function(u) {
	R_ut_is_parseable(as.character(u))
}

ud_convert = function(value, from, to) {
	R_ut_convert_doubles(as.character(from), as.character(to), value)
}

ud_set_encoding = function(enc) {
	R_ut_set_encoding(as.character(enc))
}
