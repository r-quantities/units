// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ud_exit
void ud_exit();
RcppExport SEXP _units_ud_exit() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    ud_exit();
    return R_NilValue;
END_RCPP
}
// ud_init
void ud_init(CharacterVector path);
RcppExport SEXP _units_ud_init(SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type path(pathSEXP);
    ud_init(path);
    return R_NilValue;
END_RCPP
}
// ud_set_encoding
void ud_set_encoding(std::string enc_str);
RcppExport SEXP _units_ud_set_encoding(SEXP enc_strSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type enc_str(enc_strSEXP);
    ud_set_encoding(enc_str);
    return R_NilValue;
END_RCPP
}
// ud_compare
IntegerVector ud_compare(NumericVector x, NumericVector y, std::string xn, std::string yn);
RcppExport SEXP _units_ud_compare(SEXP xSEXP, SEXP ySEXP, SEXP xnSEXP, SEXP ynSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type xn(xnSEXP);
    Rcpp::traits::input_parameter< std::string >::type yn(ynSEXP);
    rcpp_result_gen = Rcpp::wrap(ud_compare(x, y, xn, yn));
    return rcpp_result_gen;
END_RCPP
}
// ud_convertible
LogicalVector ud_convertible(std::string from, std::string to);
RcppExport SEXP _units_ud_convertible(SEXP fromSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type from(fromSEXP);
    Rcpp::traits::input_parameter< std::string >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(ud_convertible(from, to));
    return rcpp_result_gen;
END_RCPP
}
// ud_convert_doubles
NumericVector ud_convert_doubles(NumericVector val, std::string from, std::string to);
RcppExport SEXP _units_ud_convert_doubles(SEXP valSEXP, SEXP fromSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type val(valSEXP);
    Rcpp::traits::input_parameter< std::string >::type from(fromSEXP);
    Rcpp::traits::input_parameter< std::string >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(ud_convert_doubles(val, from, to));
    return rcpp_result_gen;
END_RCPP
}
// ud_map_names
void ud_map_names(CharacterVector names, SEXP inunit);
RcppExport SEXP _units_ud_map_names(SEXP namesSEXP, SEXP inunitSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type inunit(inunitSEXP);
    ud_map_names(names, inunit);
    return R_NilValue;
END_RCPP
}
// ud_unmap_names
void ud_unmap_names(CharacterVector names);
RcppExport SEXP _units_ud_unmap_names(SEXP namesSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type names(namesSEXP);
    ud_unmap_names(names);
    return R_NilValue;
END_RCPP
}
// ud_map_symbols
void ud_map_symbols(CharacterVector symbols, SEXP inunit);
RcppExport SEXP _units_ud_map_symbols(SEXP symbolsSEXP, SEXP inunitSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type symbols(symbolsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type inunit(inunitSEXP);
    ud_map_symbols(symbols, inunit);
    return R_NilValue;
END_RCPP
}
// ud_unmap_symbols
void ud_unmap_symbols(CharacterVector symbols);
RcppExport SEXP _units_ud_unmap_symbols(SEXP symbolsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type symbols(symbolsSEXP);
    ud_unmap_symbols(symbols);
    return R_NilValue;
END_RCPP
}
// R_ut_get_dimensionless_unit_one
SEXP R_ut_get_dimensionless_unit_one();
RcppExport SEXP _units_R_ut_get_dimensionless_unit_one() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(R_ut_get_dimensionless_unit_one());
    return rcpp_result_gen;
END_RCPP
}
// R_ut_new_base_unit
SEXP R_ut_new_base_unit();
RcppExport SEXP _units_R_ut_new_base_unit() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(R_ut_new_base_unit());
    return rcpp_result_gen;
END_RCPP
}
// R_ut_new_dimensionless_unit
SEXP R_ut_new_dimensionless_unit();
RcppExport SEXP _units_R_ut_new_dimensionless_unit() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(R_ut_new_dimensionless_unit());
    return rcpp_result_gen;
END_RCPP
}
// R_ut_get_name
CharacterVector R_ut_get_name(SEXP unit);
RcppExport SEXP _units_R_ut_get_name(SEXP unitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type unit(unitSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_get_name(unit));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_get_symbol
CharacterVector R_ut_get_symbol(SEXP unit);
RcppExport SEXP _units_R_ut_get_symbol(SEXP unitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type unit(unitSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_get_symbol(unit));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_scale
SEXP R_ut_scale(SEXP unit, double factor);
RcppExport SEXP _units_R_ut_scale(SEXP unitSEXP, SEXP factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type unit(unitSEXP);
    Rcpp::traits::input_parameter< double >::type factor(factorSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_scale(unit, factor));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_offset
SEXP R_ut_offset(SEXP unit, double origin);
RcppExport SEXP _units_R_ut_offset(SEXP unitSEXP, SEXP originSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type unit(unitSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_offset(unit, origin));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_multiply
SEXP R_ut_multiply(SEXP a, SEXP b);
RcppExport SEXP _units_R_ut_multiply(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< SEXP >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_multiply(a, b));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_invert
SEXP R_ut_invert(SEXP a);
RcppExport SEXP _units_R_ut_invert(SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_invert(a));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_divide
SEXP R_ut_divide(SEXP numer, SEXP denom);
RcppExport SEXP _units_R_ut_divide(SEXP numerSEXP, SEXP denomSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type numer(numerSEXP);
    Rcpp::traits::input_parameter< SEXP >::type denom(denomSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_divide(numer, denom));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_raise
SEXP R_ut_raise(SEXP a, int i);
RcppExport SEXP _units_R_ut_raise(SEXP aSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_raise(a, i));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_root
SEXP R_ut_root(SEXP a, int i);
RcppExport SEXP _units_R_ut_root(SEXP aSEXP, SEXP iSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_root(a, i));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_log
SEXP R_ut_log(SEXP a, double base);
RcppExport SEXP _units_R_ut_log(SEXP aSEXP, SEXP baseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type base(baseSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_log(a, base));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_parse
SEXP R_ut_parse(std::string name);
RcppExport SEXP _units_R_ut_parse(SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_parse(name));
    return rcpp_result_gen;
END_RCPP
}
// R_ut_format
CharacterVector R_ut_format(SEXP p, bool names, bool definition, bool ascii);
RcppExport SEXP _units_R_ut_format(SEXP pSEXP, SEXP namesSEXP, SEXP definitionSEXP, SEXP asciiSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type p(pSEXP);
    Rcpp::traits::input_parameter< bool >::type names(namesSEXP);
    Rcpp::traits::input_parameter< bool >::type definition(definitionSEXP);
    Rcpp::traits::input_parameter< bool >::type ascii(asciiSEXP);
    rcpp_result_gen = Rcpp::wrap(R_ut_format(p, names, definition, ascii));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_units_ud_exit", (DL_FUNC) &_units_ud_exit, 0},
    {"_units_ud_init", (DL_FUNC) &_units_ud_init, 1},
    {"_units_ud_set_encoding", (DL_FUNC) &_units_ud_set_encoding, 1},
    {"_units_ud_compare", (DL_FUNC) &_units_ud_compare, 4},
    {"_units_ud_convertible", (DL_FUNC) &_units_ud_convertible, 2},
    {"_units_ud_convert_doubles", (DL_FUNC) &_units_ud_convert_doubles, 3},
    {"_units_ud_map_names", (DL_FUNC) &_units_ud_map_names, 2},
    {"_units_ud_unmap_names", (DL_FUNC) &_units_ud_unmap_names, 1},
    {"_units_ud_map_symbols", (DL_FUNC) &_units_ud_map_symbols, 2},
    {"_units_ud_unmap_symbols", (DL_FUNC) &_units_ud_unmap_symbols, 1},
    {"_units_R_ut_get_dimensionless_unit_one", (DL_FUNC) &_units_R_ut_get_dimensionless_unit_one, 0},
    {"_units_R_ut_new_base_unit", (DL_FUNC) &_units_R_ut_new_base_unit, 0},
    {"_units_R_ut_new_dimensionless_unit", (DL_FUNC) &_units_R_ut_new_dimensionless_unit, 0},
    {"_units_R_ut_get_name", (DL_FUNC) &_units_R_ut_get_name, 1},
    {"_units_R_ut_get_symbol", (DL_FUNC) &_units_R_ut_get_symbol, 1},
    {"_units_R_ut_scale", (DL_FUNC) &_units_R_ut_scale, 2},
    {"_units_R_ut_offset", (DL_FUNC) &_units_R_ut_offset, 2},
    {"_units_R_ut_multiply", (DL_FUNC) &_units_R_ut_multiply, 2},
    {"_units_R_ut_invert", (DL_FUNC) &_units_R_ut_invert, 1},
    {"_units_R_ut_divide", (DL_FUNC) &_units_R_ut_divide, 2},
    {"_units_R_ut_raise", (DL_FUNC) &_units_R_ut_raise, 2},
    {"_units_R_ut_root", (DL_FUNC) &_units_R_ut_root, 2},
    {"_units_R_ut_log", (DL_FUNC) &_units_R_ut_log, 2},
    {"_units_R_ut_parse", (DL_FUNC) &_units_R_ut_parse, 1},
    {"_units_R_ut_format", (DL_FUNC) &_units_R_ut_format, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_units(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
