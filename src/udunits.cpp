/*
  part of this was modified from: https://github.com/pacificclimate/Rudunits2

  (c) James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include <Rcpp.h>

#if UDUNITS2_DIR != 0
# include <udunits2/udunits2.h>
#else
# include <udunits2.h>
#endif

using namespace Rcpp;
typedef XPtr<ut_unit, PreserveStorage, ut_free, true> XPtrUT;

void r_error_fn(const char* fmt, va_list args) {
  char buf[256];
  vsprintf(buf, fmt, args);
  stop(buf);
}

static ut_system *sys = NULL;
static ut_encoding enc = UT_UTF8;

// [[Rcpp::export]]
void udunits_init(CharacterVector path) {
  ut_set_error_message_handler(ut_ignore);
  ut_free_system(sys);
  for (int i = 0; i < path.size(); i++) {
    if ((sys = ut_read_xml(path[i])) != NULL)
      break;
  }
  if (sys == NULL)
    sys = ut_read_xml(NULL); // #nocov
  ut_set_error_message_handler((ut_error_message_handler) r_error_fn);
  if (sys == NULL)
    stop("no database found!"); // #nocov
}

// [[Rcpp::export]]
void udunits_exit() {  // #nocov start
  ut_free_system(sys);
  sys = NULL;
}                      // #nocov end

// wrap a ut_unit pointer in an XPtr
SEXP ut_wrap(ut_unit *u) {
  XPtrUT p(u);
  return p;
}

// fetch the ut_unit pointer from an XPtr wrapper
ut_unit *ut_unwrap(SEXP u) {
  XPtrUT ptr(u);
  return ((ut_unit *) ptr);
}

// [[Rcpp::export]]
SEXP R_ut_parse(CharacterVector name) {
  ut_unit *u = ut_parse(sys, ut_trim(name[0], enc), enc);
  if (u == NULL)
    stop("syntax error, cannot parse '%s'", (char*)name[0]);
  return ut_wrap(u);
}

// [[Rcpp::export]]
SEXP R_ut_get_dimensionless_unit_one() {
  return ut_wrap(ut_get_dimensionless_unit_one(sys));
}

// [[Rcpp::export]]
LogicalVector R_ut_are_convertible(SEXP a, SEXP b) {
  ut_unit *u1 = ut_unwrap(a);
  ut_unit *u2 = ut_unwrap(b);
  if (u1 == NULL || u2 == NULL)
     return false;  	// #nocov
  return ut_are_convertible(u1, u2) != 0;
}

// [[Rcpp::export]]
NumericVector R_convert_doubles(SEXP from, SEXP to, NumericVector val) {
  if (! ut_are_convertible(ut_unwrap(from), ut_unwrap(to)))
    stop("units are not convertible");
  cv_converter *cv = ut_get_converter(ut_unwrap(from), ut_unwrap(to));
  NumericVector out(val.size());
  cv_convert_doubles(cv, &(val[0]), val.size(), &(out[0]));
  cv_free(cv);
  return out;
}

// [[Rcpp::export]]
SEXP R_ut_new_dimensionless_unit() {
  return ut_wrap(ut_new_dimensionless_unit(sys));
}

// [[Rcpp::export]]
SEXP R_ut_new_base_unit() {
  return ut_wrap(ut_new_base_unit(sys));
}

// [[Rcpp::export]]
void R_ut_scale(CharacterVector nw, CharacterVector old, NumericVector d) {
  ut_unit *u_old = ut_parse(sys, ut_trim(old[0], enc), enc);
  ut_unit *u_new = ut_scale(d[0], u_old);
  ut_map_name_to_unit(nw[0], enc, u_new);
  ut_free(u_new);
  ut_free(u_old);
}

// [[Rcpp::export]]
void R_ut_offset(CharacterVector nw, CharacterVector old, NumericVector d) {
  ut_unit *u_old = ut_parse(sys, ut_trim(old[0], enc), enc);
  ut_unit *u_new = ut_offset(u_old, d[0]);
  ut_map_name_to_unit(nw[0], enc, u_new);
  ut_free(u_new);
  ut_free(u_old);
}

// [[Rcpp::export]]
SEXP R_ut_divide(SEXP numer, SEXP denom) {
  return ut_wrap(ut_divide(ut_unwrap(numer), ut_unwrap(denom)));
}

// [[Rcpp::export]]
SEXP R_ut_multiply(SEXP a, SEXP b) {
  return ut_wrap(ut_multiply(ut_unwrap(a), ut_unwrap(b)));
}

// [[Rcpp::export]]
SEXP R_ut_invert(SEXP a) {
  return ut_wrap(ut_invert(ut_unwrap(a)));
}

// [[Rcpp::export]]
SEXP R_ut_raise(SEXP a, IntegerVector i) {
  if (i.length() != 1)
    stop("i should have length 1");
  return ut_wrap(ut_raise(ut_unwrap(a), i[0]));
}

// [[Rcpp::export]]
SEXP R_ut_root(SEXP a, IntegerVector i) {
  if (i.length() != 1)
    stop("i should have length 1");
  return ut_wrap(ut_root(ut_unwrap(a), i[0]));
}

// [[Rcpp::export]]
SEXP R_ut_log(SEXP a, NumericVector base) {
  if (base.length() != 1)
    stop("base should have length 1");
  if (base[0] <= 0)
    stop("base should be positive");
  return ut_wrap(ut_log(base[0], ut_unwrap(a)));
}

// [[Rcpp::export]]
CharacterVector R_ut_format(SEXP p, bool names = false, bool definition = false,
                            bool ascii = false)
{
  int opt;
  if (! ascii)
    opt = enc;
  else
    opt = UT_ASCII;
  if (names)
    opt = opt | UT_NAMES;
  if (definition)
    opt = opt | UT_DEFINITION;
  char buf[256];
  if (ut_format(ut_unwrap(p), buf, 256, opt) == 256)
    warning("buffer of 256 bytes too small!"); // #nocov
  return CharacterVector::create(buf);
}

// [[Rcpp::export]]
void R_ut_set_encoding(std::string enc_str) {
  if (enc_str.compare("utf8") == 0)
    enc = UT_UTF8;
  else if (enc_str.compare("ascii") == 0)
    enc = UT_ASCII;
  else if (enc_str.compare("iso-8859-1") == 0 || enc_str.compare("latin1") == 0)
    enc = UT_LATIN1;
  else
    stop("Valid encoding string parameters are ('utf8'|'ascii'|'iso-8859-1','latin1')");
}

// [[Rcpp::export]]
CharacterVector R_ut_get_symbol(CharacterVector ustr) {
  ut_unit *u = ut_parse(sys, ut_trim(ustr[0], enc), enc);
  const char *s = ut_get_symbol(u, enc);
  ut_free(u);
  if (s == NULL)
    return CharacterVector::create();
  else
    return CharacterVector::create(s);
}

// [[Rcpp::export]]
CharacterVector R_ut_get_name(CharacterVector ustr) {
  ut_unit *u = ut_parse(sys, ut_trim(ustr[0], enc), enc);
  const char *s = ut_get_name(u, enc);
  ut_free(u);
  if (s == NULL)
    return CharacterVector::create();
  else
    return CharacterVector::create(s); // #nocov
}

// [[Rcpp::export]]
void R_ut_map_name_to_unit(CharacterVector name, SEXP inunit) {
  ut_unit *unit = ut_unwrap(inunit);
  for (int i = 0; i < name.size(); i++)
    ut_map_name_to_unit(name[i], enc, unit);
}

// [[Rcpp::export]]
void R_ut_unmap_name_to_unit(CharacterVector name) {
  for (int i = 0; i < name.size(); i++)
    ut_unmap_name_to_unit(sys, name[i], enc);
}

// [[Rcpp::export]]
void R_ut_map_symbol_to_unit(CharacterVector name, SEXP inunit) {
  ut_unit *unit = ut_unwrap(inunit);
  for (int i = 0; i < name.size(); i++)
    ut_map_symbol_to_unit(name[i], enc, unit);
}

// [[Rcpp::export]]
void R_ut_unmap_symbol_to_unit(CharacterVector name) {
  for (int i = 0; i < name.size(); i++)
    ut_unmap_symbol_to_unit(sys, name[i], enc);
}
