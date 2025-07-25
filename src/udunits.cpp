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

extern "C" {
  void r_error_fn(const char* fmt, va_list args) {
    char buf[256];
    vsnprintf(buf, (size_t) 256, fmt, args);
    Rcpp::stop("%s", buf);
  }
}

using namespace Rcpp;
typedef XPtr<ut_unit, PreserveStorage, ut_free, true> XPtrUT;

static ut_system *sys = NULL;
static ut_encoding enc = UT_UTF8;

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

/* High-level functions *******************************************************/

// [[Rcpp::export(rng=false)]]
void ud_exit() {
  R_gc();
  ut_free_system(sys);
  sys = NULL;
}

// [[Rcpp::export(rng=false)]]
void ud_init(CharacterVector path) {
  ut_set_error_message_handler(ut_ignore);
  ud_exit();
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

// [[Rcpp::export(rng=false)]]
void ud_set_encoding(std::string enc_str) {
  if (enc_str.compare("utf8") == 0)
    enc = UT_UTF8;
  else if (enc_str.compare("ascii") == 0)
    enc = UT_ASCII;
  else if (enc_str.compare("iso-8859-1") == 0 || enc_str.compare("latin1") == 0)
    enc = UT_LATIN1;
  else
    stop("Valid encoding string parameters are ('utf8'|'ascii'|'iso-8859-1','latin1')");
}

// [[Rcpp::export(rng=false)]]
IntegerVector ud_compare(NumericVector x, NumericVector y,
                         std::string xn, std::string yn)
{
  bool swapped = false;

  if (y.size() > x.size()) {
    std::swap(x, y);
    std::swap(xn, yn);
    swapped = true;
  }

  IntegerVector out(x.size());
  if (y.size() == 0) return IntegerVector(0);
  for (std::string &attr : x.attributeNames())
    out.attr(attr) = x.attr(attr);

  ut_unit *ux = ut_parse(sys, ut_trim(xn.data(), enc), enc);
  ut_unit *uy = ut_parse(sys, ut_trim(yn.data(), enc), enc);

  if (ut_compare(ux, uy) != 0) {
    NumericVector y_cv = clone(y);
    cv_converter *cv = ut_get_converter(uy, ux);
    cv_convert_doubles(cv, &(y_cv[0]), y_cv.size(), &(y_cv[0]));
    cv_free(cv);
    std::swap(y, y_cv);
  }

  ut_free(ux);
  ut_free(uy);

  for (int i=0, j=0; i < x.size(); i++, j++) {
    if (j == y.size())
      j = 0;
    double diff = x[i] - y[j];
    // double lnum = std::abs(x[i]) - std::abs(y[i]) > 0 ? x[i] : y[i];
    // double tol = std::abs(lnum) * std::numeric_limits<double>::epsilon();
    if (x[i] == y[j]) // || std::abs(diff) < tol)
      out[i] = 0;
    else if (ISNAN(diff))
      out[i] = NA_INTEGER;
    else out[i] = diff < 0 ? -1 : 1;
  }

  if (swapped)
    out = -out;
  return out;
}

// [[Rcpp::export(rng=false)]]
LogicalVector ud_convertible(std::string from, std::string to) {
  bool convertible = false;

  ut_unit *u_from = ut_parse(sys, ut_trim(from.data(), enc), enc);
  ut_unit *u_to = ut_parse(sys, ut_trim(to.data(), enc), enc);

  if (u_from == NULL || u_to == NULL)
    goto finished;  	// #nocov
  convertible = ut_are_convertible(u_from, u_to) != 0;

finished:
  ut_free(u_from);
  ut_free(u_to);
  return convertible;
}

// [[Rcpp::export(rng=false)]]
NumericVector ud_convert_doubles(NumericVector x, std::string from, std::string to) {
  if (x.size() == 0) return x;
  NumericVector out = clone(x);

  ut_unit *u_from = ut_parse(sys, ut_trim(from.data(), enc), enc);
  ut_unit *u_to = ut_parse(sys, ut_trim(to.data(), enc), enc);

  cv_converter *cv = ut_get_converter(u_from, u_to);
  cv_convert_doubles(cv, &(x[0]), x.size(), &(out[0]));

  cv_free(cv);
  ut_free(u_from);
  ut_free(u_to);
  return out;
}

// [[Rcpp::export(rng=false)]]
void ud_map_names(CharacterVector names, SEXP inunit) {
  if (!names.size()) return;

  ut_unit *unit = ut_unwrap(inunit);
  for (int i = 0; i < names.size(); i++)
    ut_map_name_to_unit(ut_trim(names[i], enc), enc, unit);
  ut_map_unit_to_name(unit, ut_trim(names[0], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_names(CharacterVector names) {
  if (!names.size()) return;

  ut_unit *unit = ut_parse(sys, ut_trim(names[0], enc), enc);
  ut_unmap_unit_to_name(unit, enc);
  ut_free(unit);
  for (int i = 0; i < names.size(); i++)
    ut_unmap_name_to_unit(sys, ut_trim(names[i], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_map_symbols(CharacterVector symbols, SEXP inunit) {
  if (!symbols.size()) return;

  ut_unit *unit = ut_unwrap(inunit);
  for (int i = 0; i < symbols.size(); i++)
    ut_map_symbol_to_unit(ut_trim(symbols[i], enc), enc, unit);
  ut_map_unit_to_symbol(unit, ut_trim(symbols[0], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_symbols(CharacterVector symbols) {
  if (!symbols.size()) return;

  ut_unit *unit = ut_parse(sys, ut_trim(symbols[0], enc), enc);
  ut_unmap_unit_to_symbol(unit, enc);
  ut_free(unit);
  for (int i = 0; i < symbols.size(); i++)
    ut_unmap_symbol_to_unit(sys, ut_trim(symbols[i], enc), enc);
}

/* Thin wrappers **************************************************************/

// # nocov start

// [[Rcpp::export(rng=false)]]
SEXP R_ut_get_dimensionless_unit_one() {
  return ut_wrap(ut_get_dimensionless_unit_one(sys));
}

// # nocov end

// [[Rcpp::export(rng=false)]]
SEXP R_ut_new_base_unit() {
  return ut_wrap(ut_new_base_unit(sys));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_new_dimensionless_unit() {
  return ut_wrap(ut_new_dimensionless_unit(sys));
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_name(SEXP unit) {
  const char *s = ut_get_name(ut_unwrap(unit), enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s); // #nocov
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_symbol(SEXP unit) {
  const char *s = ut_get_symbol(ut_unwrap(unit), enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s);
}

// # nocov start

// [[Rcpp::export(rng=false)]]
SEXP R_ut_scale(SEXP unit, double factor) {
  return ut_wrap(ut_scale(factor, ut_unwrap(unit)));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_offset(SEXP unit, double origin) {
  return ut_wrap(ut_offset(ut_unwrap(unit), origin));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_multiply(SEXP a, SEXP b) {
  return ut_wrap(ut_multiply(ut_unwrap(a), ut_unwrap(b)));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_invert(SEXP a) {
  return ut_wrap(ut_invert(ut_unwrap(a)));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_divide(SEXP numer, SEXP denom) {
  return ut_wrap(ut_divide(ut_unwrap(numer), ut_unwrap(denom)));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_raise(SEXP a, int i) {
  return ut_wrap(ut_raise(ut_unwrap(a), i));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_root(SEXP a, int i) {
  return ut_wrap(ut_root(ut_unwrap(a), i));
}

// # nocov end

// [[Rcpp::export(rng=false)]]
SEXP R_ut_log(SEXP a, double base) {
  return ut_wrap(ut_log(base, ut_unwrap(a)));
}

// [[Rcpp::export(rng=false)]]
SEXP R_ut_parse(std::string name) {
  ut_unit *u = ut_parse(sys, ut_trim(name.data(), enc), enc);
  if (u == NULL)
    stop("syntax error, cannot parse '%s'", name);
  return ut_wrap(u);
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_format(SEXP p, bool names = false, bool definition = false,
                            bool ascii = false)
{
  int opt = UT_ASCII;
  if (!ascii)
    opt = enc;
  if (names)
    opt = opt | UT_NAMES;
  if (definition)
    opt = opt | UT_DEFINITION;
  char buf[256];
  if (ut_format(ut_unwrap(p), buf, sizeof(buf), opt) == sizeof(buf))
    warning("buffer too small!"); // #nocov
  return CharacterVector::create(buf);
}
