/*
  part of this was modified from: https://github.com/pacificclimate/Rudunits2

  (c) James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include "units_types.h"

extern "C" {
  void r_error_fn(const char* fmt, va_list args) {
    char buf[256];
    vsnprintf(buf, (size_t) 256, fmt, args);
    Rcpp::stop("%s", buf);
  }
}

using namespace Rcpp;

static ut_system *sys = NULL;
static ut_encoding enc = UT_UTF8;

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

  xut_unit ux(ut_parse(sys, ut_trim(xn.data(), enc), enc));
  xut_unit uy(ut_parse(sys, ut_trim(yn.data(), enc), enc));

  if (ut_compare(ux.get(), uy.get()) != 0) {
    NumericVector y_cv = clone(y);
    xcv_converter cv(ut_get_converter(uy.get(), ux.get()));
    cv_convert_doubles(cv, &(y_cv[0]), y_cv.size(), &(y_cv[0]));
    std::swap(y, y_cv);
  }

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
  xut_unit u_from(ut_parse(sys, ut_trim(from.data(), enc), enc));
  xut_unit u_to(ut_parse(sys, ut_trim(to.data(), enc), enc));

  if (!u_from || !u_to)
    return false;
  return ut_are_convertible(u_from, u_to) != 0;
}

// [[Rcpp::export(rng=false)]]
NumericVector ud_convert_doubles(NumericVector x, std::string from, std::string to) {
  if (x.size() == 0) return x;
  NumericVector out = clone(x);

  xut_unit u_from(ut_parse(sys, ut_trim(from.data(), enc), enc));
  xut_unit u_to(ut_parse(sys, ut_trim(to.data(), enc), enc));

  xcv_converter cv(ut_get_converter(u_from.get(), u_to.get()));
  cv_convert_doubles(cv, &(x[0]), x.size(), &(out[0]));

  return out;
}

// [[Rcpp::export(rng=false)]]
void ud_map_names(CharacterVector names, xut_unit unit) {
  if (!names.size() || !unit) return;

  for (int i = 0; i < names.size(); i++)
    ut_map_name_to_unit(ut_trim(names[i], enc), enc, unit);
  ut_map_unit_to_name(unit, ut_trim(names[0], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_names(CharacterVector names) {
  if (!names.size()) return;

  xut_unit unit(ut_parse(sys, ut_trim(names[0], enc), enc));
  if (!unit) return;

  ut_unmap_unit_to_name(unit, enc);
  for (int i = 0; i < names.size(); i++)
    ut_unmap_name_to_unit(sys, ut_trim(names[i], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_map_symbols(CharacterVector symbols, xut_unit unit) {
  if (!symbols.size() || !unit) return;

  for (int i = 0; i < symbols.size(); i++)
    ut_map_symbol_to_unit(ut_trim(symbols[i], enc), enc, unit);
  ut_map_unit_to_symbol(unit, ut_trim(symbols[0], enc), enc);
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_symbols(CharacterVector symbols) {
  if (!symbols.size()) return;

  xut_unit unit(ut_parse(sys, ut_trim(symbols[0], enc), enc));
  if (!unit) return;

  ut_unmap_unit_to_symbol(unit, enc);
  for (int i = 0; i < symbols.size(); i++)
    ut_unmap_symbol_to_unit(sys, ut_trim(symbols[i], enc), enc);
}

/* Thin wrappers **************************************************************/

// # nocov start

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_get_dimensionless_unit_one() {
  return xut_unit(ut_get_dimensionless_unit_one(sys));
}

// # nocov end

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_new_base_unit() {
  return xut_unit(ut_new_base_unit(sys));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_new_dimensionless_unit() {
  return xut_unit(ut_new_dimensionless_unit(sys));
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_name(xut_unit unit) {
  const char *s = ut_get_name(unit, enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s); // #nocov
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_symbol(xut_unit unit) {
  const char *s = ut_get_symbol(unit, enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s);
}

// # nocov start

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_scale(xut_unit unit, double factor) {
  return xut_unit(ut_scale(factor, unit));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_offset(xut_unit unit, double origin) {
  return xut_unit(ut_offset(unit, origin));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_multiply(xut_unit a, xut_unit b) {
  return xut_unit(ut_multiply(a, b));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_invert(xut_unit a) {
  return xut_unit(ut_invert(a));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_divide(xut_unit numer, xut_unit denom) {
  return xut_unit(ut_divide(numer, denom));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_raise(xut_unit a, int i) {
  return xut_unit(ut_raise(a, i));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_root(xut_unit a, int i) {
  return xut_unit(ut_root(a, i));
}

// # nocov end

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_log(xut_unit a, double base) {
  return xut_unit(ut_log(base, a));
}

// [[Rcpp::export(rng=false)]]
xut_unit R_ut_parse(std::string name) {
  xut_unit u(ut_parse(sys, ut_trim(name.data(), enc), enc));
  if (!u)
    stop("syntax error, cannot parse '%s'", name);
  return u;
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_format(xut_unit p, bool names = false,
                            bool definition = false, bool ascii = false)
{
  int opt = UT_ASCII;
  if (!ascii)
    opt = enc;
  if (names)
    opt = opt | UT_NAMES;
  if (definition)
    opt = opt | UT_DEFINITION;
  char buf[256];
  if (ut_format(p, buf, sizeof(buf), opt) == sizeof(buf))
    warning("buffer too small!"); // #nocov
  return CharacterVector::create(buf);
}
