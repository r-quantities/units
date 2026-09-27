/*
  part of this was modified from: https://github.com/pacificclimate/Rudunits2

  (c) James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include "units_types.h"

extern "C" {
  int r_error_fn(const char* fmt, va_list args) {
    char buf[256];
    vsnprintf(buf, (size_t) 256, fmt, args);
    Rcpp::stop("%s", buf);
    return 0;
  }
}

using namespace Rcpp;

static ut_system *sys = NULL;
static ut_encoding enc = UT_UTF8;

/* Helpers ********************************************************************/

static ut_unit_ptr parse_or_stop(std::string name) {
  ut_unit_ptr unit(ut_parse(sys, ut_trim(name.data(), enc), enc));
  if (!unit)
    stop("syntax error, cannot parse '%s'", name);
  return unit;
}

static CharacterVector format_unit(const ut_unit* unit, int opt) {
  char buf[256];
  if (ut_format(unit, buf, sizeof(buf), opt) == sizeof(buf))
    warning("buffer too small!"); // #nocov
  return CharacterVector::create(buf);
}

static void map_names(CharacterVector names, ut_unit* unit) {
  if (!names.size() || !unit) return;

  for (int i = 0; i < names.size(); i++) {
    ut_map_name_to_unit(ut_trim(names[i], UT_ASCII), UT_ASCII, unit);
    ut_map_name_to_unit(ut_trim(names[i], enc), enc, unit);
  }
  ut_map_unit_to_name(unit, ut_trim(names[0], UT_ASCII), UT_ASCII);
  ut_map_unit_to_name(unit, ut_trim(names[0], enc), enc);
}

static void map_symbols(CharacterVector symbols, ut_unit* unit) {
  if (!symbols.size() || !unit) return;

  for (int i = 0; i < symbols.size(); i++) {
    ut_map_symbol_to_unit(ut_trim(symbols[i], UT_ASCII), UT_ASCII, unit);
    ut_map_symbol_to_unit(ut_trim(symbols[i], enc), enc, unit);
  }
  ut_map_unit_to_symbol(unit, ut_trim(symbols[0], UT_ASCII), UT_ASCII);
  ut_map_unit_to_symbol(unit, ut_trim(symbols[0], enc), enc);
}

/* High-level functions *******************************************************/

// [[Rcpp::export(rng=false)]]
void ud_exit() {
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
  ut_set_error_message_handler(r_error_fn);
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

  ut_unit_ptr ux(ut_parse(sys, ut_trim(xn.data(), enc), enc));
  ut_unit_ptr uy(ut_parse(sys, ut_trim(yn.data(), enc), enc));

  if (ut_compare(ux.get(), uy.get()) != 0) {
    NumericVector y_cv = clone(y);
    cv_converter_ptr cv(ut_get_converter(uy.get(), ux.get()));
    cv_convert_doubles(cv.get(), &(y_cv[0]), y_cv.size(), &(y_cv[0]));
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
  ut_unit_ptr u_from(ut_parse(sys, ut_trim(from.data(), enc), enc));
  ut_unit_ptr u_to(ut_parse(sys, ut_trim(to.data(), enc), enc));

  if (!u_from || !u_to)
    return false;
  return ut_are_convertible(u_from.get(), u_to.get()) != 0;
}

// [[Rcpp::export(rng=false)]]
NumericVector ud_convert_doubles(NumericVector x, std::string from, std::string to) {
  if (x.size() == 0) return x;
  NumericVector out = clone(x);

  ut_unit_ptr u_from(ut_parse(sys, ut_trim(from.data(), enc), enc));
  ut_unit_ptr u_to(ut_parse(sys, ut_trim(to.data(), enc), enc));

  cv_converter_ptr cv(ut_get_converter(u_from.get(), u_to.get()));
  cv_convert_doubles(cv.get(), &(x[0]), x.size(), &(out[0]));

  return out;
}

// Maps symbols and names to a new base unit if def is empty, to a new
// dimensionless unit if def is "unitless", and to the unit def defines otherwise.
// [[Rcpp::export(rng=false)]]
void ud_map_unit(CharacterVector symbols, CharacterVector names,
                 CharacterVector def)
{
  ut_unit_ptr unit;
  if (!def.size())
    unit.reset(ut_new_base_unit(sys));
  else if (as<std::string>(def[0]) == "unitless")
    unit.reset(ut_new_dimensionless_unit(sys));
  else unit = parse_or_stop(as<std::string>(def[0]));

  map_symbols(symbols, unit.get());
  map_names(names, unit.get());
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_names(CharacterVector names) {
  if (!names.size()) return;

  ut_unit_ptr unit(ut_parse(sys, ut_trim(names[0], enc), enc));
  if (!unit) return;

  ut_unmap_unit_to_name(unit.get(), enc);
  ut_unmap_unit_to_name(unit.get(), UT_ASCII);
  for (int i = 0; i < names.size(); i++) {
    ut_unmap_name_to_unit(sys, ut_trim(names[i], enc), enc);
    ut_unmap_name_to_unit(sys, ut_trim(names[i], UT_ASCII), UT_ASCII);
  }
}

// [[Rcpp::export(rng=false)]]
void ud_unmap_symbols(CharacterVector symbols) {
  if (!symbols.size()) return;

  ut_unit_ptr unit(ut_parse(sys, ut_trim(symbols[0], enc), enc));
  if (!unit) return;

  ut_unmap_unit_to_symbol(unit.get(), enc);
  ut_unmap_unit_to_symbol(unit.get(), UT_ASCII);
  for (int i = 0; i < symbols.size(); i++) {
    ut_unmap_symbol_to_unit(sys, ut_trim(symbols[i], enc), enc);
    ut_unmap_symbol_to_unit(sys, ut_trim(symbols[i], UT_ASCII), UT_ASCII);
  }
}

/* Thin wrappers **************************************************************/

// These take a unit string rather than a unit, see units_types.h.

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_name(std::string unit) {
  ut_unit_ptr u(parse_or_stop(unit));
  const char *s = ut_get_name(u.get(), enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s); // #nocov
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_get_symbol(std::string unit) {
  ut_unit_ptr u(parse_or_stop(unit));
  const char *s = ut_get_symbol(u.get(), enc);
  if (s == NULL)
    return CharacterVector::create();
  return CharacterVector::create(s);
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_log(std::string unit, double base) {
  ut_unit_ptr u(parse_or_stop(unit));
  ut_unit_ptr u_log(ut_log(base, u.get()));
  return format_unit(u_log.get(), UT_ASCII);
}

// Throws if unit cannot be parsed.
// [[Rcpp::export(rng=false)]]
void R_ut_parse(std::string unit) {
  parse_or_stop(unit);
}

// [[Rcpp::export(rng=false)]]
CharacterVector R_ut_format(std::string unit, bool names = false,
                            bool definition = false, bool ascii = false)
{
  int opt = UT_ASCII;
  if (!ascii)
    opt = enc;
  if (names)
    opt = opt | UT_NAMES;
  if (definition)
    opt = opt | UT_DEFINITION;
  ut_unit_ptr u(parse_or_stop(unit));
  return format_unit(u.get(), opt);
}
