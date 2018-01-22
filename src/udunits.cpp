/*
  part of this was modified from: https://github.com/pacificclimate/Rudunits2

  (c) James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include "Rcpp.h"

using namespace Rcpp;

extern "C" {
#include <R.h>
#include <udunits2.h>
#include "units.h"

ut_system *sys = NULL;
static ut_encoding enc = UT_UTF8;

#include "io.h"
}

// [[Rcpp::export]]
LogicalVector udunits_init(CharacterVector path, bool warn_on_failure = false) {

  LogicalVector ret(1);
  ut_set_error_message_handler((ut_error_message_handler) r_error_fn);
  if (sys != NULL)
    ut_free_system(sys);
  
  sys = NULL;
  ut_set_error_message_handler(ut_ignore);
  for (int i = 0; i < path.size(); i++) {
    sys = ut_read_xml(path[i]);
    if (sys != NULL)
      break;
  }
  if (sys == NULL)
    sys = ut_read_xml(NULL);
  ut_set_error_message_handler((ut_error_message_handler) r_error_fn);
  if (sys == NULL) {
    if (warn_on_failure)
      handle_error("udunit_init");
    ret[0] = false;
  } else
    ret[0] = true;
  enc = UT_UTF8;
  return ret;
}

// [[Rcpp::export]]
LogicalVector udunits_exit(LogicalVector lo) {
  if (sys != NULL)
    ut_free_system(sys);
  sys = NULL;
  return lo;
}

LogicalVector R_ut_has_system(List foo) {
  LogicalVector ret(1);
  if (sys != NULL)
    ret(0) = true;
  else 
    ret(0) = false;
  return ret;
}

// typedef std::vector<void *> ut_vec;

void finalizeUT(ut_unit *ptr) {
  ut_free(ptr);
}

// typedef XPtr<ut_unit,PreserveStorage,finalizeUT> XPtrUT;

// wrap a ut_unit pointer in an XPtr
XPtrUT ut_wrap(ut_unit *u) {
  XPtrUT p(u);
  return p;
}

// fetch the ut_unit pointer from an XPtr wrapper
ut_unit *ut_unwrap(SEXP u) {
  XPtrUT ptr(u);
  return ((ut_unit *) ptr);
}

// [[Rcpp::export]]
XPtrUT R_ut_parse(CharacterVector name) {
  ut_unit *u = ut_parse(sys, ut_trim(name[0], enc), enc);
  if (u == NULL) {
    switch (ut_get_status()) {
    case UT_BAD_ARG:
    case UT_SYNTAX:
    case UT_UNKNOWN:
    case UT_OS:
      handle_error("R_ut_parse");
    default:;
    }
  }
  // error checking ...
  return ut_wrap(u);
}

// [[Rcpp::export]]
XPtrUT R_ut_get_dimensionless_unit_one(CharacterVector name) {
  return ut_wrap(ut_get_dimensionless_unit_one(sys));
}
// [[Rcpp::export]]
LogicalVector R_ut_are_convertible(SEXP a, SEXP b) {
  int i = ut_are_convertible(ut_unwrap(a), ut_unwrap(b));
  return i != 0;
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
XPtrUT R_ut_new_dimensionless_unit(CharacterVector name) {
  ut_unit *u = ut_new_dimensionless_unit(sys); 
  if (ut_map_name_to_unit(name[0], enc, u) != UT_SUCCESS)
    handle_error("R_ut_new_dimensionless");
  return ut_wrap(u);
}

// [[Rcpp::export]]
XPtrUT R_ut_scale(CharacterVector nw, CharacterVector old, NumericVector d) {
  if (d.size() != 1)
    stop("d should have size 1");
  ut_unit *u_old = ut_parse(sys, ut_trim(old[0], enc), enc);
  ut_unit *u_new = ut_scale(d[0], u_old);
  if (ut_map_name_to_unit(nw[0], enc, u_new) != UT_SUCCESS)
    handle_error("R_ut_add_scale");
  ut_free(u_old);
  return ut_wrap(u_new);
}

// [[Rcpp::export]]
XPtrUT R_ut_offset(CharacterVector nw, CharacterVector old, NumericVector d) {
  if (d.size() != 1)
    stop("d should have size 1");
  ut_unit *u_old = ut_parse(sys, ut_trim(old[0], enc), enc);
  ut_unit *u_new = ut_offset(u_old, d[0]);
  if (ut_map_name_to_unit(nw[0], enc, u_new) != UT_SUCCESS)
    handle_error("R_ut_offset");
  ut_free(u_old);
  return ut_wrap(u_new);
}

// [[Rcpp::export]]
XPtrUT R_ut_divide(SEXP numer, SEXP denom) {
  return ut_wrap(ut_divide(ut_unwrap(numer), ut_unwrap(denom)));
}

// [[Rcpp::export]]
XPtrUT R_ut_multiply(SEXP a, SEXP b) {
  return ut_wrap(ut_multiply(ut_unwrap(a), ut_unwrap(b)));
}

// [[Rcpp::export]]
XPtrUT R_ut_invert(SEXP a) {
  return ut_wrap(ut_invert(ut_unwrap(a)));
}

// [[Rcpp::export]]
XPtrUT R_ut_raise(SEXP a, IntegerVector i) {
  if (i.length() != 1)
    stop("i should have length 1");
  return ut_wrap(ut_raise(ut_unwrap(a), i[0]));
}

// [[Rcpp::export]]
XPtrUT R_ut_root(SEXP a, IntegerVector i) {
  if (i.length() != 1)
    stop("i should have length 1");
  return ut_wrap(ut_root(ut_unwrap(a), i[0]));
}

// [[Rcpp::export]]
XPtrUT R_ut_log(SEXP a, NumericVector base) {
  if (base.length() != 1)
    stop("base should have length 1");
  if (base[0] <= 0)
    stop("base should be positive");
  return ut_wrap(ut_log(base[0], ut_unwrap(a)));
}

// [[Rcpp::export]]
CharacterVector R_ut_format(SEXP p, bool names = false, bool definition = false, 
    bool ascii = false) {
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
  ut_set_error_message_handler(ut_ignore);
  int len = ut_format(ut_unwrap(p), buf, 256, opt);
  ut_set_error_message_handler((ut_error_message_handler) r_error_fn);
  if (len == -1) {
    switch (ut_get_status()) {
    case UT_BAD_ARG:
    case UT_CANT_FORMAT:
      handle_error("R_ut_format");
      break;
    default:;
    }
    buf[0] = '\0'; // "": dont' return rubbish
  } else if (len == 256)
    handle_error("buffer of 256 bytes too small!");
  return CharacterVector::create(buf);
}

// [[Rcpp::export]]
CharacterVector R_ut_set_encoding(CharacterVector enc_str) {
  const char *e = enc_str[0];
  if (strcmp(e, "utf8") == 0)
    enc = UT_UTF8;
  else if (strcmp(e, "ascii") == 0)
    enc = UT_ASCII;
  else if (strcmp(e, "iso-8859-1") == 0 || strcmp(e, "latin1") == 0)
    enc = UT_LATIN1;
  else
    stop("Valid encoding string parameters are ('utf8'|'ascii'|'iso-8859-1','latin1')");
  return enc_str;
}

// [[Rcpp::export]]
CharacterVector R_ut_get_symbol(CharacterVector ustr) {
  ut_unit *u = ut_parse(sys, ut_trim(ustr[0], enc), enc);
  if (u == NULL)
    handle_error("R_ut_get_name");
  const char *s = ut_get_symbol(u, enc);
  ut_free(u);
  if (s == NULL)
    return CharacterVector::create("");
  else
    return CharacterVector::create(s);
}

// [[Rcpp::export]]
CharacterVector R_ut_get_name(CharacterVector ustr) {
  ut_unit *u = ut_parse(sys, ut_trim(ustr[0], enc), enc);
  if (u == NULL)
    handle_error("R_ut_get_name");
  const char *s = ut_get_name(u, enc);
  ut_free(u);
  if (s == NULL)
    return CharacterVector::create("");
  else
    return CharacterVector::create(s);
}

// https://github.com/r-quantities/units/issues/89#issuecomment-359251623
// [[Rcpp::export]]
XPtrUT R_ut_map_name_to_unit( CharacterVector name, SEXP inunit) {
  ut_unit *unit = ut_unwrap(inunit);
  if (ut_map_name_to_unit(name[0], enc, unit) != UT_SUCCESS)
    handle_error("R_ut_map_name_to_unit");
  return ut_wrap(unit);
}
