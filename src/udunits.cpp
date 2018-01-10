/*
  modified from: https://github.com/pacificclimate/Rudunits2

  (c) James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include "Rcpp.h"
using namespace Rcpp;

#include <R.h>
#include <udunits2.h>
#include <stdio.h> /* FILENAME_MAX */

ut_system *sys = NULL;
static ut_encoding enc;

extern "C" {
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

LogicalVector R_ut_has_system(List foo) {
  LogicalVector ret(1);
  if (sys != NULL)
    ret(0) = true;
  else 
    ret(0) = false;
  return ret;
}

typedef std::vector<void *> ut_vec;

void finalize_ut(ut_vec *v) {
	for (int i = 0; i < v->size(); i++)
		ut_free((ut_unit *) ((*v)[i]));
}

// typedef XPtr<ut_vec,PreserveStorage,finalize_ut> XPtrUT;
typedef XPtr<ut_vec> XPtrUT;

// [[Rcpp::export]]
XPtr< std::vector<void *> > R_ut_parse(CharacterVector name) {
	ut_vec *v = new ut_vec;
	Rcout << name[0] << std::endl;
	ut_unit *u = ut_parse(sys, name[0], enc);
    if (u == NULL) {
		switch (ut_get_status()) {
			case UT_BAD_ARG:
			case UT_SYNTAX:
			case UT_UNKNOWN:
			case UT_OS:
				handle_error("R_ut_parse");
		}
	}
	// error checking ...
	v->push_back(u);
	XPtrUT p(v);
	return(p);
}

// [[Rcpp::export]]
CharacterVector R_ut_format(SEXP p, bool names = false, bool definition = false) {
	XPtr< std::vector<void *> > ptr(p);
	int opt = enc;
	if (names)
		opt = opt | UT_NAMES;
	if (definition)
		opt = opt | UT_DEFINITION;
	char buf[256];
	int i = ut_format((ut_unit *) (*ptr)[0], buf, 256, opt);
	if (i == -1) {
		switch (ut_get_status()) {
			UT_BAD_ARG:
			UT_CANT_FORMAT:
					handle_error("R_ut_format");
					break;
			default:;
		}
	}
	return CharacterVector::create(buf);
}
