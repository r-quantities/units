#ifndef units_types__h
#define units_types__h

#include <Rcpp.h>

#if UDUNITS2_DIR != 0
# include <udunits2/udunits2.h>
#else
# include <udunits2.h>
#endif

typedef Rcpp::XPtr<ut_unit, Rcpp::PreserveStorage, ut_free, true> xut_unit;
typedef Rcpp::XPtr<cv_converter, Rcpp::PreserveStorage, cv_free, true> xcv_converter;

#endif