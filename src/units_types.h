#ifndef units_types__h
#define units_types__h

#include <Rcpp.h>
#include <memory>

#if UDUNITS2_DIR != 0
# include <udunits2/udunits2.h>
#else
# include <udunits2.h>
#endif

// udunits objects are owned by these and never handed to R. The finalizer of an
// external pointer can run after ud_exit() has freed the unit system that owns
// the unit, or after this DLL has been unloaded, and then crashes R.
struct ut_unit_deleter {
  void operator()(ut_unit* unit) const { ut_free(unit); }
};
struct cv_converter_deleter {
  void operator()(cv_converter* cv) const { cv_free(cv); }
};

typedef std::unique_ptr<ut_unit, ut_unit_deleter> ut_unit_ptr;
typedef std::unique_ptr<cv_converter, cv_converter_deleter> cv_converter_ptr;

#endif
