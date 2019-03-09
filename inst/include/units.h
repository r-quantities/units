#if UDUNITS2_DIR != 0
# include <udunits2/udunits2.h>
#else
# include <udunits2.h>
#endif

#include "Rcpp.h"

using namespace Rcpp;

void finalizeUT(ut_unit *ptr);
typedef XPtr<ut_unit,PreserveStorage,finalizeUT,true> XPtrUT;
