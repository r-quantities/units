#include <udunits2.h>
#include "Rcpp.h"

using namespace Rcpp;

void finalizeUT(ut_unit *ptr);
typedef XPtr<ut_unit,PreserveStorage,finalizeUT,true> XPtrUT;
