#include <udunits2.h>
#include "Rcpp.h"

using namespace Rcpp;

typedef std::vector<void *> ut_vec;
void finalizeUT(ut_vec *ptr);
typedef XPtr<ut_vec,PreserveStorage,finalizeUT,true> XPtrUT;
