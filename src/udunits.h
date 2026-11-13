#ifndef units_udunits__h
#define units_udunits__h

#include "units_types.h"

namespace units {
  // defined in udunits.cpp
  extern ut_system *sys;
  extern ut_encoding enc;
  // defined in user_units.cpp; called before the unit system is freed
  void registry_clear();
}

#endif
