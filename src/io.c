#include <R.h>
#include <udunits2.h>
#include <stdio.h> /* FILENAME_MAX */

#include "io.h"

/* From the enum comments in udunits2.h */
const char * ut_status_strings[] = {
  "Success",
  "An argument violates the function's contract",
  "Unit, prefix, or identifier already exists",
  "No such unit exists",
  "Operating-system error.  See \"errno\".",
  "The units belong to different unit-systems",
  "The operation on the unit(s) is meaningless",
  "The unit-system doesn't have a unit named \"second\"",
  "An error occurred while visiting a unit",
  "A unit can't be formatted in the desired manner",
  "string unit representation contains syntax error",
  "string unit representation contains unknown word",
  "Can't open argument-specified unit database",
  "Can't open environment-specified unit database",
  "Can't open installed, default, unit database",
  "Error parsing unit specification"
};

void handle_error(const char *calling_function) {
  ut_status stat;
  stat = ut_get_status();
  error("Error in function %s: %s", calling_function, ut_status_strings[stat]);
}

void r_error_fn(const char* fmt, va_list args) { // #nocov start
	Rvprintf(fmt, args);
} // #nocov end
