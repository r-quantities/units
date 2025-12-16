define([pkgversion], esyscmd([sh -c "grep Version: DESCRIPTION | cut -d' ' -f2 | tr -d '\n'"]))
AC_INIT(units, [pkgversion], edzer.pebesma@uni-muenster.de)
AC_MSG_NOTICE([${PACKAGE_NAME}: ${PACKAGE_VERSION}])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`"${R_HOME}/bin/R" CMD config LDFLAGS`
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
AC_LANG(C++)

STD_VER=`$CXX -dM -E -x c++ /dev/null | grep -F __cplusplus | sed 's/.* //' | sed 's/L//'`
printf %s "checking for default C++ standard... $STD_VER" >&6
if test [ $STD_VER -lt 201703 ] ; then
    printf %s ", setting 201703" >&6
    CXX_STD="CXX_STD = CXX17"
fi; echo ""

AC_CHECK_HEADER_STDBOOL
AC_FUNC_ERROR_AT_LINE
AC_PREREQ
AC_PROG_CC

AC_PATH_PROG([BREW], [brew])
if test [ -n "${BREW}" ] ; then
    UD_CPPFLAGS="-I`${BREW} --prefix`/include"
    LIBS="-L`${BREW} --prefix`/lib ${LIBS}"
fi

AC_ARG_WITH([udunits2-include],
    AS_HELP_STRING([--with-udunits2-include=DIR],
		[location of the udunits2 header files]),
		[udunits2_include_path=$withval])
if test [ -n "$udunits2_include_path" ] ; then
    UD_CPPFLAGS="-I${udunits2_include_path} ${UD_CPPFLAGS}"
elif test [ -n "${UDUNITS2_INCLUDE}" ] ; then
    UD_CPPFLAGS="-I${UDUNITS2_INCLUDE} ${UD_CPPFLAGS}"
fi
CPPFLAGS="${UD_CPPFLAGS} ${CPPFLAGS}"

AC_ARG_WITH([udunits2-lib],
    AS_HELP_STRING([--with-udunits2-lib=DIR],
		[location of the udunits2 libraries]),
		[udunits2_lib_path=$withval])
if test [ -n "$udunits2_lib_path" ] ; then
    LIBS="-L${udunits2_lib_path} ${LIBS}"
elif test [ -n "${UDUNITS2_LIBS}" ] ; then
    LIBS="-L${UDUNITS2_LIBS} ${LIBS}"
fi

AC_CHECK_LIB(expat, XML_ParserCreate, [],[], ${LIBS})
if test "${ac_cv_lib_expat_XML_ParserCreate}" = yes; then
   LIBS="${LIBS} -lexpat"
fi

AC_CHECK_HEADER(udunits2.h, UDUNITS2_DIR=0, [
  AC_CHECK_HEADER(udunits2/udunits2.h, UDUNITS2_DIR=1,
                  UD_ERROR="udunits2.h was not found") ])
AC_CHECK_LIB(udunits2, ut_read_xml,
             LIBS="${LIBS} -ludunits2",
             UD_ERROR="libudunits2.so was not found")

if test "${UD_ERROR}" != ""  ; then AC_MSG_FAILURE([
--------------------------------------------------------------------------------
  Configuration failed because ${UD_ERROR}. Try installing:
    * deb: libudunits2-dev (Debian, Ubuntu, ...)
    * rpm: udunits2-devel (Fedora, EPEL, ...)
    * brew: udunits (OSX)
  If udunits2 is already installed in a non-standard location, use:
    --configure-args='--with-udunits2-lib=/usr/local/lib'
  if the library was not found, and/or:
    --configure-args='--with-udunits2-include=/usr/include/udunits2'
  if the header was not found, replacing paths with appropriate values.
  You can alternatively set UDUNITS2_INCLUDE and UDUNITS2_LIBS manually.
--------------------------------------------------------------------------------
]) fi

# Only run this check if we successfully found the library so far
if test -z "${UD_ERROR}"; then
  AC_MSG_CHECKING([whether udunits2 accepts exceptions])
  AC_RUN_IFELSE([AC_LANG_PROGRAM(
    [[
      #include <stdexcept>
      #include <cstdarg>
      #ifdef HAVE_UDUNITS2_UDUNITS2_H
      # include <udunits2/udunits2.h>
      #else
      # include <udunits2.h>
      #endif
      
      // A custom handler that throws a C++ exception
      int my_handler(const char* fmt, va_list args) {
        throw std::runtime_error("udunits error");
        return 0;
      }
    ]],
    [[
      // 1. Set the handler
      ut_set_error_message_handler(my_handler);
      
      try {
        // 2. Trigger an operation that uses the library.
        // ut_read_xml(NULL) loads the default database.
        ut_system* system = ut_read_xml(NULL);

        if (system) {
            // 3. Force an error by parsing nonsense
            ut_parse(system, "ThisIsNotAUnit", UT_ASCII);
        } else {
            // If system is NULL, ut_read_xml failed. 
            // If it failed, it should have called our handler and thrown.
            // If we are here, it failed silently or handler didn't throw.
            return 1;
        }
      } catch (const std::runtime_error& e) {
        return 0; // Success: Exception caught!
      } catch (...) {
        return 0; // Success: Some exception caught!
      }
      
      // Failure: No exception was caught
      return 1;
    ]])],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     AC_MSG_ERROR([The linked udunits2 library does not support exception propagation. Please reinstall udunits2 with -fexceptions enabled.])],
    [AC_MSG_RESULT([unknown (cross-compiling)])
     AC_MSG_WARN([Cross-compiling: cannot verify exception propagation in udunits2. Assuming it works.])]
  )
fi

AC_SUBST([CXX_STD])
AC_SUBST([LIBS])
AC_SUBST([UD_CPPFLAGS])
AC_SUBST([UDUNITS2_DIR])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
