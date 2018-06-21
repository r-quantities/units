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

AC_CHECK_HEADER_STDBOOL
AC_FUNC_ERROR_AT_LINE
AC_PREREQ
AC_PROG_CC

AC_ARG_WITH([udunits2-include],
    AS_HELP_STRING([--with-udunits2-include=DIR],
		[location of the udunits2 header files]),
		[udunits2_include_path=$withval])
if test [ -n "$udunits2_include_path" ] ; then
   UD_CPPFLAGS="-I${udunits2_include_path}"
else
   if test [ -n "${UDUNITS2_INCLUDE}" ] ; then
      UD_CPPFLAGS="-I${UDUNITS2_INCLUDE}"
   fi
fi

AC_ARG_WITH([udunits2-lib],
    AS_HELP_STRING([--with-udunits2-lib=DIR],
		[location of the udunits2 libraries]),
		[udunits2_lib_path=$withval])
if test [ -n "$udunits2_lib_path" ] ; then
   LIBS="-L${udunits2_lib_path} ${LIBS}"
else
   if test [ -n "${UDUNITS2_LIBS}" ] ; then
      LIBS="-L${UDUNITS2_LIBS} ${LIBS}"
   fi
fi

AC_CHECK_LIB(expat, XML_ParserCreate, [],[], ${LIBS})
if test "${ac_cv_lib_expat_XML_ParserCreate}" == yes; then
   LIBS="${LIBS} -lexpat"
fi

CPPFLAGS="${UD_CPPFLAGS} ${CPPFLAGS}"

AC_CHECK_HEADER(udunits2.h, [], [
AC_CHECK_HEADER(udunits2/udunits2.h, 
                UD_CPPFLAGS="${UD_CPPFLAGS} -I/usr/include/udunits2", 
                UD_ERROR="udunits2.h not found") ])

AC_CHECK_LIB(udunits2, ut_read_xml, 
             LIBS="${LIBS} -ludunits2",
             UD_ERROR="libudunits2.so not found")

if test "${UD_ERROR}" != ""  ; then AC_MSG_FAILURE([
--------------------------------------------------------------------------------
  ${UD_ERROR}!
  
  If the udunits2 library is installed in a non-standard location, use:
  
    --configure-args='--with-udunits2-lib=/usr/local/lib'
    
  for example, if the library was not found, and/or
  
    --configure-args='--with-udunits2-include=/usr/include/udunits2'
    
  if the header was not found, replacing paths with appropriate values for your
  installation. You can alternatively use the UDUNITS2_INCLUDE and UDUNITS2_LIBS
  environment variables.
  
  If udunits2 is not installed, please install it.
  It is required for this package.
--------------------------------------------------------------------------------
]) fi

UD_CPPFLAGS="${UD_CPPFLAGS} ${CPPFLAGS}"
AC_SUBST([LIBS])
AC_SUBST([UD_CPPFLAGS])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
