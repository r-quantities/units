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

AC_SUBST([CXX_STD])
AC_SUBST([LIBS])
AC_SUBST([UD_CPPFLAGS])
AC_SUBST([UDUNITS2_DIR])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
