#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.65)

AC_INIT([SQLiteMap],[0.5],[solymosi.norbert@gmail.com])

AC_CONFIG_SRCDIR(src/spatialite.c)

AC_PROG_CC

AC_TYPE_SIZE_T
AC_FUNC_MALLOC
AC_FUNC_REALLOC

AC_CHECK_HEADERS([stdlib.h stdio.h string.h memory.h math.h float.h fcntl.h inttypes.h stddef.h stdint.h sys/time.h unistd.h locale.h limits.h langinfo.h])

AC_CHECK_HEADERS(proj_api.h, [], AC_MSG_ERROR(PROJ4 header proj_api.h not found))
AC_CHECK_HEADERS(geos_c.h, [], AC_MSG_ERROR(GEOS header geos_c.h not found))


AC_CHECK_FUNCS([floor memset nl_langinfo pow setlocale sqrt strcasecmp strerror strncasecmp strstr])

AC_CHECK_LIB(proj, pj_transform, [], [have_proj=FALSE])
if test "${have_proj}" = FALSE; then
   AC_MSG_ERROR([PROJ4 library not found])
fi

AC_CHECK_LIB(geos_c, GEOSversion, [], [have_geos=FALSE])
if test "${have_geos}" = FALSE; then
   AC_MSG_ERROR([GEOS library not found])
fi

: ${R_HOME='R RHOME'}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi
CC='${R_HOME}/bin/R CMD config CC'
CFLAGS='${R_HOME}/bin/R CMD config CFLAGS'

AC_ARG_WITH([proj-include],
    AS_HELP_STRING([--with-proj-include=DIR],[location of PROJ4 header files])
    [proj_include_path="$withval"])
if test [ -n "$proj_include_path" ] ; then
    AC_SUBST([CPPFLAGS], ["-I${proj_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([proj-lib],
    AS_HELP_STRING([--with-proj-lib=DIR],[location of PROJ4 header libraries])
    [proj_lib_path="$withval"])
if test [ -n "$proj_lib_path" ] ; then
    AC_SUBST([LIBS], ["-L${proj_lib_path} ${LIBS}"])
fi


AC_ARG_WITH([geos-include],
    AS_HELP_STRING([--with-geos-include=DIR],[location of GEOS header files])
    [geos_include_path="$withval"])
if test [ -n "$geos_include_path" ] ; then
    AC_SUBST([CPPFLAGS], ["-I${geos_include_path} ${CPPFLAGS}"])
fi

AC_ARG_WITH([geos-lib],
    AS_HELP_STRING([--with-geos-lib=DIR],[location of GEOS header libraries])
    [geos_lib_path="$withval"])
if test [ -n "$geos_lib_path" ] ; then
    AC_SUBST([LIBS], ["-L${geos_lib_path} ${LIBS}"])
fi

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT


