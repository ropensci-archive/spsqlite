#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([SQLiteMap],[0.62],[solymosi.norbert@gmail.com]) 
# fundamentally sourced from work of Alessandro Furieri (a.furieri@lqt.it)

AC_CONFIG_SRCDIR(src/spatialite.c)
#AC_CONFIG_HEADERS(src/config.h)

# Checks for programs.
AC_PROG_CPP
AC_PROG_CC

# Checks for header files.
#AC_CHECK_HEADERS([fcntl.h float.h inttypes.h langinfo.h limits.h locale.h memory.h stddef.h stdint.h stdlib.h string.h sys/file.h sys/ioctl.h sys/mount.h sys/param.h sys/time.h unistd.h utime.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_MEMBERS([struct stat.st_blksize])
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_REALLOC
#AC_FUNC_STRERROR_R
# AC_CHECK_FUNCS([clock_gettime fdatasync floor gettimeofday localtime_r memmove memset mkdir munmap nl_langinfo pow setlocale sqrt strcasecmp strchr strncasecmp strstr utime])



#-----------------------------------------------------------------------
#   --enable-mathsql
#
AC_ARG_ENABLE(mathsql, [AS_HELP_STRING(
  [--enable-mathsql], [enables SQL math functions [default=yes]])],
  [], [mathsql=yes])
if test x"$enable_mathsql" != "xno"; then
  OMIT_MATHSQL_FLAGS=
else
  OMIT_MATHSQL_FLAGS=-DOMIT_MATHSQL
fi
AC_SUBST(OMIT_MATHSQL_FLAGS)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#   --enable-geocallbacks
#
AC_ARG_ENABLE(geocallbacks, [AS_HELP_STRING(
  [--enable-geocallbacks], [enables geometry callbacks [default=yes]])],
  [], [geocallbacks=yes])
if test x"$enable_geocallbacks" != "xno"; then
  OMIT_GEOCALLBACKS_FLAGS=
else
  OMIT_GEOCALLBACKS_FLAGS=-DOMIT_GEOCALLBACKS
fi
AC_SUBST(OMIT_GEOCALLBACKS_FLAGS)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#   --enable-proj
#
AC_ARG_ENABLE(proj, [AS_HELP_STRING(
  [--enable-proj], [enables PROJ.4 inclusion [default=yes]])],
  [], [proj=yes])
if test x"$enable_proj" != "xno"; then
  OMIT_PROJ_FLAGS=
  AC_CHECK_HEADERS(proj_api.h,, [AC_MSG_ERROR([cannot find proj_api.h, bailing out])])
  AC_SEARCH_LIBS(pj_init_plus,proj,,AC_MSG_ERROR(['libproj' is required but it doesn't seem to be installed on this system.]),-lm)
else
  OMIT_PROJ_FLAGS=-DOMIT_PROJ
fi
AC_SUBST(OMIT_PROJ_FLAGS)
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#   --enable-geos
#
AC_ARG_ENABLE(geos, [AS_HELP_STRING(
  [--enable-geos], [enables GEOS inclusion [default=yes]])],
  [], [geos=yes])
if test x"$enable_geos" != "xno"; then
  OMIT_GEOS_FLAGS=
  AC_CHECK_HEADERS(geos_c.h,, [AC_MSG_ERROR([cannot find geos_c.h, bailing out])])
  AC_SEARCH_LIBS(GEOSTopologyPreserveSimplify,geos_c,,AC_MSG_ERROR(['libgeos_c' is required but it doesn't seem to be installed on this system.]),-lm -lgeos)
  #-----------------------------------------------------------------------
  #   --enable-geosadvanced
  #
  AC_ARG_ENABLE(geosadvanced, [AS_HELP_STRING(
	  [--enable-geosadvanced], [enables GEOS advanced features [default=no]])],
	  [], [enable_geosadvanced=no])
  if test x"$enable_geosadvanced" != "xno"; then
	  GEOSADVANCED_FLAGS=-DGEOS_ADVANCED
	  AC_SEARCH_LIBS(GEOSCoveredBy,geos_c,,AC_MSG_ERROR([obsolete 'libgeos_c' (< v.3.3.0). please retry specifying: --disable-geosadvanced.]),-lm -lgeos)
  else
	  GEOSADVANCED_FLAGS=
  fi
	  AC_SUBST(GEOSADVANCED_FLAGS)
else
  OMIT_GEOS_FLAGS=-DOMIT_GEOS
fi
AC_SUBST(OMIT_GEOS_FLAGS)

#-----------------------------------------------------------------------
#   --enable-epsg
#
AC_ARG_ENABLE(epsg, [AS_HELP_STRING(
  [--enable-epsg], [enables full EPSG dataset support [default=yes]])],
  [], [epsg=yes])
if test x"$enable_epsg" != "xno"; then
  OMIT_EPSG_FLAGS=
else
  OMIT_EPSG_FLAGS=-DOMIT_EPSG
fi
AC_SUBST(OMIT_EPSG_FLAGS)
#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
#   --enable-iconv
#
AC_ARG_ENABLE(iconv, [AS_HELP_STRING(
  [--enable-iconv], [enables ICONV inclusion [default=no]])],
  [], [enable_iconv=no])
if test x"$enable_iconv" != "xno"; then
  OMIT_ICONV_FLAGS=
  AC_CHECK_HEADERS(iconv.h,, [AC_MSG_ERROR([cannot find iconv.h, bailing out])])
  AC_SEARCH_LIBS(iconv,iconv,, AC_SEARCH_LIBS(libiconv,iconv,,AC_MSG_ERROR(['libiconv' is required but it doesn't seem to be installed on this system.]),))
  AC_SEARCH_LIBS(locale_charset,charset,, AC_SEARCH_LIBS(nl_langinfo,c,,AC_MSG_ERROR(['libcharset' is required but it doesn't seem to be installed on this system.]),))
else
  OMIT_ICONV_FLAGS=-DOMIT_ICONV
fi
AC_SUBST(OMIT_ICONV_FLAGS)


#-----------------------------------------------------------------------
#   --enable-freexl
#
AC_ARG_ENABLE(freexl, [AS_HELP_STRING([--enable-freexl], [enables FreeXL inclusion [default=no]])], [], [enable_freexl=no])
if test x"$enable_freexl" != "xno"; then
  OMIT_FREEXL_FLAGS=
  AC_CHECK_HEADERS(freexl.h,, [AC_MSG_ERROR([cannot find freexl.h, bailing out])])
  AC_SEARCH_LIBS(freexl_open,freexl,,AC_MSG_ERROR(['libfreexl' is required but it doesn't seem to be installed on this system.]),-lm)
else
  OMIT_FREEXL_FLAGS=-DOMIT_FREEXL
fi
AC_SUBST(OMIT_FREEXL_FLAGS)
#-----------------------------------------------------------------------

: ${R_HOME='R RHOME'}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi
CC='${R_HOME}/bin/R CMD config CC'
CFLAGS='${R_HOME}/bin/R CMD config CFLAGS'


VERSION="3.0.1-stable"
AC_SUBST(VERSION)

AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
AC_CONFIG_FILES([src/Makevars])

AC_OUTPUT

