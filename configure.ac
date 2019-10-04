# Process this file with autoconf to produce a configure script.
#

# Prelude
AC_INIT(kolores,2.4,[cardiel@ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/kolores.f])

# Define directories to search for libraries
AC_MSG_CHECKING([uname -s for detecting host operating system])
OSNAME=`uname -s`
case "$OSNAME" in
  Darwin*)
    AC_MSG_RESULT(Darwin)
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
  ;;
  Linux*)
    AC_MSG_RESULT(Linux)
    LDFLAGS="$LDFLAGS -L/usr/local/pgplot"
  ;;
esac

# Checks for libraries
AC_MSG_RESULT(LDFLAGS set to... $LDFLAGS)
AC_CHECK_LIB([X11], main,,AC_MSG_ERROR(X11 library not found))
AC_CHECK_LIB([pgplot], main,,AC_MSG_ERROR(pgplot library not found))
PKG_CHECK_MODULES([CFITSIO], [cfitsio])
# Define in LIBS all the previous libraries
LIBS="$LIBS"

# Definitions
DEFAULT_AUXDIR="/usr/local/share/kolores"
AUXDIR=${AUXDIR="$DEFAULT_AUXDIR"}
AC_ARG_VAR(AUXDIR,[Directory to install auxiliary kolores files])

DEFAULT_NXMAX="4800"
NXMAX=${NXMAX="$DEFAULT_NXMAX"}
AC_ARG_VAR(NXMAX,[Maximum image size in the X direction])

DEFAULT_NYMAX="1100"
NYMAX=${NYMAX="$DEFAULT_NYMAX"}
AC_ARG_VAR(NYMAX,[Maximum image size in the Y direction])

DEFAULT_NPMAX="20000"
NPMAX=${NPMAX="$DEFAULT_NPMAX"}
AC_ARG_VAR(NPMAX,[Maximum number of points in filters])

DEFAULT_NSIMULMAX="500"
NSIMULMAX=${NSIMULMAX="$DEFAULT_NSIMULMAX"}
AC_ARG_VAR(NSIMULMAX,[Maximum number of numerical simulations])

# Output files
AC_CONFIG_FILES([Makefile
                 src/button.inc
                 src/photometry.inc
                 src/version.inc
                 src/Makefile
                 atmosphere/Makefile
                 seds/Makefile
                 res/Makefile])
AC_OUTPUT
