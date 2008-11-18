# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(kolores,02.1,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/kolores.f])

# Checks for libraries
AC_CHECK_LIB([cfitsio], main,,AC_MSG_ERROR(cfitsio library not found))
STTL_LIB_PGPLOT
LIBS="$LIBS $PGPLOT_LIBS"

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
                 m4/Makefile
                 atmosphere/Makefile
                 seds/Makefile
                 res/Makefile])
AC_OUTPUT
