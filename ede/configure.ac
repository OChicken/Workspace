# Automatically Generated/Maintained  by EDE.
#
# YOU MAY MODIFY THIS FILE
# Hand made changes in some sections will be preserved
# by EDE when this file is updated.
#
# EDE is the Emacs Development Environment.
# https://cedet.sourceforge.net/ede.shtml
#
# Process this file with autoconf to produce a configure script

AC_INIT([make-demo], [1.0])

AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_LIBOBJ_DIR([lib])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE

######################
##  Basic checks.  ### (we need some results later on (e.g. $GCC)
######################

AC_PROG_MAKE_SET
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
# AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_PROG_CC
AC_PROG_CPP
AM_PROG_CC_C_O
AM_PROG_AS
AC_SEARCH_LIBS([strerror],[cposix])
AC_PROG_INSTALL
AC_PROG_AWK

LT_INIT

# And create the files.
AC_CONFIG_FILES([
Makefile
src/Makefile
tests/Makefile
])

# End the configure script.
AC_OUTPUT
