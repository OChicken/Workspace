#!/bin/bash

# The script used to clean the files generated by GNU make

# rm Makefile
if [ -f Makefile ]; then
    make distclean
fi
find ./* -type d -exec rm -rf {}/Makefile {}/Makefile.in \;
rm -f Makefile.in

# rm m4
rm -rf m4/

# rm build
rm -rf build/

# rm build-aux
rm -rf build-aux/

# rm executable files
find . -type f -executable ! \( -name "make-clean.sh" \) -exec rm -f {} \;

# rm .deps
find . -type d -exec rm -rf {}/.deps/ \;

# rm files generated by auto tools
rm -rf autoscan.log
rm -rf aclocal.m4
rm -rf autom4te.cache/
rm -rf config.h config.h.in config.log
rm -rf ar-lib
rm -rf stamp-*
rm -rf ltmain.sh
rm -rf COPYING INSTALL
