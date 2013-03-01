#!/bin/bash

# Set names.
DIST_SOURCE_NAME="zephyrus-1.0"
DIST_TESTS_NAME="zephyrus-1.0-tests"

# Clean up just in case...
rm -f $DIST_SOURCE_NAME.tar.gz
rm -f $DIST_TESTS_NAME.tar.gz

rm -rf $DIST_SOURCE_NAME/
rm -rf $DIST_TESTS_NAME/

# Prepare file lists.
DIST_SOURCE_FILES="*.ml *.mli *.mll *.mly *.atd Makefile OCamlMakefile Atdgen.mk dist_make.bash README COPYING copyleft_statement.txt INSTALL_debian.bash coinst-1.01"
DIST_TEST_FILES="repositories tests example-inputs example-results"


# Copy source files into a new temporary dir.
mkdir $DIST_SOURCE_NAME
cp -r $DIST_SOURCE_FILES $DIST_SOURCE_NAME/


# Copy test files into a new temporary dir.
mkdir $DIST_TESTS_NAME
cp -r $DIST_TEST_FILES $DIST_TESTS_NAME/

# Create the archives.
tar -czf $DIST_SOURCE_NAME.tar.gz $DIST_SOURCE_NAME
tar -czf $DIST_TESTS_NAME.tar.gz $DIST_TESTS_NAME

# Remove the remaining temporary directory.
rm -rf $DIST_SOURCE_NAME/
rm -rf $DIST_TESTS_NAME/

