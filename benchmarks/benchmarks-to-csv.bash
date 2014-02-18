#!/bin/bash

if `test $1 != "-append"`; then
  first_file=$1
  # Data description line
  cat ${first_file} | cut -d ':' -f 1 | tr "\n" "," | sed 's/\,$//'
  echo ""
else
  shift
fi

# Data lines
for file in $@; do

	#echo "${file}"

	# All data lines from a file 
    cat ${file} | cut -d ':' -f 2 | tr "\n" "," | sed 's/\,$//'
    echo ""

done