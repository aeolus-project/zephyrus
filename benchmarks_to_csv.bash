#!/bin/bash

first_file=$1
# Data description line
cat ${first_file} | cut -d ':' -f 1 | tr "\n" "," | sed 's/\,$//'
echo ""

# Data lines
for file in $@; do

	#echo "${file}"

	# All data lines from a file 
    cat ${file} | cut -d ':' -f 2 | tr "\n" "," | sed 's/\,$//'
    echo ""

done