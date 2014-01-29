#!/bin/bash

first_file=$1
cat ${first_file} | cut -d ':' -f 1 | tr "\n" "," | sed 's/\,$//'

for file in $@; do

	#echo "${file}"
    cat ${file} | cut -d ':' -f 2 | tr "\n" "," | sed 's/\,$//'
    echo ""

done