#!/bin/bash

declare -a result
result+="X"

while (( "$#" )); do

	#echo "RESULT:"
	#for i in ${result}; do echo "$i"; done

	current_file=$1
	#echo "current file: ${current_file}"
	
	declare -a new_result
	new_result=()

	for prefix in ${result}; do
	  #echo "prefix  = ${prefix}"
	  for line in `cat ${current_file}`; do
        #echo "line = ${line}"

        new_line="${prefix},${line}"
        #echo "newline = ${new_line}"

        new_result+="${new_line} "
	  done
	done
	
	result="${new_result}"
	
	shift

done

# echo "RESULT:"
for i in ${result}; do echo "$i" | cut -c 3-; done