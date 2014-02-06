#!/bin/bash

declare -a parts=()

part_number=0
while (( "$#" )); do

	#echo "RESULT:"
	#for i in ${result}; do echo "$i"; done

	current_file=$1
	#echo "current file: ${current_file}"
	
	let part_number++
	
	if [[ `cat ${current_file} | wc -w` -eq 1 ]]; then
		parts[$part_number]="`cat ${current_file}`"
	else
		parts[$part_number]="{`cat ${current_file} | tr "\n" ","`}"
	fi
	
	shift

done

#echo "PARTS:"
#for i in `seq ${part_number}`; do echo "${parts[$i]}"; done

#SAVE_IFS=$IFS
IFS=',' parts_together="${parts[*]}"
#IFS=$SAVE_IFS
#echo "PARTS: $parts_together"

./cartesian.bash "$parts_together"