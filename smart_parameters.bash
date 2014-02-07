#!/bin/bash

input="$@"
#echo "Input: \"${input}\""

declare -a input_with_replacers_array
declare -a replacers
declare -a params
declare -a options

param_number=0
for word in $input; do
	#echo "Word: $word"
	if [[ $word =~ (\[(.+):(.+)\]) ]]; then
    	let param_number++
		
		matched="\[${BASH_REMATCH[0]:1:-1}\]"
    	#echo "matched : $matched"

		local_param=${BASH_REMATCH[2]}
		params+=("${local_param}")
		#echo "local_param : $local_param"
		local_option=${BASH_REMATCH[3]}
		options+=("${local_option}")
		#echo "local_option : $local_option"
		

		replacer="%PARAM_${param_number}%"
		replacers+=("$replacer")
		#echo "replacer : $replacer"

		word_with_replacer="${word/${matched}/${replacer}}"
		#echo "word_with_replacer : $word_with_replacer"

		input_with_replacers_array+=("$word_with_replacer")
	else
		input_with_replacers_array+=("$word")
	fi
done

#echo "param_number : $param_number"

input_with_replacers="${input_with_replacers_array[@]}"
#echo "input_with_replacers = ${input_with_replacers}"

#echo "replacers = ${replacers[@]}"
#echo "params = ${params[@]}"
#echo "options = ${options[@]}"

options_for_cartesian=`echo "${options[@]}" | tr " " ","`

declare -a cases=(`./cartesian.bash "${options_for_cartesian}"`)

for cases_i in `seq 0 $((${#cases[*]} - 1))`; do
	
	#echo "case $cases_i : ${cases[$cases_i]}"

	case="${cases[$cases_i]}"
	IFS=', ' read -a options <<< "$case"
	#echo "${options[@]}"

	declare -a pre=()
	line="${input_with_replacers}"

	for params_i in `seq 0 $((${#params[@]]} - 1))`; do
		param="${params[${params_i}]}"
		value="${options[${params_i}]}"
		replacer="${replacers[${params_i}]}"
		#echo "$param = $value  ($replacer)"

		pre+=("$param:$value")
		line="${line//${replacer}/${value}}"
	done
	
	preamble=$(printf ",%s" "${pre[@]}")
    preamble=${preamble:1}
	
	echo "${preamble};${line}"

done
