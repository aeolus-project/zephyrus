#!/bin/bash

input="$@"
#echo "Input: \"${input}\""

space_replacement="%SPACE%"

input_no_spaces=`echo "$input" | sed -E "s/\{([^,}]+)\}/\1/g" | sed "s/ /${space_replacement}/g"`
eval echo $input_no_spaces | sed "s/ /\n/g" | sed "s/${space_replacement}/ /g"
