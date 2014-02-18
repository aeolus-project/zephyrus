#!/bin/bash

echoerr() { echo "$@" >&2; }

echoerr "Aggregate CSV script running..."

number_of_id_fields=$1

# Read from file
#file=$1
#descr_line=`head -n 1 ${file}`
#data_lines=`tail -n +2 ${file}`

# First line is the data description line
read -e descr_line

# Other lines are the data
echoerr "Reading data..."
declare -a data_lines_array
while read -e data_line; do
  data_lines_array+="$data_line "
done
data_lines=`echo ${data_lines_array[*]} | tr " " "\n"`
echoerr "Data read!"

echoerr "Description:"; echoerr "${descr_line}"
#echo "Data:"; echo "${data_lines}"

# Find the number of fields
fields_number=`echo "${descr_line}" | tr "," " " | wc -w`
echoerr "Number of fields: ${fields_number}"

# Find the first not-id field number
first_not_prefix_field=`echo "$(($number_of_id_fields + 1))"`
#echo "First not prefix field number: ${first_not_prefix_field}"

# Prepare the not-id fields
prefix_fields=`echo "${descr_line}" | cut -d "," -f -${number_of_id_fields}`
echoerr "Prefix fields: ${prefix_fields}"
not_prefix_fields=`echo "${descr_line}" | cut -d "," -f ${first_not_prefix_field}-`
echoerr "Not prefix fields: ${not_prefix_fields}"

# Find the unique prefixes
unique_id_prefixes=`echo "${data_lines}" | cut -d "," -f -${number_of_id_fields} | sort -u -t ','`
#echo "Unique id prefixes:"; echo "${unique_id_prefixes}"
unique_id_prefixes_count=`echo "${unique_id_prefixes}" | wc -w`
echoerr "Unique id prefixes count: ${unique_id_prefixes_count}"

output_data_desctiption_line=""
declare -a output_data_lines_array

# For each unique prefix
i=0
for unique_prefix in ${unique_id_prefixes}; do
    i=`echo "$(($i + 1))"`
	echoerr "Unique prefix (${i} / ${unique_id_prefixes_count}): ${unique_prefix}"
	#echo "Prefixed lines:"; echo "${data_lines}" | grep ${unique_prefix}
    
    cut_data_lines=`echo "${data_lines}" | grep ${unique_prefix} | cut -d "," -f ${first_not_prefix_field}-`
    #echo "Cut lines with this prefix:"; echo "${cut_data_lines}"

    cut_csv=`echo -e "${not_prefix_fields}\n${cut_data_lines}"`
	#echo -e "Cut CSV:\n---->"; echo "${cut_csv}"; echo "---->"

	aggregated_csv=`echo "${cut_csv}" | ./aggregate-csv-all.bash`
	#echo -e "Aggregated CSV:\n---->"; echo "${aggregated_csv}"; echo "---->"
    
    aggregated_csv_data_description=`echo -e "${aggregated_csv}" | head -n 1`
    aggregated_csv_data=`echo -e "${aggregated_csv}" | tail -n 1`

    output_data_desctiption_line=`echo ${prefix_fields},${aggregated_csv_data_description}`
    output_data_lines_array+=`echo "${unique_prefix},${aggregated_csv_data} "`

done

#echo "OUTPUT:"
echo "${output_data_desctiption_line}"
for line in ${output_data_lines_array}; do
	echo $line
done