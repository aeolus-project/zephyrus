#!/bin/bash

# Read from file
#file=$1
#descr_line=`head -n 1 ${file}`
#data_lines=`tail -n +2 ${file}`

# First line is the data description line
read -e descr_line

# Other lines are the data
declare -a data_lines_array
while read -e data_line; do
  data_lines_array+="$data_line "
done
data_lines=`echo ${data_lines_array[*]} | tr " " "\n"`

#echo "Description:"; echo "${descr_line}"
#echo "Data:"; echo "${data_lines}"

# Find the number of fields
fields_number=`echo "${descr_line}" | tr "," " " | wc -w`
#echo "Number of fields: ${fields_number}" 

# Initialize arrays for stocking output
declare -a output_data_desctiption_line_array
declare -a output_data_line_array

# For each field
for field_i in `seq ${fields_number}`; do
	
	# Extract the name of the field
	field_name=`echo "${descr_line}" | cut -d "," -f ${field_i}`
	#echo -e "\nField ${field_i}: ${field_name}" 

	# Extract the values of the field
	values=`echo "${data_lines}" | cut -d "," -f ${field_i}`
	spaced_values=`echo "${values}" | tr '\n' ' '`
    #echo "Values: ${spaced_values}"

    # Aggregate the values
    aggregated=`echo "$spaced_values" | awk '{ sum=0; sumsq=0; min=$1; max=$1;
       for(i=1; i<=NF;i++) {sum+=$i; sumsq+=$i*$i; if($i < min) min=$i; if($i > max) max=$i }
       printf("min=%f,max=%f,sumsq=%f,sum=%f,mean=%f,stddev=%f",  min, max, sumsq, sum, sum/NF, sqrt((sumsq-(NF*((sum/NF))*(sum/NF)))/NF)) }'`
    #echo "Aggregated values: ${aggregated}"

    # Extract each aggregation
       min=`echo "$aggregated" | cut -d "," -f 1 | cut -d "=" -f 2`
       max=`echo "$aggregated" | cut -d "," -f 2 | cut -d "=" -f 2`
    #sumsq=`echo "$aggregated" | cut -d "," -f 3 | cut -d "=" -f 2`
    #  sum=`echo "$aggregated" | cut -d "," -f 4 | cut -d "=" -f 2`
      mean=`echo "$aggregated" | cut -d "," -f 5 | cut -d "=" -f 2`
    stddev=`echo "$aggregated" | cut -d "," -f 6 | cut -d "=" -f 2`

    # Add the new field description to the output data description line
    new_field_names="min(${field_name}),max(${field_name}),mean(${field_name}),stddev(${field_name})"
    output_data_desctiption_line_array[$field_i]="${new_field_names}"
    #echo "${new_field_names}"

    # Add the new field values to the output data line
    new_values="${min},${max},${mean},${stddev}"
    output_data_line_array[$field_i]="${new_values}"
    #echo "${new_values}"

done

# Prepare the output lines
output_first_line=`echo "${output_data_desctiption_line_array[*]}" | tr " " ","`
output_second_line=`echo "${output_data_line_array[*]}" | tr " " ","`

#echo "Output:"
echo "${output_first_line}"
echo "${output_second_line}"