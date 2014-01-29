#!/bin/bash

file=$1

descr_line=`head -n 1 ${file}`
fields_number=`echo "${descr_line}" | tr "," " " | wc -w`
echo "Number of fields: ${fields_number}" 

data_lines=`tail -n +2 ${file}`
echo "${data_lines}"

output=""

for field_i in `seq ${fields_number}`; do
	
	field_name=`echo "${descr_line}" | cut -d "," -f ${field_i}`
	echo -e "\nField ${field_i}: ${field_name}" 

	values=`echo "${data_lines}" | cut -d "," -f ${field_i}`
	spaced_values=`echo "${values}" | tr '\n' ' '`
    echo "Values: ${spaced_values}"

    aggregated=`echo "$spaced_values" | awk '{ sum=0; sumsq=0; min=$1; max=$1;
       for(i=1; i<=NF;i++) {sum+=$i; sumsq+=$i*$i; if($i < min) min=$i; if($i > max) max=$i }
       printf("sumsq=%f sum=%f mean=%f stddev=%f min=%f max=%f\n", sumsq, sum, sum/NF, sqrt((sumsq-(NF*((sum/NF))*(sum/NF)))/NF), min, max) }'`
    echo "Aggregated: ${aggregated}"

    #sumsq=`echo "$aggregated" | cut -d " " -f 1 | cut -d "=" -f 2`
    #  sum=`echo "$aggregated" | cut -d " " -f 2 | cut -d "=" -f 2`
      mean=`echo "$aggregated" | cut -d " " -f 3 | cut -d "=" -f 2`
    stddev=`echo "$aggregated" | cut -d " " -f 4 | cut -d "=" -f 2`
    #  min=`echo "$aggregated" | cut -d " " -f 5 | cut -d "=" -f 2`
    #  max=`echo "$aggregated" | cut -d " " -f 6 | cut -d "=" -f 2`

    new_field_names="mean(${field_name}),stddev(${field_name})"
    new_values="${mean},${stddev}"

    

done