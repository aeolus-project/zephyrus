#!/bin/bash

result=$1
#echo $result

# Prepare the solution file name and check if it exists.
file="$result/output.json"
if ! test -e $file; then exit; fi

# Count the number of locations, free locations and used locations
locations=`grep "\"repository\"" $file | wc -l`
free_locations=`grep "\"packages_installed\": \[\]" $file | wc -l`
used_locations=`echo $(($locations - $free_locations))`

# Count components
components=`grep "\"component_name\"" $file | wc -l`
unique_component_types=`grep "\"component_type\"" $file | cut -d "\"" -f 4 | sort | uniq -c`

# Show results
echo -e "$result\n- $used_locations locations used\n- $components components present:"
echo -e "$unique_component_types\n"
