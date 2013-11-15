#!/bin/bash

results=`ls -F | grep "/" | cut -d'/' -f 1`
#echo "$results"

for result in $results
do
	./compute-single-result-details.bash $result
done