#!/bin/bash
for file in $@; do

	#echo "${file}"
	ipython $file --pylab

done
