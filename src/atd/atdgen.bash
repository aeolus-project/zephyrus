#!/bin/bash
atdfiles=`ls *.atd | cut -d '.' -f 1`

for file in $atdfiles; 
do
  echo "atdgen: $file"
  atdgen -t             $file.atd
  atdgen -j -j-defaults $file.atd
done