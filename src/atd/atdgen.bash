#!/bin/bash
echo -e "\nGenerating OCaml sources from ATD files..."
atd_files=`ls *.atd`

for atd_file in $atd_files
do
  name=`echo "$atd_file" | cut -d'.' -f 1`
  echo -e "\n=> File $atd_file :"
  
  echo "> generating ${name}_t.ml* files..."
  if atdgen -t $atd_file
  then echo "> ok!"; else echo ""; exit 1; fi

  echo "> generating ${name}_j.ml* files..."
  if atdgen -j -j-defaults $atd_file
  then echo "> ok!"; else echo ""; exit 1; fi
  
  echo "> generating ${name}_v.ml* files..."
  if atdgen -v $atd_file
  then echo "> ok!"; else echo ""; exit 1; fi

done

echo -e "\nAll OCaml sources successfully generated!\n"