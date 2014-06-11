#!/bin/bash

help() { 
  echo ""
  echo "usage: $0 original_file"
  echo ""
  echo "Creates a patch containing a difference between the original file and the patched file."
  echo "Original file is the one supplied as the argument."
  echo "Patched file must exist and its name must be exactly the name of original file with a \"_patched\" suffix."
  echo "Example: if your original file is \"foo.ml\" then your patched file must be \"foo.ml_patched\"."
  echo ""
}

if [ $# -eq 0 ]
  then
    echo "No arguments supplied!"
    help
    exit 1
fi

if [[ "$1" -eq "-help" ]]; then
  help
  exit 1
fi

original_file="$1"

echo "Creating a patch for file $original_file..."

if [ -f $original_file ]
  then
    echo "+ The original file \"$original_file\" exists."
  else
    echo "Error: The original file \"$original_file\" does not exist!"
    exit 1
fi

patched_file="${original_file}_patched"

if [ -f $patched_file ]
  then
    echo "+ The patched file \"$patched_file\" exists."
  else
    echo "Error: The patched file \"$patched_file\" does not exist!"
    exit 1
fi

patch_file="${original_file}_patch"

echo "+ Creating a patch file \"$patch_file\"..."

echo "diff -u $original_file $patched_file > $patch_file"
diff -u $original_file $patched_file > $patch_file

echo "+ Patch file \"$patch_file\" created successfuly!"
