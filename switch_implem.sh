#!/bin/sh

if [ -e "_tags_new_approach" ]
then
  mv "_tags" "_tags_old"
  mv "_tags_new_approach" "_tags"

else 
  if [ -e "_tags_old" ]
  then
    mv "_tags" "_tags_new_approach"
    mv "_tags_old" "_tags"
  fi
fi
