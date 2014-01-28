#!/bin/bash

mk_tmp_file="mktemp"

tmp_time_file=`${mk_tmp_file}`
#echo "tmp file ${tmp_time_file}"

time_cmd="/usr/bin/time -o ${tmp_time_file} -f %S,%U"

${time_cmd} ./zephyrus.native -benchmark wordpress -benchmark-option wordpress_require 10 -benchmark-option mysql_require 10 > /dev/null 2> /dev/null

cat ${tmp_time_file}
rm ${tmp_time_file}