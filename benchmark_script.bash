#!/bin/bash

mk_tmp_file="mktemp"

tmp_time_file=`${mk_tmp_file}`
#echo "tmp file ${tmp_time_file}"

format_description="SystemTime:%S\nUserTime:%U\nAverageTotalMemoryUse:%K\nMaximumResidentSetSize:%M\nAverageResidentSetSize:%t\nAverageUnsharedStackSize:%p"
time_cmd="/usr/bin/time -o ${tmp_time_file} --format=${format_description}"

benchmark_choice="wordpress"
option_wordpress_require="99"
option_mysql_require="99"
option_webservers="3"

exec_stats="BenchmarkChoice:${benchmark_choice}\nOptionWordpressRequire:${option_wordpress_require}\nOptionMysqlRequire:${option_mysql_require}\nOptionWebservers:${option_webservers}"

tmp_statistics_file=`${mk_tmp_file}`
echo "tmp file ${tmp_statistics_file}"

echo -e "> Benchmark:\n${exec_stats}"
echo -e "> Running..."
${time_cmd} ./zephyrus.native -benchmark ${benchmark_choice} -benchmark-option wordpress_require ${option_wordpress_require} -benchmark-option mysql_require ${option_mysql_require} -benchmark-option webservers ${option_webservers} -out statistics "${tmp_statistics_file}" > /dev/null 2> /dev/null
echo -e "< Done!"

time_stats=`cat ${tmp_time_file}`
zephyrus_stats=`cat ${tmp_statistics_file}`
rm ${tmp_time_file} ${tmp_statistics_file}

echo -e "\nStats:\n${exec_stats}\n${zephyrus_stats}\n${time_stats}"