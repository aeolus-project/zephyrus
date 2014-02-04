#!/bin/bash

timeout_in_seconds="1200"

# Prepare a directory for this series of benchmarks
benchmarks_dir_name="benchmark_results/benchmarks_`date +'%F_%H:%M:%S'`"
mkdir ${benchmarks_dir_name}

format_description_1="SolvingSystemTime:%S\nSolvingUserTime:%U"
format_description_2="SystemTime:%S\nUserTime:%U\nAverageTotalMemoryUse:%K\nMaximumResidentSetSize:%M\nAverageResidentSetSize:%t\nAverageUnsharedStackSize:%p"

benchmark_choice="wordpress"

options_wordpress_require=`cat option_wordpress_require`
options_mysql_require=`cat option_mysql_require`
options_mysql_provide=`cat option_mysql_provide`
options_webservers=`cat option_webservers`
options_solver=`cat option_solver`

for option_wordpress_require in ${options_wordpress_require}; do
for option_mysql_require     in ${options_mysql_require}    ; do
for option_mysql_provide     in ${options_mysql_provide}    ; do
for option_webservers        in ${options_webservers}       ; do
for option_solver            in ${options_solver}           ; do

#option_wordpress_require="200"
#option_mysql_require="200"
#option_webservers="3"

tmp_time_file_1=`mktemp zephyrus_benchmark_time_XXXXX`
tmp_time_file_2=`mktemp zephyrus_benchmark_time_XXXXX`
#echo "tmp file ${tmp_time_file}"

time_cmd_1="/usr/bin/time -o ${tmp_time_file_1} --format=${format_description_1}"
time_cmd_2="/usr/bin/time -o ${tmp_time_file_2} --format=${format_description_2}"

exec_stats="Solver:${option_solver}\nBenchmarkChoice:${benchmark_choice}\nOptionWordpressRequire:${option_wordpress_require}\nOptionMysqlRequire:${option_mysql_require}\nOptionMysqlProvide:${option_mysql_provide}\nOptionWebservers:${option_webservers}\n"

tmp_statistics_file=`mktemp zephyrus_benchmark_stats_XXXXX`
#echo "tmp file ${tmp_statistics_file}"

echo -e "> Benchmark:\n${exec_stats}"
echo -e "> Running (without generating the final configuration)... (started on `date +'%F %H:%M:%S'`)"
${time_cmd_1} timeout ${timeout_in_seconds}s ./zephyrus.native -solver ${option_solver} -benchmark ${benchmark_choice} -benchmark-option wordpress_require ${option_wordpress_require} -benchmark-option mysql_require ${option_mysql_require} -benchmark-option mysql_provide ${option_mysql_provide} -benchmark-option webservers ${option_webservers} -stop-after-solving                      > /dev/null 2> /dev/null
echo -e "< Done!"
echo -e "> Running (with generating the final configuration)... (started on `date +'%F %H:%M:%S'`)"
${time_cmd_2} timeout ${timeout_in_seconds}s ./zephyrus.native -solver ${option_solver} -benchmark ${benchmark_choice} -benchmark-option wordpress_require ${option_wordpress_require} -benchmark-option mysql_require ${option_mysql_require} -benchmark-option mysql_provide ${option_mysql_provide} -benchmark-option webservers ${option_webservers} -out statistics "${tmp_statistics_file}" > /dev/null 2> /dev/null
echo -e "< Done!"

# Collect statistics from temporary files, delete the temporary files
if test "`head -n 1 ${tmp_time_file_2}`" = "Command exited with non-zero status 124"; then 
	time_stats_1="TIMEOUT"
	time_stats_2="TIMEOUT"
	zephyrus_stats=""
else 
    time_stats_1=`cat ${tmp_time_file_1}`
    time_stats_2=`cat ${tmp_time_file_2}`
    zephyrus_stats="`cat ${tmp_statistics_file}`\n"
fi

rm ${tmp_time_file_1}
rm ${tmp_time_file_2}
rm ${tmp_statistics_file}

echo -e "\nStats:\n${exec_stats}${zephyrus_stats}${time_stats_1}\n${time_stats_2}\n"

# Print statistics to a file.
benchmark_results_file=`mktemp --tmpdir=${benchmarks_dir_name} benchmark_XXXXXX`
#echo "benchmark_results_file : ${benchmark_results_file}"
echo -en "${exec_stats}${zephyrus_stats}${time_stats_1}\n${time_stats_2}" > ${benchmark_results_file}

done
done
done
done
done