#!/bin/bash

timeout_in_seconds="120"

# Prepare a directory for this series of benchmarks
benchmarks_dir_name="benchmark_results/benchmarks_`date +'%F_%H:%M:%S'`"
mkdir ${benchmarks_dir_name}

format_description="SystemTime:%S\nUserTime:%U\nAverageTotalMemoryUse:%K\nMaximumResidentSetSize:%M\nAverageResidentSetSize:%t\nAverageUnsharedStackSize:%p"

benchmark_choice="wordpress"

for option_wordpress_require in `cat option_wordpress_require`; do
for option_mysql_require     in `cat option_mysql_require`    ; do
for option_webservers        in `cat option_webservers`       ; do

#option_wordpress_require="200"
#option_mysql_require="200"
#option_webservers="3"

tmp_time_file=`mktemp zephyrus_benchmark_time_XXXXX`
#echo "tmp file ${tmp_time_file}"

time_cmd="/usr/bin/time -o ${tmp_time_file} --format=${format_description}"

exec_stats="BenchmarkChoice:${benchmark_choice}\nOptionWordpressRequire:${option_wordpress_require}\nOptionMysqlRequire:${option_mysql_require}\nOptionWebservers:${option_webservers}\n"

tmp_statistics_file=`mktemp zephyrus_benchmark_stats_XXXXX`
#echo "tmp file ${tmp_statistics_file}"

echo -e "> Benchmark:\n${exec_stats}"
echo -e "> Running... (started on `date +'%F %H:%M:%S'`)"
${time_cmd} timeout ${timeout_in_seconds}s ./zephyrus.native -benchmark ${benchmark_choice} -benchmark-option wordpress_require ${option_wordpress_require} -benchmark-option mysql_require ${option_mysql_require} -benchmark-option webservers ${option_webservers} -out statistics "${tmp_statistics_file}" > /dev/null 2> /dev/null
echo -e "< Done!"

# Collect statistics from temporary files, delete the temporary files
if test "`head -n 1 ${tmp_time_file}`" = "Command exited with non-zero status 124"; then 
	time_stats="TIMEOUT"
	zephyrus_stats=""
else 
    time_stats=`cat ${tmp_time_file}`
    zephyrus_stats="`cat ${tmp_statistics_file}`\n"
fi

rm ${tmp_time_file}
rm ${tmp_statistics_file}

echo -e "\nStats:\n${exec_stats}${zephyrus_stats}${time_stats}\n"

# Print statistics to a file.
benchmark_results_file=`mktemp --tmpdir=${benchmarks_dir_name} benchmark_XXXXXX`
#echo "benchmark_results_file : ${benchmark_results_file}"
echo -en "${exec_stats}${zephyrus_stats}${time_stats}" > ${benchmark_results_file}

done
done
done