#!/bin/bash

script_dir="`dirname $0`"

echoerr() { echo "$@" >&2; }

input="$@"
#echo "Input: \"${input}\""

timeout_in_seconds="1200"

# Prepare a directory for this series of benchmarks
benchmarks_dir_name="${script_dir}/results/benchmarks_`date +'%F_%H:%M:%S'`"
mkdir ${benchmarks_dir_name}

# Time formats
format_description_1="SolvingSystemTime:%S\nSolvingUserTime:%U\nSolvingWallClockTime:%E"
format_description_2="SystemTime:%S\nUserTime:%U\nWallClockTime:%E\nAverageTotalMemoryUse:%K\nMaximumResidentSetSize:%M\nAverageResidentSetSize:%t\nAverageUnsharedStackSize:%p"

# Treat parameter sets
echoerr "Preparing parameter sets for benchmark cases... (it may take some time)"
declare -a cases=()
IFS=$'\n'
cases=( `${script_dir}/smart-parameters.bash \"${input}\" | shuf` )
unset IFS
echoerr "Parameter sets ready!"

last_case_i="$((${#cases[*]} - 1))"

# Prepare the children pipe
pipe=`mktemp --tmpdir zephyrus_fifo_XXXXX`
rm -f $pipe
mkfifo $pipe

children="0"
max_children="2"

add_child () { children="$(( $children + 1 ))"; }
sub_child () { children="$(( $children - 1 ))"; }
wait_children () {
	while [[ $children -ge $max_children ]]; do
		while read line < $pipe
		do
			if [[ "$line"  == "end" ]]; then
				sub_child
				break
			fi
		done
	done
}


# Launch a benchmark for each case
for cases_i in `seq 0 ${last_case_i}`; do

	#echo "case $cases_i : ${cases[$cases_i]}"
	case="${cases[$cases_i]}"

	worker () {

		preamble="`echo "$case" | cut -d ';' -f 1 | sed "s/,/\n/g"`\n"
		#echo "preamble = $preamble"
		exec_stats="$preamble"

		zephyrus_command="`echo "$case" | cut -d ';' -f 2`"
		zephyrus_command=${zephyrus_command#\"}
		zephyrus_command=${zephyrus_command%\"}
		#echo "zephyrus_command = $zephyrus_command"


		# FIRST RUN (without generating the final configuration)

		tmp_time_file_1=`mktemp zephyrus_benchmark_time_XXXXX`
		time_cmd_1="/usr/bin/time -o ${tmp_time_file_1} --format=${format_description_1}"

		echo -e "> Benchmark (${cases_i}/${last_case_i}):\n${exec_stats}"
		echo -e "> Running (without generating the final configuration)... (started on `date +'%F %H:%M:%S'`)"
		cmd="${time_cmd_1} timeout ${timeout_in_seconds}s ${zephyrus_command} -stop-after-solving"
		#echo "$cmd"
		$cmd > /dev/null 2> /dev/null

		echo -e "> Benchmark (${cases_i}/${last_case_i}):\n${exec_stats}"
		echo -e "< Done first run!"


		# SECOND RUN (with generating the final configuration)

		tmp_time_file_2=`mktemp zephyrus_benchmark_time_XXXXX`
		time_cmd_2="/usr/bin/time -o ${tmp_time_file_2} --format=${format_description_2}"
		tmp_statistics_file=`mktemp zephyrus_benchmark_stats_XXXXX`

		echo -e "> Running (with generating the final configuration)... (started on `date +'%F %H:%M:%S'`)"
		cmd="${time_cmd_2} timeout ${timeout_in_seconds}s ${zephyrus_command} -out statistics ${tmp_statistics_file}"
		#echo "$cmd"
		$cmd > /dev/null 2> /dev/null
		echo -e "> Benchmark (${cases_i}/${last_case_i}):\n${exec_stats}"
		echo -e "< Done second run!"

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
	
		echo "end" > $pipe
	}

	worker &

	add_child
	wait_children

done

max_children=1
wait_children

rm -f $pipe
