#!/bin/bash

script_dir="`dirname $0`"

# Debug printing
echoerr() { echo "$@" >&2 ; }


### === CPU token manager === ###

token_manager="${script_dir}/token-manager.bash"
token_name="cpu"

tokens="8" # Number of cpu cores

# Check if the cpu token manager is running. If not, launch it.
if ! $token_manager $token_name is-running
then
  $token_manager $token_name start $tokens
fi

# Take token / yield token functions
take_token  () { $token_manager $token_name take;  }
yield_token () { $token_manager $token_name yield; }

### === CPU token manager === ###


### === Time command === ###

# Output format
time_format_description="SolvingSystemTime:%S\nSolvingUserTime:%U\nSolvingWallClockTime:%E"
#time_format_description_full="SystemTime:%S\nUserTime:%U\nWallClockTime:%E\nAverageTotalMemoryUse:%K\nMaximumResidentSetSize:%M\nAverageResidentSetSize:%t\nAverageUnsharedStackSize:%p"

make_time_command () {
  output=$1
  format=$2
  echo "/usr/bin/time -o ${output} --format=${format}"
}

### === Time command === ###


### === Date command === ###

formatted_date () {
  echo `date +'%F %H:%M:%S'`
}

### === Date command === ###


input="$@"
#echo "Input: \"${input}\""

timeout_in_seconds="36000"

# Prepare a directory for this series of benchmarks
benchmarks_dir_name="${script_dir}/results/benchmarks_`date +'%F_%H:%M:%S'`"
mkdir ${benchmarks_dir_name}


# Treat parameter sets
echoerr "Preparing parameter sets for benchmark cases... (it may take some time)"
declare -a cases=()
IFS=$'\n'
cases=( `${script_dir}/smart-parameters.bash \"${input}\" | shuf` )
unset IFS
echoerr "Parameter sets ready!"

last_case_i="$((${#cases[*]} - 1))"



# Launch a benchmark for each case
for cases_i in `seq 0 ${last_case_i}`; do

  #echo "case $cases_i : ${cases[$cases_i]}"
  case="${cases[$cases_i]}"

  worker () {

    trap "{ yield_token; echo 'Worker exitting...' >&2 ; exit 0; }" EXIT SIGINT SIGTERM

    preamble="`echo "$case" | cut -d ';' -f 1 | sed "s/,/\n/g"`"
    #echo "preamble = $preamble"
    exec_stats="$preamble"

    case_command="`echo "$case" | cut -d ';' -f 2`"
    case_command=${case_command#\"} # strip leading  double quote
    case_command=${case_command%\"} # strip trailing double quote
    #echo "case_command = $case_command"


#  DEPRECATED
#
#    # FIRST RUN (without generating the final configuration)
#
#    tmp_time_file_1=`mktemp zephyrus_benchmark_time_XXXXX`
#    time_cmd_1=`make_time_command ${tmp_time_file_1} ${time_format_description}`
#
#    echo -e "> Benchmark (${cases_i}/${last_case_i}):\n${exec_stats}\n"
#    echo -e "> Running (without generating the final configuration)... (started on `date +'%F %H:%M:%S'`)"
#    #cmd="${time_cmd_1} timeout ${timeout_in_seconds}s ${case_command} -stop-after-solving"
#    cmd="${time_cmd_1} ${case_command} -stop-after-solving"
#    #echo "$cmd"
#    $cmd > /dev/null 2> /dev/null
#
#    echo -e "> Benchmark (${cases_i}/${last_case_i}):\n${exec_stats}\n"
#    echo -e "< Done first run!"


    # SECOND RUN (with generating the final configuration)

    tmp_time_file=`mktemp zephyrus_benchmark_time_XXXXX`
    time_cmd=`make_time_command ${tmp_time_file} ${time_format_description}`
    tmp_statistics_file=`mktemp zephyrus_benchmark_stats_XXXXX`

    echo -e "> Running (with generating the final configuration)... (started on `formatted_date`)"
    #cmd="${time_cmd} timeout ${timeout_in_seconds}s ${case_command} -out statistics ${tmp_statistics_file}"
    zephyrus_cmd="${case_command} -out statistics ${tmp_statistics_file}"
    cmd="${time_cmd} ${zephyrus_cmd}"
    cmd_printable=`echo $(printf '%q' "$cmd") | sed -e 's/\\\\ / /g'`
    echo "COMMAND EXECUTED: ${cmd_printable}"
    $cmd > /dev/null 2> /dev/null
    echo -e "> Benchmark (${cases_i}/${last_case_i}):\nCOMMAND EXECUTED: ${cmd_printable}\n${exec_stats}\n"
    echo -e "< All done!"

    # Collect statistics from temporary files
    if test "`head -n 1 ${tmp_time_file}`" = "Command exited with non-zero status 124"; then 
      time_stats="TIMEOUT"
      zephyrus_stats=""
    else 
        time_stats="`cat ${tmp_time_file}`"
        zephyrus_stats="`cat ${tmp_statistics_file}`"
    fi

    # Delete the temporary files
    #rm ${tmp_time_file_1}
    rm ${tmp_time_file}
    rm ${tmp_statistics_file}


    echo -e "\nStats:\n${exec_stats}\n${zephyrus_stats}\n${time_stats}\n"

    # Print statistics to a benchmark results file
    benchmark_results_file=`mktemp --tmpdir=${benchmarks_dir_name} benchmark_XXXXXX`
    echo "Benchmark results file : ${benchmark_results_file}"
    echo -en "${exec_stats}\n${zephyrus_stats}\n${time_stats}" > ${benchmark_results_file}

    exit 0
  }

  take_token
  worker &

done

wait

echo "Benchmark script done!"
