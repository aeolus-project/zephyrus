#!/bin/bash

# Prepare the processus sharing pipe
distribution_pipe="/tmp/cpus_distribution_pipe"
reclaim_pipe="/tmp/cpus_reclaim_pipe"

take_token () { read cpu_token < $distribution_pipe; echo "$cpu_token"; }

yield_token () { echo "$1" > $reclaim_pipe & }

count="$1"
echo "Master $count running."

for x in `seq $1`; do

	y="[$x]"

	cpu_token=`take_token`
	echo "Taken cpu token $cpu_token !"

	worker () {
	  echo "Worker $y running!"
	  local cpu_token="$1"
	  echo "$y running on token $cpu_token...";
	  sleep $x;
	  echo "$y finished on token $cpu_token.";
	  yield_token $cpu_token
	}

	echo "Launching $x"

	worker $cpu_token &

done

wait
