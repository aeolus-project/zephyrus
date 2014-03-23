#!/bin/bash

token_name="test"
token_manager="./token-manager.bash"

take_token () { $token_manager $token_name take; }

yield_token () { $token_manager $token_name yield; }

count="$1"
echo "Master $count running."

for x in `seq $1`; do

	y="[$x]"

	take_token
	echo "Taken cpu token!"

	worker () {
	  echo "Worker $y running!"
	  local cpu_token="$1"
	  echo "$y running..."
	  sleep $x
	  echo "$y finished!"
	  yield_token
	}

	echo "Launching $x"

	worker $cpu_token &

done

wait
