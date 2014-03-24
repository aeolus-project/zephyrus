#!/bin/bash

token_name="test"
token_manager="./token-manager.bash"

tokens="4"

if ! $token_manager $token_name is-running
then
  $token_manager $token_name start $tokens
fi

take_token () { $token_manager $token_name take; }

yield_token () { $token_manager $token_name yield; }

count="$1"
echo "Master $count running."

for x in `seq $1`; do

  y="[$x]"

  take_token
  echo "Taken cpu token!"

  worker () {
    trap "{ yield_token; echo 'Worker $y exitting...'; exit 0; }" EXIT SIGINT SIGTERM
    echo "Worker $y running!"
    echo "$y running..."
    sleep $x
    echo "$y finished!"
    exit 0
  }

  echo "Launching $x"

  worker &

done

wait
