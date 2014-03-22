#!/bin/bash

# Prepare the processus sharing pipe
distribution_pipe="/tmp/cpus_distribution_pipe"
reclaim_pipe="/tmp/cpus_reclaim_pipe"

cpus="$1"

if [[ "$1" = "-reset" ]]; then
  echo "Deleting the existing pipes..."
  rm -f $distribution_pipe $reclaim_pipe && echo "Pipes deleted." || echo "Error: Pipes not deleted!"
  echo "Killing children and terminating..."
  killall `basename $0`
  exit 0
fi

if [[ "$1" = "-distribution-pipe" ]]; then
  echo "$distribution_pipe"
  exit 0
fi

if [[ "$1" = "-reclaim-pipe" ]]; then
  echo "$reclaim_pipe"
  exit 0
fi

if [[ "$1" = "-exit" ]]; then
  echo "exit" > reclaim_pipe &
  exit 0
fi



if test -e $distribution_pipe; then
  echo "Distribution pipe exists!"
else
  echo "Distribution pipe does not exist."

  echo "Creating distribution pipe..."
  if mkfifo $distribution_pipe; then
  	echo "Distribution pipe created successfuly at: $distribution_pipe !"
  else
    echo "Distribution pipe creation at $distribution_pipe failed!"
    exit 1
  fi

fi

if test -e $reclaim_pipe; then
  echo "Reclaim pipe exists!"
else
  echo "Reclaim pipe does not exist."

  echo "Creating reclaim pipe..."
  if mkfifo $reclaim_pipe; then
    echo "Reclaim pipe created successfuly at: $reclaim_pipe !"
  else
    echo "Reclaim pipe creation at $reclaim_pipe failed!"
    exit 1
  fi

fi

manage () {
  exec 4<>$reclaim_pipe
  exec 5<>$distribution_pipe
  while true
  do
    if read -u 3 cpu_token
    then
      echo "Token manager: reclaimed token $cpu_token." >&2
      if [[ "$cpu_token" = "exit" ]]
      then
        echo "Token manager exitting..."  >&2
        break
      else
        echo "(Re)distributing token $cpu_token..."  >&2
        (while true; do echo "$cpu_token" > $distribution_pipe && break || echo "WTF"; done; echo "Token $cpu_token (re)distributed" >&2) &
      fi
    fi
  done 3< $reclaim_pipe #4> $reclaim_pipe 5>$distribution_pipe
}

manage &


echo "Filling the distribution pipe..."
for cpu_number in `seq ${cpus}`; do
  cpu_token="cpu_${cpu_number}"
  echo "Adding cpu token '${cpu_token}' to the distribution pipe..."
  (while true; do echo "$cpu_token" > $distribution_pipe && break || echo "WTF"; done; echo "Token $cpu_token distributed") &
  echo "Added cpu token '${cpu_token}' to the distribution pipe."
done
echo "Distribution pipe filled!"


echo "Cpu token manager running!"
echo "$distribution_pipe"
echo "$reclaim_pipe"

exit 0
