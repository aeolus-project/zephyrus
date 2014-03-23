#!/bin/bash

token_name="$1"
cmd="$2"

# Tokens
token="T"
exit_token="X"

# Prepare pipe names
distribution_pipe="/tmp/${token_name}_token_distribution_pipe"
reclaim_pipe="/tmp/${token_name}_token_reclaim_pipe"

# Delete the pipes, kill all the working token managers and their children.
if [[ "$cmd" = "killall" ]]
then
  echo "Deleting the existing pipes..." >&2
  rm -f $distribution_pipe $reclaim_pipe && echo "Pipes deleted." >&2 || echo "Error: Pipes not deleted!" >&2
  echo "Killing children and terminating..." >&2
  killall `basename $0`
  exit 0
fi

# Print the distribution pipe name.
if [[ "$cmd" = "distribution-pipe" ]]
then
  echo "$distribution_pipe"
  exit 0
fi

# Print the reclaim pipe name.
if [[ "$cmd" = "reclaim-pipe" ]]
then
  echo "$reclaim_pipe"
  exit 0
fi

# Stop the running token manager.
if [[ "$cmd" = "stop" ]]
then
  echo "$exit_token" > reclaim_pipe &
  exit 0
fi

# Run a new token manager.
if [[ "$cmd" = "start" ]]
then

  tokens_available="$3"

  if test -e $distribution_pipe
  then
    echo "Distribution pipe exists!" >&2
  else
    echo "Distribution pipe does not exist." >&2

    echo "Creating distribution pipe..." >&2
    if mkfifo $distribution_pipe
    then
    	echo "Distribution pipe created successfuly at: $distribution_pipe !" >&2
    else
      echo "Distribution pipe creation at $distribution_pipe failed!" >&2
      exit 1
    fi
  fi

  if test -e $reclaim_pipe
  then
    echo "Reclaim pipe exists!" >&2
  else
    echo "Reclaim pipe does not exist." >&2

    echo "Creating reclaim pipe..." >&2
    if mkfifo $reclaim_pipe
    then
      echo "Reclaim pipe created successfuly at: $reclaim_pipe !" >&2
    else
      echo "Reclaim pipe creation at $reclaim_pipe failed!" >&2
      exit 1
    fi
  fi

  manage () {
    exec 4<>$reclaim_pipe
    exec 5<>$distribution_pipe
    while true
    do
      if read -n 1 -u 3 reclaimed_token
      then
        echo "Token manager: reclaimed token $reclaimed_token." >&2
        if [[ "$reclaimed_token" = "$exit_token" ]]
        then
          echo "Token manager exitting..."  >&2
          break
        elif [[ "$reclaimed_token" = "$token" ]]
        then
          echo "(Re)distributing token $reclaimed_token..."  >&2
          (
            while true
            do 
              echo -e -n $reclaimed_token > $distribution_pipe && break || echo "(Re)distributing token $reclaimed_token failed!" >&2
            done
            echo "Token $reclaimed_token (re)distributed" >&2
          ) &
        fi
      fi
    done 3< $reclaim_pipe #4> $reclaim_pipe 5>$distribution_pipe
  }

  manage &


  echo "Filling the distribution pipe..." >&2
  for token_number in `seq ${tokens_available}`; do
    #token="${token_name}_${token_number}"
    distributed_token="$token"
    echo "Adding token '${distributed_token}' to the distribution pipe..." >&2
    (
      while true
      do 
        echo -e -n $distributed_token > $distribution_pipe && break || echo "Distributing token $distributed_token failed!" >&2
      done
      echo "Token $distributed_token distributed!" >&2
    ) &
    echo "Added token '${distributed_token}' to the distribution pipe." >&2
  done
  echo "Distribution pipe filled!"


  echo "Token manager for ${token_name} tokens running!" >&2
  echo "Token distribution pipe: $distribution_pipe" >&2
  echo "Token reclaim pipe: $reclaim_pipe" >&2

  exit 0
fi

if [[ "$cmd" = "take" ]]
then
  echo "Taking a ${token_name} token..." >&2
  read -n 1 taken_token < $distribution_pipe
  #echo "Got a ${token_name} token '${taken_token}'!" >&2
  echo "Got a ${token_name} token!" >&2
  #echo "$taken_token"
  exit 0
fi

if [[ "$cmd" = "yield" ]]
then
  #yielded_token="$3"
  yielded_token="$token"
  #echo "Yielding a ${token_name} token '${yielded_token}'..." >&2
  echo "Yielding a ${token_name} token..." >&2
  echo -e -n $yielded_token > $reclaim_pipe &
  #echo "Yielded a ${token_name} token '${yielded_token}'!" >&2
  echo "Yielded a ${token_name} token!" >&2
  exit 0
fi

echo "Wrong command ${cmd}!"  >&2
exit 1