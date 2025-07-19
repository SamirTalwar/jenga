#!/bin/bash

# blast parallel jenga builds, looking for odd behaviour
JOBS=3

#scope=examples/01-two-files
scope=examples/02-discover-deps
#scope=src

rm -rf /tmp/.cache/jenga
install-jenga

expect=$(jenga build -fp $scope -j1 | grep ran | cut -d' ' -f3 | paste -sd+ | bc)
echo "expect:" $expect

max=0 # 0 means forever

i=0
while true; do
  i=$((i+1))
  echo -n "$i:"
  got=$(jenga build -fp $scope -j$JOBS | grep ran | cut -d' ' -f3 | paste -sd+ | bc)
  echo -n $got
  if [ $got != $expect ]; then echo ' **WRONG**'; else echo;  fi
  if [ $i = $max ]; then
      #find /tmp/.cache/jenga/
      #diff -r /tmp/.cache/jenga/*/traces
      exit
  fi
done

