#!/bin/bash

set -e

# Phases
#p=1
p=2

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads

rm -f $1 &> /dev/null

for exe in no-invariants fine-hle coarse htm-bloom hle-bloom; do
  main=./.cabal-sandbox-$exe/bin/vacation-$exe
  for t in `seq 1 72` ; do

#    retry=
#    if [ "$exe" == "htm-bloom" -o "$exe" == "hle-bloom" ]; then
#      # retry="--hle-retry=25"
#      retry=
#    fi
#    if [ "$exe" == "fine-hle" ]; then
#      # retry="--hle-retry=25 --htm-retry=25"
#      # retry="--hle-retry=25 --htm-retry=0"
#      retry=
#    fi
# low
    cmd="$main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -H -K32m $retry"
    echo $cmd
    perf stat -e tx-start,tx-capacity,tx-conflict -- $cmd &>> $1
  
  done
done

