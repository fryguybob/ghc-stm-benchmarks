#!/bin/bash

set -e

# Phases
#p=1
p=2

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads

echo "Benchmark running with log to logs/$1"

rm -f logs/$1 &> /dev/null

for exe in no-invariants fine-hle coarse htm-bloom hle-bloom htm-mut; do
  main=./.cabal-sandbox-$exe/bin/vacation-$exe
  for t in `seq 1 72` ; do

    retry=
    if [ "$exe" == "htm-bloom" ] || [ "$exe" == "htm-mut" ] || [ "$exe" == "htm-mut-align" ]; then
      # retry="--hle-retry=25"
      count=`ghc -e "max $t 10"`
      retry="--htm-retry=$count"
    fi
    if [ "$exe" == "hle-bloom" ]; then
      count=`ghc -e "max $t 10"`
      retry="--hle-retry=$count"
    fi
    if [ "$exe" == "htm-mut-fine" ]; then
      retry="--htm-retry=0"
    fi
    if [ "$exe" == "fine-hle" ]; then
      # retry="--hle-retry=25 --htm-retry=25"
      # retry="--hle-retry=25 --htm-retry=0"
      retry="--htm-retry=10"
    fi

# low
#    cmd="$main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -H -K32m $retry"
    cmd="$main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry"
    echo $cmd
    perf stat -e tx-start,tx-capacity,tx-conflict -- $cmd &>> logs/$1
  
  done
done

