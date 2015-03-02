#!/bin/bash

set -e

# Phases
#p=1
p=2

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads

for exe in no-invariants fine-hle coarse htm-bloom hle-bloom; do
# for exe in fine-hle htm-bloom hle-bloom; do
# for exe in htm-bloom; do
  main=./.cabal-sandbox-$exe/bin/vacation
  for t in `seq 1 72` ; do

    retry=
    if [ "$exe" == "htm-bloom" -o "$exe" == "hle-bloom" ]; then
      # retry="--hle-retry=25"
      retry=
    fi
    if [ "$exe" == "fine-hle" ]; then
      # retry="--hle-retry=25 --htm-retry=25"
      # retry="--hle-retry=25 --htm-retry=0"
      retry=
    fi
# low
    # perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -lu -H -K512m -A2m $retry
    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -lu -H -K32m $retry
# low+
#    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 2 -q 90 -u 98 -r 1048576 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -lsu -H -K512m -A2m

# high
#    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 4 -q 60 -u 90 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -lsu -H -K512m -A2m

    ./countUserTransactions vacation.eventlog
  done
done

