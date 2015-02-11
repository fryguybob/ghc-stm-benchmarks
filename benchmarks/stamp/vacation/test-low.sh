#!/bin/bash

set -e

# Phases
#p=1
p=2

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads

for exe in no-invariants coarse htm-bloom hle-bloom; do
  main=./.cabal-sandbox-$exe/bin/vacation
  for t in `seq 1 72` ; do
# low
    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 2 -q 90 -u 98 -r 16384 -t 4096 -p $p +RTS --stm-stats $q -N$t -lsu
# low+
#    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 2 -q 90 -u 98 -r 1048576 -t 4096 -p $p +RTS --stm-stats $q -N$t -lsu

# high
#    perf stat -e tx-start,tx-capacity,tx-conflict -- $main -c $t -n 4 -q 60 -u 90 -r 16384 -t 4096 -p $p +RTS --stm-stats $q -N$t -lsu
  done
done

