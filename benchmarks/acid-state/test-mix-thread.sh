#!/bin/bash

set -e

# Settings
n=1000
m=90

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

for exe in no-invariants coarse htm-bloom hle-bloom fine-hle; do
# for exe in htm-bloom; do
    main=./.cabal-sandbox-$exe/bin/acid-state-bench
    for t in `seq 1 72` ; do    
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s $i +RTS --stm-stats -lu $q -N$t -H -K512m -A2m
        rm -rf state 
        ./countUserTransactions acid-state-bench.eventlog
    done
done


