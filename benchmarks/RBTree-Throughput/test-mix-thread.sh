#!/bin/bash

set -e

# sparse writes
n=100000
# n=1000
m=90

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

for exe in HashMap-no-invariants no-invariants coarse htm-bloom hle-bloom fine-hle; do
# for exe in htm-bloom hle-bloom fine-hle; do
# for exe in IORef-no-invariants HashMap-no-invariants; do
    main=./Main-$exe
#    for t in `seq 1 72` ; do
    for ti in `seq 1 36` ; do
        t=`ghc -e "$ti*2"`
        # perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s $i +RTS --stm-stats -lu $q -N$t -H -K512m -A2m
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s $i +RTS --stm-stats -lu $q -N$t -H -K512m -A2m

        ./countUserTransactions ${main}.eventlog
    done
done


