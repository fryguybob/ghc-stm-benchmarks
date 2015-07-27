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

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

for exe in HashMap-no-invariants no-invariants coarse htm-bloom hle-bloom fine-hle htm-mut; do
# for exe in htm-bloom; do
# for exe in IORef-no-invariants HashMap-no-invariants; do
    main=./Main-$exe
    for t in `seq 1 72` ; do
#    for ti in `seq 1 36` ; do
#        t=`ghc -e "$ti*2"`
        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -H -K512m -A2m"
        echo $cmd
        perf stat -e tx-start,tx-capacity,tx-conflict -- $cmd &>> $1
    done
done


