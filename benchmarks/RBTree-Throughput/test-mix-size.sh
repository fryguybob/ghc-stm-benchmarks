#!/bin/bash

set -e

# sparse writes
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

for exe in HashMap-no-invariants no-invariants coarse htm-bloom hle-bloom fine-hle; do
#for exe in no-invariants coarse htm-bloom hle-bloom; do
    main=./Main-$exe
    t=36
    for i in `seq 1 8` ; do
        n=`ghc -e "(2^$i)*100"`
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s +RTS --stm-stats -lu $q -N$t -H -K512m -A2m

        ./countUserTransactions ${main}.eventlog
    done
done


