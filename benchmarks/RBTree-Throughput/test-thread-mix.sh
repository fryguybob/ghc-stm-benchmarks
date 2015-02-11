#!/bin/bash

set -e

# heavy-write
# r=10
# g=4
# n=1000
# m=10

# light-write
# r=10
# g=10
# n=2000
# m=90

# small-tree
# r=1000
# g=4
# n=100
# m=90

# sparse writes
r=1000
g=1
n=1000
t=36

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

for exe in IORef-no-invariants; do
# for exe in no-invariants coarse htm-bloom hle-bloom; do
    main=./Main-$exe
    for p in `seq 50 90` ; do
        #m=`ghc -e "$p/100"`
        m=$p
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s $i +RTS --stm-stats -lsu $q -N$t

        ./countUserTransactions ${main}.eventlog
    done
done


