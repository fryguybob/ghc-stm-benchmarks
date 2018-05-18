#!/bin/bash

set -e

m=90
#m=95
#m=100
#m=50

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

# noish write (all inserts share key, all deletes share key)
nw=
#nw=-n

s=1000
#t=1  # for IORef and mutsingle
#t=18  # for STM
t=$1

b=$2

# accumulate sum for average allocation per transaction stats
accum="--stm-accum=0"
#accum="--stm-accum=3" # ignore read-only transactions
#accum="--stm-accum=6" # ignore writing transactions

echo "Benchmark running with log to $b"

rm -f $b &> /dev/null

# for exe in treapioref-TVar-fine treapmutsingle-TVar-fine rbtreemutsingle-TVar-fine rbtreeioref-TVar-fine; do
for exe in treapmutstm-TVar-fine treaptvar-TVar-fine rbtreemutstm-TVar-fine rbtree-TStruct-fine rbtree-TVar-fine; do
    main=./bin/Main-$exe-8
    for e in `seq 2 6` ; do
        count=`ghc -e "10^$e"`
        retry="--htm-retry=0 --hle-retry=0"
        bloom=
        cmd="$main -e $count -t $t -m $m -s $s $i $nw +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A1m $retry $bloom $accum" 
        echo $cmd
        perf stat -e tx-start,tx-capacity,tx-conflict -- $cmd &>> $b
    done
done


