#!/bin/bash

set -e

# sparse writes
n=100000
# n=1000
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

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

# HASKELL 2017 
# for exe in stmtrie-fine-8 stmtrie-tstruct-fine-8; do # HAMT
# for exe in no-invariants-8 tstruct-fine-8; do # RBTree
# for exe in cuckoo-tvar-fine-8 cuckoo-tstruct-fine-8 cuckoo-tstruct-int-fine-8; do # Cuckoo
for exe in skiplist-8 skiplist-tstruct-fine-8; do # Skiplist

# ICFP 2017
# for exe in stmtrie-fine-8 stmtrie-fine stmtrie-tstruct-fine-8 stmtrie-tstruct-fine; do # HAMT
# for exe in no-invariants-8 no-invariants tstruct-fine-8 tstruct-fine; do # RBTree
# for exe in cuckoo-tvar-fine-8 cuckoo-tvar-fine cuckoo-tstruct-fine-8 cuckoo-tstruct-fine cuckoo-tstruct-int-fine-8 cuckoo-tstruct-int-fine; do # Cuckoo

# IFL 2016
# This is TStruct without HTM for IFL
# for exe in stmtrie-fine stmtrie-tstruct stmtrie-tstruct-fine; do # HAMT
# for exe in no-invariants tstruct-fine htm-mut; do # RBTree 
# for exe in no-invariants cuckoo-tvar-fine cuckoo-tstruct-fine cuckoo-tstruct-int-fine; do # Cuckoo
# for exe in cuckoo-tvar-fine cuckoo-tvar-fine-simple cuckoo-tstruct-int-fine; do # Cuckoo
# for exe in skiplist-tstruct-fine skiplist; do # SkipList

    main=./bin/Main-$exe
    for t in `seq 1 72` ; do
#    for ti in `seq 1 36` ; do
#        t=`ghc -e "$ti*2"`

        retry="--htm-retry=0 --hle-retry=0"
        bloom=

        if [ $exe == "cuckoo-tvar-fine" ] || [ $exe == "no-invariants" ] || [ $exe == "stmtrie-fine" ]; then
          retry=
        fi

#        cmd="$main -e 100000 -t $t -m 30 -s $s $i +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry" # large high contention
        cmd="$main -e 100000 -t $t -m $m -s $s $i $nw +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A1m $retry $bloom --stm-accum=0" # large GHC-8 ICFP 2017
#        cmd="$main -e 100000 -t $t -m $m -s $s $i $nw +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry $bloom" # large IFL 2016
#        cmd="$main -e 10000  -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry" # small
#        cmd="$main -e 100000 -t $t -m $m -s $s $i +RTS -N$t -ki4k -kc64k -kb4k -A8m $retry" # TL2 Special
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -ki8m -kc128m -kb8m -A8m -V0 $retry"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -H -K512m -ki4k -kc64k -kb4k -A8m -V0"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -H -K512m -A2m -V0"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -K128m -A8m -V0"
        echo $cmd
        perf stat -e tx-start,tx-capacity,tx-conflict -- $cmd &>> $1
    done
done


