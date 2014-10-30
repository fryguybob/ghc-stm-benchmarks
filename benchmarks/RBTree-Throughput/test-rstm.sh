#!/bin/bash

main=./Main
#main=./Main-early
#main=./Main-late

# perf stat -e tx-start,tx-conflict,tx-capacity -- ./Main-7.6.3 -e 1024 -t 2 -r 20000 -m 90
s=1000
m=90

# for t in 1 2 4 8 ; do
for t in 4 ; do
    for n in 16 256 1024 ; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -s $s -i -m $m
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -s $s    -m $m
    done
done

