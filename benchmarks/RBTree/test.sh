#!/bin/bash

#main=./Main
#main=./Main-early
#main=./Main-late
main=./Main-sort
r=20

for t in 1 2 4 8 ; do
# t=1
    for n in 10 20 30 40 50 60 70 80 90 100 ; do
        perf stat -e tx-start,tx-commit,tx-abort -- $main -e $n -t $t -r $r -i
        perf stat -e tx-start,tx-commit,tx-abort -- $main -e $n -t $t -r $r
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -i
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
    done
done

