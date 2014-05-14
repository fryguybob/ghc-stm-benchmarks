#!/bin/bash

#main=./Main
#main=./Main-early
main=./Main-7.6.3
#main=./Main-late
r=20000
g=20

for t in 1 2 4 8 ; do
# t=1
#    for n in 10 20 30 40 50 60 70 80 90 100 ; do
    for n in 256 ; do
#        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l -i
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
    done
done

