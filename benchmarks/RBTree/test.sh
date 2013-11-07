#!/bin/bash

#for t in 1 2 4 8 ; do
t=1
    for n in 10 20 30 40 50 60 70 80 90 100 ; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- ./Main -e $n -t $t -i
        perf stat -e tx-start,tx-capacity,tx-conflict -- ./Main -e $n -t $t
    done
#done

