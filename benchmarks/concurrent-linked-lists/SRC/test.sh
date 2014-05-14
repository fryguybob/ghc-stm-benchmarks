#!/bin/bash

l=1000
t=4
benchmarks="CAS MLC IO STM CASusingSTM MLCusingSTM LAZY"

rm log.txt

for n in `seq 1 10` ; do
    for main in ./Main-7.6 ./Main ; do
        for bench in $benchmarks ; do
            perf stat -e tx-start,tx-capacity,tx-conflict -- $main $bench $t $l +RTS -N$t >>log.txt 2>&1
        done
    done
done

#main=./Main-early
#for t in 4 ; do
#    for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200; do
#        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
#    done
#done


