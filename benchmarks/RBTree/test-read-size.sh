#!/bin/bash

#main=./Main
#main=./Main-late
r=20000
g=40

main=./Main-7.6.3
for t in 4 ; do
    for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
    done
done

main=./Main-early
for t in 4 ; do
    for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
    done
done

main=./Main-coarse
for t in 4 ; do
    for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
    done
done

main=./Main-rtm-coarse
for t in 4 ; do
    for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
    done
done


