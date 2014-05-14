#!/bin/bash

#main=./Main
#main=./Main-late
r=20000
g=40

main=./Main-7.6.3

for t in 4 ; do
    for n in 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -m 90
    done
done

main=./Main-early
for t in 4 ; do
    for n in 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -m 90
    done
done

main=./Main-iw
for t in 4 ; do
    for n in 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -m 90
    done
done

