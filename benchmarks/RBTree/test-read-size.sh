#!/bin/bash

set -e

r=20000
g=40

for exe in 7.10 no-invariants coarse htm-bloom; do
    main=./Main-$exe
    for t in 72; do
#    for t in 1 2 4 8 16 18 32 36 64 72 ; do
        for n in 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250; do
#        for n in 50 100 150 200 250; do
#        for n in 10 50 100 150 200; do
            perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -l
        done
    done
done


