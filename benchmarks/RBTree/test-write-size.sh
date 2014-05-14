#!/bin/bash

#main=./Main
#main=./Main-late
r=10
g=2

#ns="10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200"
ns="50 100 150 200 250 300"

main=./Main-coarse
for t in 4 ; do
    for n in $ns; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
    done
done

#main=./Main-rtm-coarse
#for t in 4 ; do
#    for n in $ns; do
#        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
#    done
#done

#main=./Main-sort
#for t in 4 ; do
#    for n in $ns; do
#        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
#    done
#done

#main=./Main-7.6.3
#for t in 4 ; do
#    for n in 50 100 150 200 250; do
#        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
#    done
#done

main=./Main-iw
for t in 4 ; do
    for n in $ns; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
    done
done

main=./Main-unpack
for t in 4 ; do
    for n in $ns; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r
    done
done

