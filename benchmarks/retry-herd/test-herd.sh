#!/bin/bash

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q=

n=72

for exe in no-invariants coarse htm-bloom hle-bloom fine-hle; do
    main=./Herd-$exe

    for t in `seq 1 72` ; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -t $t -e $n \
                +RTS -N$t --stm-stats -lsu $q -s
    done
done

