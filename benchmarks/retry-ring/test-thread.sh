#!/bin/bash

# set -e

# sparse writes
n=1000

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

for exe in no-invariants coarse htm-bloom hle-bloom fine-hle; do
#    main=./Main-$exe
    main=./MainB-$exe
    for tn in `seq 1 36` ; do
#    for tn in `seq 1 18` ; do
        t=`ghc -e "$tn*2"`
        c="$main -e $n -t $t -s $s $i +RTS $q -N$t -lu"
        # ./Main-htm-bloom -e 1000 -t 20 -s 1000 +RTS -qatopo-cores-sockets-threads -N20

        for j in `seq 1 100` ; do
            timeout 3s $c 
            stat=$?
            if [ $stat -ne 124 ]; then
              break
            fi
        done
    done
    echo 
done


