#!/bin/bash

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

exe=$1

t=$2

main=./Main-$exe

echo $main
c="$main -e $n -t $t -s $s $i +RTS $q -N$t"
echo "$c"

for j in `seq 1 10` ; do
    timeout 5s $c 
    stat=$?
    if [ $stat -eq 1 -o $stat -eq 0 ]; then
        exit 0
    fi
    echo exit was $stat trying again.
done
