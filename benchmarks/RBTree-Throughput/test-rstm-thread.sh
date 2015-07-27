#!/bin/bash

set -e

# sparse writes
n=100000
m=90

# topo
#q=atopo-cores-threads-sockets
q=topo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

# for stm in NOrec CGL RingSW OrecLazy Swiss; do
for stm in NOrec TML OrecLazy Swiss; do
    main=/localdisk/ryates/rstm_build/bench/TreeBenchSSB64
    export STM_CONFIG=$stm
    for t in `seq 1 72` ; do
        $main -m $n -p $t -R $m -q $q
    done
done


