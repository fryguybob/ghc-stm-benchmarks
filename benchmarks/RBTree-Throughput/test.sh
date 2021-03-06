#!/bin/bash

b=$1

for i in `seq 1 5` ; do
#    sleep 2

###  Throughput vs thread count
    lx=Threads
    fx=threads
    ly=Throughput
    fy=transactions
    ./test-mix-thread.sh logs/$b-${i}.log
    ./plot.sh logs/$b-${i}.log $b-${i}.pdf $lx $fx $ly $fy

###  Throughput vs lookup mix
#    lx=Readonly-percent
#    fx=cmd-m
#    ly=Throughput
#    fy=transactions
#    ./test-fixthread-varmix.sh logs/$b-${i}.log
#    ./plot.sh logs/$b-${i}.log $b-${i}.pdf $lx $fx $ly $fy
done

./plotMulti.sh $lx $fx $ly $fy ave $b-ave.pdf logs/$b-*.log
./plotMulti.sh $lx $fx $ly $fy median $b-median.pdf logs/$b-*.log
./plotMulti.sh $lx $fx $ly $fy min $b-min.pdf logs/$b-*.log
./plotMulti.sh $lx $fx $ly $fy max $b-max.pdf logs/$b-*.log

