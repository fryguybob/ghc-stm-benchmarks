#!/bin/bash

n=$1

# for s in 1 ; do
for s in 1 2 4 8 ; do
# for s in 1 2 4 8 18 ; do
    b=$n-$s
    for i in `seq 1 5` ; do
    #    sleep 2
    
    ###  Throughput vs size
        lx=Entries
        fx=entries
        ly=Throughput
        fy=transactions
        ./test-size.sh $s logs/$b-${i}.log
        ./plot.sh  $lx $fx $ly $fy $b-${i}.pdf --log-x logs/$b-${i}.log
    done
    
    ./plotMulti.sh $lx $fx $ly $fy ave    $b-ave.pdf    --log-x logs/$b-*.log
    ./plotMulti.sh $lx $fx $ly $fy median $b-median.pdf --log-x logs/$b-*.log
    ./plotMulti.sh $lx $fx $ly $fy min    $b-min.pdf    --log-x logs/$b-*.log
    ./plotMulti.sh $lx $fx $ly $fy max    $b-max.pdf    --log-x logs/$b-*.log

    # ./plotMulti-heap.sh $lx $fx Bytes both ave $b-bytes-ave.pdf logs/$b-*.log
done

