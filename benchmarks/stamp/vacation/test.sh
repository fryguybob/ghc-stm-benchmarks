#!/bin/bash

b=$1

for i in `seq 1 5` ; do
#    sleep 2
    ./test-low.sh $b-${i}.log
    ./plot.sh logs/$b-${i}.log $b-${i}.pdf
done

./plotMulti.sh max $b-max.pdf logs/$b-*.log
./plotMulti.sh ave $b-ave.pdf logs/$b-*.log
./plotMulti.sh mean $b-mean.pdf logs/$b-*.log
./plotMulti.sh min $b-min.pdf logs/$b-*.log

