#!/bin/bash

pre=vac-align
for i in `seq 1 5` ; do
#    sleep 2
    ./test-low.sh $pre-${i}.log
    ./plot.sh $pre-${i}.log $pre-${i}.pdf
done
