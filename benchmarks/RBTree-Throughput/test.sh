#!/bin/bash

for i in `seq 1 5` ; do
#    sleep 2
    ./test-mix-thread.sh &> results-${i}.log
    ./plot.sh results-${i}.log results-${i}.pdf Threads t
done
