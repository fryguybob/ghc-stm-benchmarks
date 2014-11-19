#!/bin/bash

main=./Herd-htm-bloom

# perf stat -e tx-start,tx-conflict,tx-capacity -- ./Main-7.6.3 -e 1024 -t 2 -r 20000 -m 90

# for t in 1 2 4 8 ; do
for t in 4 ; do
    perf stat -e tx-start,tx-capacity,tx-conflict -- $main
done

