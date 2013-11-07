#!/bin/bash

for t in 1 2 4 8 ; do
    for n in 10 20 30 40 50 ; do
        perf stat -e tx-start,tx-commit,tx-abort -- ./Main -e $n -t $t -i
        perf stat -e tx-start,tx-commit,tx-abort -- ./Main -e $n -t $t
    done
done

