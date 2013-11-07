#!/bin/bash

for n in `seq 10 10 10000` ; do
    perf stat -e tx-start,tx-commit,tx-abort -- ./Main-inline-edit -e $n -t 1 -i -g
done

