#!/bin/bash

for n in {1..100} ; do
    perf stat -e tx-start,tx-commit,tx-abort -- ./Main-inline-edit -e $n -t 1 -i -g
done

