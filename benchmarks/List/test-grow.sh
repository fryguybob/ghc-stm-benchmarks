#!/bin/bash

for n in {1..1000} ; do
    perf stat -e tx-start,tx-commit,tx-abort -- ./Main -e $n -t 1 -i -g
done

