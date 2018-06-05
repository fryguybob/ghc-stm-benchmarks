#!/bin/bash

for n in `cat variants`; do
    echo Running Treap$n
    perf stat -- ./bin/Treap$n +RTS -s >& .run-logs/Treap$n.log
done

grep "time elapsed" .run-logs/*.log | column -t
