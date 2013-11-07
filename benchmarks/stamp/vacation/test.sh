#!/bin/sh

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 1 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 1 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 1 -r 500 -t 500

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 2 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 2 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 2 -r 500 -t 500

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 4 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 4 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 4 -r 500 -t 500

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 8 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 8 -r 500 -t 500
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 8 -r 500 -t 500

