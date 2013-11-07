#!/bin/bash

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 1 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 1 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 1 -n 2 -q 90 -u 98 -r 16384 -t 4096

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 2 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 2 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 2 -n 2 -q 90 -u 98 -r 16384 -t 4096

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 4 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 4 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 4 -n 2 -q 90 -u 98 -r 16384 -t 4096

perf stat -e tx-start,tx-commit,tx-abort -- ./Main-init    -c 8 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main         -c 8 -n 2 -q 90 -u 98 -r 16384 -t 4096
perf stat -e tx-start,tx-commit,tx-abort -- ./Main-nocheck -c 8 -n 2 -q 90 -u 98 -r 16384 -t 4096

