#!/bin/sh

perf record -e tx-abort -- ./Main-nocheck -c 1 -r 500 -t 500
